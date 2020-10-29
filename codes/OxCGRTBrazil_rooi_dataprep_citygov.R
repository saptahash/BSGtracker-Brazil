renv::load()
library(tidyverse)
library(countrycode)
library(zoo)
library(RcppRoll)
library(feather)
library(lubridate)
# set path
path <- "C:/Users/sapta/Downloads/random/oxford cgrt"

list.files(path)
# import all the necessary data
testingdata <- read.csv(paste0(path, "/INFLUD20-09102020/INFLUD20-09102020.csv"), sep = ";")

# set URL path for google mobility
url_gmobility <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

# GLOBAL - import google mobility data -------------------------------------------
google.mobility <- read_csv(url(url_gmobility), col_types = cols(sub_region_1 = col_character(), 
                                                                 sub_region_2 = col_character(), 
                                                                 metro_area = col_character()))

## GLOBAL - clean up google mobility data -----------------------------------------
crc <- google.mobility %>% pull(country_region_code)
google.mobility$newcountry_code <- NA 
google.mobility$newcountry_code <- unlist(countrycode(crc, origin = "iso2c", destination = "iso3c"))
google.mobility <- google.mobility %>%
  subset(select = -country_region_code) %>%
  rename(country_region_code = newcountry_code)

google.mobility <- google.mobility %>% rename(goog_retail = starts_with("retail"),
                                              goog_groceryandpharmacy = starts_with("groceryandpharmacy"),
                                              goog_parks = starts_with("parks"), 
                                              goog_transitstations = starts_with("transit"), 
                                              goog_workplaces = starts_with("workplaces"), 
                                              goog_residential = starts_with("residential"))

# subset by countrycode BRA and replacing state codes with OxCGRT matching codes
#google.mobility_state <-
#  google.mobility %>%
#  filter(country_region_code == "BRA" & !is.na(iso_3166_2_code)) %>%
#  mutate(iso_3166_2_code = str_replace(iso_3166_2_code, "-", "_")) %>%
#  select(starts_with("goog"), iso_3166_2_code, date)

## set-up city google mobility data ---------------------------------------------
google.mobility_city <- 
  google.mobility %>%
  filter(!is.na(sub_region_2))

googlekeyfile <- read.csv("./googlemob_citystate+matched.csv")

googlekeyfile <- 
  googlekeyfile %>%
  filter(matched != "")

google.mobility_city <- left_join(google.mobility_city, googlekeyfile, by = c("sub_region_1", "sub_region_2"))

google.mobility_city <- 
  google.mobility_city %>%
  filter(!is.na(matched)) %>%
  mutate(matched = as.character(matched))

# ROOI for CITY_ALL --------------------------------------------------------

# STEPS - 
# 1. Collect Data 
# 2. set-up data accordingly 
# 3. merge 
# 4. create sub-indices
# 5. create endemic factor
# 6. 

## Creating google mobility - cityname key file-------------------------------- 

#citystate <- 
#  google.mobility %>%
#  select(sub_region_1, sub_region_2)

#citystate <- unique(citystate)

#write.csv(citystate, "./googlemob_citystate.csv")

## city ROOI

## importing datasets ----------------------------------------------
oxcgrtdata <- read.csv(paste0(path, "/OxCGRT_Download_271020_145615_BRA.csv"))
citycasedata <- read_csv(url("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"))

# RUN FROM HERE IF ALL BASIC FILES ARE LOADED

## setup CITY_ALL base file ---------------------------------------------------
city_all <-
  oxcgrtdata %>%
  filter(Jurisdiction == "CITY_GOV") %>%
  mutate(ibgeID = str_extract(CityCode, pattern = "[0-9]*$")) %>%
  select(-contains(c("Notes", "Country")), -Jurisdiction, H3_Contact.tracing, H2_Testing.policy,
         contains("Index"), C8_International.travel.controls, C7_Restrictions.on.internal.movement,
         C7_Flag, H1_Public.information.campaigns, ConfirmedCases, ConfirmedDeaths)

## merging with the aux files 
city_all <- left_join(city_all %>% mutate(Date = ymd(Date)), 
                      google.mobility_city, 
                      by = c("CityName" = "matched", "Date" = "date"))

# TESTING DATA FOR CITIES 

## important variables: 
# 1. city IBGE code - co_mun_not
# 2. nu_notific - person ID
# 3. sg_uf_not - state code 
# 4. pcr_resul - PCR result 
# 5. dt_coleta - testing date
# 6. amostra - tests? 1 - yes 2 - no 9 - NA


# selecting important variables from testing data
testingdata_tibble <- 
  testingdata %>%
  select(co_mun_not, nu_notific, id_municip, sg_uf_not, dt_coleta, amostra)

pcrtesting_tibble <-
  testingdata %>%
  select(co_mun_not, nu_notific, sg_uf_not, pcr_resul, dt_pcr)

## consistency checks - does same person get tested multiple times? 
# - repeated data enties, filter out using unique
# - even after that, some IDs have two test results against them
# some with blank dates, pick non-blank dates, or dates with amostra == 1 against them
# - do multiple IDs have amostra == 1 on multiple dates? yes - omit? (possible to take latest date, omitting instead)

testingdata_tibble <- unique(testingdata_tibble)

testingdata_tibble <- 
  testingdata_tibble %>%
  filter(amostra == 1) %>%
  group_by(nu_notific) %>%
  arrange(nu_notific, dt_coleta) %>%
  mutate(n = n()) %>%
  filter(n == 1)

## aggregate by summing amostra 

### find list of cities first 

IBGE_list <- str_extract(unique(city_all$CityCode), pattern = "[0-9]*$")
IBGE_list <- str_sub(IBGE_list,end = -2)

testingdata_tibble <- 
  testingdata_tibble %>%
  ungroup() %>%
  filter(co_mun_not %in% as.numeric(IBGE_list)) %>%
  group_by(co_mun_not, id_municip, dt_coleta) %>%
  summarise(tests = sum(amostra))

## to merge to city_all dataframe, need to create a new column without last digit of IBGE

city_all <- 
  city_all %>%
  mutate(ibgeID = str_sub(ibgeID, end = -2))

testingdata_tibble <- 
  testingdata_tibble %>%
  mutate(dt_coleta = str_replace_all(dt_coleta, pattern = "/", replacement = "-"),
         dt_coleta = mdy(dt_coleta))

city_all <- left_join(city_all %>% mutate(ibgeID = as.numeric(ibgeID)), 
                      testingdata_tibble, 
                      by = c("ibgeID" = "co_mun_not", 
                             "Date" = "dt_coleta"))

## city cases data 
#citycasedata <- read_csv(url("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv"))

IBGE_list <- str_extract(unique(city_all$CityCode), pattern = "[0-9]*$")

citycasedata <- 
  citycasedata %>%
  filter(ibgeID %in% IBGE_list) %>%
  select(ibgeID, newDeaths, deaths, newCases, totalCases, deaths_per_100k_inhabitants, totalCases_per_100k_inhabitants, date)

city_all <- 
  city_all %>%
  mutate(ibgeID = as.numeric(str_extract(CityCode, pattern = "[0-9]*$")))

## prepare for merge 
city_all <- 
  left_join(city_all, 
            citycasedata, 
            by = c("Date" = "date", 
                   "ibgeID"))

## fill in gaps in testing and google data
city_all <- 
  city_all %>% 
  group_by(CityName) %>%
  arrange(CityName, Date) %>%
  fill(contains(c("cases", "deaths")), tests, contains("goog"), .direction = "down", H1_Public.information.campaigns, H2_Testing.policy, H3_Contact.tracing, C7_Restrictions.on.internal.movement, C8_International.travel.controls)

## note that newcases < 0 at times, not possible, so carryforward cases and then compute newcases
city_all <- 
  city_all %>%
  group_by(CityName) %>%
  arrange(CityName, Date) %>%
  mutate(pop_100k = totalCases/totalCases_per_100k_inhabitants,
         newCases = ifelse(newCases < 0, NA, newCases)) %>%
  #totalCases = ifelse(newCases < 0, lag(totalCases), totalCases),
  #totalCases_per_100k_inhabitants = ifelse(newCases < 0, lag(totalCases_per_100k_inhabitants), totalCases_per_100k_inhabitants)) %>%
  fill(newCases, .direction = "down") %>%
  mutate(moveave_newcases = rollmean(newCases, k = 7, fill = NA))

# create ROOI subindices for CITY_ALL----------------------------------------------

## cases controlled ---------------------------------------------------------------- 
city_all <-
  city_all %>%
  group_by(CityCode) %>%
  arrange(CityCode, Date) %>%
  mutate(cases_controlled = ifelse((moveave_newcases)/50 < 1, (moveave_newcases)/50, 1)) # Check limits

## setting up for TTI sub-index -------------------------------------------------------
maxtests <- 
  city_all %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(test_percase = tests/totalCases) %>%
  arrange(Date, test_percase) %>%
  filter(!is.na(test_percase)) %>%
  slice_tail(1) %>% 
  select(test_percase, Date) %>%
  rename(max_test_percase = test_percase)

mintests <- 
  city_all %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(test_percase = tests/totalCases) %>%
  arrange(Date, test_percase) %>%
  filter(!is.na(test_percase)) %>%
  slice_head(1) %>% 
  select(test_percase, Date) %>%
  rename(min_test_percase = test_percase)

meantests <- 
  city_all %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(test_percase = tests/totalCases) %>%
  summarise(mean_test_percase = mean(test_percase, na.rm = T)) %>%
  mutate(mean_test_percase = ifelse(is.finite(mean_test_percase), mean_test_percase, NA))

# adding max cumulative tests and cumulative tests to main data
city_all <- left_join(city_all, maxtests, by = c("Date"))
city_all <- left_join(city_all, mintests, by = c("Date"))
city_all <- left_join(city_all, meantests, by = c("Date"))

# constructing TTI sub-index
city_all <-
  city_all %>%
  mutate(test_percase = ifelse(!is.na(totalCases), tests/totalCases, NA),
         test_parameter = ifelse(is.na(totalCases), 
                                 1,  # if no cases yet, then the risk from low testing is 0, hence 1 on parameter
                                 ifelse(!is.na(tests), #what do we do during missing tests data
                                        #handle this edge case
                                        (log(test_percase) - log(min_test_percase))/(log(max_test_percase) - log(min_test_percase)),
                                        0)),
         test_and_trace = 1 - ((0.25*H2_Testing.policy)/3 + 0.25*H3_Contact.tracing/2 + 0.5*test_parameter))

#constructing behavioural change sub-index------------------------------------------
googlevars <- str_subset(names(city_all), pattern = "goog")

for(i in googlevars){
  avevar <- paste0("ave_", i)
  city_all <- 
    city_all %>% 
    arrange(CityCode, Date) %>% 
    group_by(CityCode) %>% 
    mutate(!! avevar := rollmean(!! sym(i), k=7, fill = NA))
}

googlevars <- str_subset(names(city_all), pattern = "ave_goog")

# ISSUE - why not take all googlevars
city_all <- 
  city_all %>% 
  ungroup() %>%
  mutate(google_ave = rowMeans(city_all[,c("ave_goog_retail", "ave_goog_transitstations", "ave_goog_workplaces")], na.rm = T), 
         google_ave = ifelse(is.finite(google_ave), google_ave, NA)) %>%
  arrange(CityCode, Date) %>%
  group_by(CityCode) %>%
  mutate(min_google = roll_min(google_ave, n = 28L, align = "right",fill = NA, na.rm = T),
         min_google = ifelse(is.finite(min_google), min_google, NA),
         min_google = min_google + 100, 
         mob = case_when(min_google < 20 ~ 20,
                         min_google > 20 & min_google < 120 ~ min_google,
                         min_google > 120 & (!is.na(min_google)) ~ 120))

# getting community understanding sub-index -----------------------------------------
city_all <- 
  city_all %>% 
  mutate(community_understanding = (0.5*cases_controlled) + (1 - 0.5*cases_controlled)*(mob-20)/100,
         community_understanding = ifelse(H1_Public.information.campaigns != 2, 0, community_understanding)) 

# constructing risk of importing and exporting cases -----------------------------------
city_all <- 
  city_all %>%
  mutate(c7risk = case_when(C7_Restrictions.on.internal.movement == 2 & C7_Flag == 1 ~ 2, 
                            C7_Restrictions.on.internal.movement == 2 & C7_Flag == 0 ~ 1,
                            C7_Restrictions.on.internal.movement == 1 & C7_Flag == 1 ~ 1, 
                            C7_Restrictions.on.internal.movement == 1 & C7_Flag == 0 ~ 0,
                            C7_Restrictions.on.internal.movement == 0 ~ 0),
         c8risk = case_when(C8_International.travel.controls == 0 ~ 1, 
                            C8_International.travel.controls == 1 ~ 0.5, 
                            C8_International.travel.controls == 2 ~ 0.25, 
                            C8_International.travel.controls > 2 ~ 0),
         manage_imported_cases = 0.5*c7risk + 0.5*c8risk)


# endemic factor ------------------------------------------------
city_all <- 
  city_all %>%
  group_by(CityCode) %>%
  fill(pop_100k, .direction = "updown") %>%
  mutate(newcases_permillion = moveave_newcases/(pop_100k/10),
         endemic_factor = case_when(newcases_permillion < 50 ~ 0, 
                                    newcases_permillion > 200 ~ 1, 
                                    newcases_permillion < 200 & !is.na(newcases_permillion) ~ (newcases_permillion - 50)/150))

# risk of openness index
city_gov <- 
  city_all %>%
  ungroup() %>%
  mutate(rooi_unadjusted = rowMeans(city_all[,c("community_understanding", "test_and_trace",
                                                "manage_imported_cases", "cases_controlled")], na.rm = T),
         risk_of_openness = ifelse(!is.na(endemic_factor), 
                                   endemic_factor + (1 - endemic_factor)*rooi_unadjusted, 
                                   rooi_unadjusted), 
         risk_of_openness = ifelse(is.na(totalCases) | totalCases == 0, NA, risk_of_openness))

## ISSUE NOTES: 
# 1. total cases needs to be defined correctly for erroneous cases 
# 2. NA handling needs to improve -- carry forward all indicators

write.csv(city_gov, "./brazil_citygov_risk_of_openness.csv")
write_feather(city_gov, "./brazil_citygov_risk_of_openness.feather")
