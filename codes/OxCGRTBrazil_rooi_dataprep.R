renv::load()
library(tidyverse)
library(countrycode)
library(zoo)
library(RcppRoll)
library(feather)
library(lubridate)
renv::install(packages = c("feather"))
renv::snapshot()
# set path
path <- "C:/Users/sapta/Downloads/random/oxford cgrt"

# import all the necessary data
testingdata <- read.csv(paste0(path, "/INFLUD20-09102020/INFLUD20-09102020.csv"), sep = ";")
oxcgrtdata <- read.csv(paste0(path, "/OxCGRT_Download_131120_115044_BRAImputed.csv"))

#oxcgrtdata <- read.csv(paste0(path, "/OxCGRT_Download_191020_091820_BRAImputed.csv"))

casedata <- read_csv(url("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv"), col_types = cols(tests = col_double(),
                                                                                                                               tests_per_100k_inhabitants = col_double()))
#citycasedata <- read_csv(url("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv"))
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
google.mobility_state <-
  google.mobility %>%
  filter(country_region_code == "BRA" & !is.na(iso_3166_2_code)) %>%
  mutate(iso_3166_2_code = str_replace(iso_3166_2_code, "-", "_")) %>%
  select(starts_with("goog"), iso_3166_2_code, date)

# work around cases data to make it merge-able 
casedata <- 
  casedata %>%
  filter(state != "TOTAL") %>%
  mutate(state = str_c("BR_", state, sep = "")) %>%
  select(date, state, newCases, newDeaths, totalCases, deaths, tests, tests_per_100k_inhabitants, 
         totalCases_per_100k_inhabitants) %>%
  mutate(newCases = ifelse(newCases < 0, NA, newCases)) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  fill(newCases, .direction = "down") %>%
  mutate(moveave_newcases = rollmean(newCases, k = 7, fill = NA))

# prepare for merge 
oxcgrtdata <-
  oxcgrtdata %>%
  mutate(Date = lubridate::ymd(Date)) %>%
  filter(Jurisdiction == "STATE_ALL") %>%
  select(-contains(c("Notes", "City", "Country")))

oxcgrtdata <- 
  left_join(oxcgrtdata, google.mobility_state, by = c("Date" = "date", "RegionCode" = "iso_3166_2_code"))

oxcgrtdata <- 
  left_join(oxcgrtdata, casedata, by = c("RegionCode" = "state", "Date" = "date"))

# create ROOI independent variables 

## cases controlled - generate rolling average of new cases and then check bounds 
oxcgrtdata <- 
  oxcgrtdata %>%
  group_by(RegionCode) %>%
  arrange(RegionCode, Date) %>%
  mutate(#moveave_newcases = rollmean(newCases, k = 7, fill = NA),
         cases_controlled = ifelse((moveave_newcases)/50 < 1, (moveave_newcases)/50, 1)) # Check limits

## test trace isolate - H3, H2, min(tests) and max(tests)
 
maxtests <- 
  oxcgrtdata %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(test_percase = tests/totalCases) %>%
  arrange(Date, test_percase) %>%
  filter(!is.na(test_percase)) %>%
  slice_tail(1) %>% 
  select(test_percase, Date) %>%
  rename(max_test_percase = test_percase)
  
mintests <- 
  oxcgrtdata %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(test_percase = tests/totalCases) %>%
  arrange(Date, test_percase) %>%
  filter(!is.na(test_percase)) %>%
  slice_head(1) %>% 
  select(test_percase, Date) %>%
  rename(min_test_percase = test_percase)


### ISSUE NOTE - should weigh this using population?
meantests <- 
  oxcgrtdata %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(test_percase = tests/totalCases) %>%
  summarise(mean_test_percase = mean(test_percase, na.rm = T)) %>%
  mutate(mean_test_percase = ifelse(is.finite(mean_test_percase), mean_test_percase, NA))

# adding max cumulative tests and cumulative tests to main data
oxcgrtdata <- left_join(oxcgrtdata, maxtests, by = c("Date"))
oxcgrtdata <- left_join(oxcgrtdata, mintests, by = c("Date"))
oxcgrtdata <- left_join(oxcgrtdata, meantests, by = c("Date"))

# constructing TTI sub-index
oxcgrtdata <-
  oxcgrtdata %>%
  mutate(test_percase = ifelse(!is.na(totalCases), tests/totalCases, NA),
         test_parameter = ifelse(is.na(totalCases), 
                                 1,  # if no cases yet, then the risk from low testing is 0, hence 1 on parameter
                                 ifelse(!is.na(tests), #what do we do during missing tests data
                                        #handle this edge case
                                        (log(test_percase) - log(min_test_percase))/(log(max_test_percase) - log(min_test_percase)),
                                        0)),
         test_and_trace = 1 - ((0.25*H2_Testing.policy)/3 + 0.25*H3_Contact.tracing/2 + 0.5*test_parameter))

# constructing behavioural change sub-index
googlevars <- str_subset(names(oxcgrtdata), pattern = "goog")

for(i in googlevars){
  avevar <- paste0("ave_", i)
  oxcgrtdata <- 
    oxcgrtdata %>% 
    arrange(RegionCode, Date) %>% 
    group_by(RegionCode) %>% 
    mutate(!! avevar := rollmean(!! sym(i), k=7, fill = NA))
}

googlevars <- str_subset(names(oxcgrtdata), pattern = "ave_goog")

# ISSUE - why not take all googlevars
oxcgrtdata <- 
  oxcgrtdata %>% 
  ungroup() %>%
  mutate(google_ave = rowMeans(oxcgrtdata[,c("ave_goog_retail", "ave_goog_transitstations", "ave_goog_workplaces")], na.rm = T), 
         google_ave = ifelse(is.finite(google_ave), google_ave, NA)) %>%
  arrange(RegionCode, Date) %>%
  group_by(RegionCode) %>%
  mutate(min_google = roll_min(google_ave, n = 28L, align = "right",fill = NA, na.rm = T),
         min_google = ifelse(is.finite(min_google), min_google, NA),
         min_google = min_google + 100, 
         mob = case_when(min_google < 20 ~ 20,
                         min_google > 20 & min_google < 120 ~ min_google,
                         min_google > 120 & (!is.na(min_google)) ~ 120))

# getting community understanding variable
oxcgrtdata <- 
  oxcgrtdata %>% 
  mutate(community_understanding = (0.5*cases_controlled) + (1 - 0.5*cases_controlled)*(mob-20)/100,
         community_understanding = ifelse(H1_Public.information.campaigns != 2, 1, community_understanding)) 

# risk of importing and exporting cases
oxcgrtdata <- 
  oxcgrtdata %>%
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

# endemic factor
oxcgrtdata <- 
  oxcgrtdata %>%
  mutate(pop = tests*100000/tests_per_100k_inhabitants) %>%
  group_by(RegionCode) %>%
  fill(pop, .direction = "updown") %>%
  mutate(newcases_permillion = moveave_newcases/(pop/1000000),
         endemic_factor = case_when(newcases_permillion < 50 ~ 0, 
                                    newcases_permillion > 200 ~ 1, 
                                    newcases_permillion < 200 & !is.na(newcases_permillion) ~ (newcases_permillion - 50)/150))

# risk of openness index
oxcgrtdata <- 
  oxcgrtdata %>%
  ungroup() %>%
  mutate(rooi_unadjusted = rowMeans(oxcgrtdata[,c("community_understanding", "test_and_trace",
                                                  "manage_imported_cases", "cases_controlled")], na.rm = T),
         risk_of_openness = ifelse(!is.na(endemic_factor), 
                                   endemic_factor + (1 - endemic_factor)*rooi_unadjusted, 
                                   rooi_unadjusted), 
         risk_of_openness = ifelse(is.na(totalCases) | totalCases == 0, NA, risk_of_openness))

write.csv(oxcgrtdata, "./brazil_stateall_risk_of_openness.csv")
write_feather(oxcgrtdata, "./brazil_stateall_risk_of_openness.feather")
