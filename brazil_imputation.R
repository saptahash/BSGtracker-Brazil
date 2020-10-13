library(tidyverse)
library(RCurl)
library(rlang)
library(here)

#brazil_nonimp <- getURL("http://oxcgrtportal.azurewebsites.net/api/csvdownload?type=subnational_brazil")

## set path correctly
brazildatapath <- "https://raw.githubusercontent.com/saptahash/BSGtracker-Brazil/master/OxCGRTBrazil_nonimputed_latest.csv"
brazildata <- read.csv(brazildatapath)

## Two structures needed -> Inheritance and AggregateUp

citydata <- 
  brazildata %>%
  filter(CityName != '') %>%
  select(-starts_with("Confirmed"))

indicators <- unique((str_extract(names(brazildata), pattern = "(C|H|E)[0-9]")))
indicators <- indicators[!is.na(indicators)]
indicators <- str_subset(indicators, pattern = "Notes", negate = T)
flags <- str_subset(indicators, pattern = "Flag")
indicators <- str_subset(indicators, pattern = "Flag", negate = T)

## these operations apply to only certain indicators
indicators <- indicators[!indicators %in% c("E3", "E4", "H4", "H5", "E1", "E2", "E4")]

## get city base of aggregate up 
agg_tibble <- NULL
temp_tibble <- NULL

#### STEP 1 - Aggregate Up to get STATE_ALL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  if(length(flag) > 0){
    temp_tibble <- 
      citydata %>% 
      select(RegionName, RegionCode, CityName, CityCode, Date, !!sym(level), !!sym(flag)) %>%
      group_by(RegionName, Date) %>%
      filter(!is.na(!!sym(level))) %>%
      arrange(RegionName, Date, !!sym(level), CityCode) %>%
      slice_tail(1) 
    temp_tibble <- 
      temp_tibble %>% 
      mutate(!!flag := ifelse(!is.na(!!sym(flag)),ifelse(CityCode == "STATE_GOV", !!sym(flag), 0), NA)) %>%
      select(-starts_with("City"))
  } else{
    temp_tibble <- 
      citydata %>% 
      select(RegionName, RegionCode, CityName, CityCode, Date, !!sym(level)) %>%
      group_by(RegionName, Date) %>%
      filter(!is.na(!!sym(level))) %>%
      arrange(RegionName, Date, !!sym(level), CityCode) %>%
      slice_tail(1) %>%
      select(-starts_with("City"))
  }
  if(length(agg_tibble) > 0){
    agg_tibble <- left_join(agg_tibble, temp_tibble, by = c("RegionName" = "RegionName",
                                                            "RegionCode" = "RegionCode",
                                                            "Date" = "Date"))
  } else{
    agg_tibble <- temp_tibble
  } 
}

## agg_tibble now represents STATE_ALL from first level

## Level 2 - Inheritance from NAT_GOV to STATE_ALL from previous level 

nat_gov <- 
  brazildata %>%
  filter(Jurisdiction == "NAT_GOV") %>%
  arrange(Date) %>%
  group_by(Date) %>%
  slice_head(1)

for(i in indicators){
  cols <- str_subset(names(nat_gov), pattern = i)
  for(c in cols){
    newc <- paste0(c, "_natgov")
    names(nat_gov)[names(nat_gov) == c] <- newc
  }
}

i <- paste(indicators, collapse = "|")
i <- str_subset(names(nat_gov), pattern = i) 

nat_gov <- 
  nat_gov %>%
  select(Date, all_of(i), -contains("Notes"))

agg_tibble <-
  left_join(agg_tibble, nat_gov,by = c("Date" = "Date"))

state_all <- NULL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  natgov_level <- paste(level, "_natgov", sep = "")
  natgov_flag <- paste(flag, "_natgov", sep  = "")
  if(length(flag)>0){
    temp_tibble <- 
      agg_tibble %>%
      mutate(!!level := ifelse(is.na(!!sym(natgov_level)), !!sym(level),
                               ifelse(is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(natgov_flag) == 1), !!sym(natgov_level),
                                      ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) > !!sym(natgov_level)),!!sym(level),
                                             ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) < !!sym(natgov_level)) & (!!sym(natgov_flag) == 1), !!sym(natgov_level), !!sym(level))))),
             !!flag := case_when(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) == !!sym(natgov_level)) & (!!sym(natgov_flag) == 1) ~ 1,
                                 TRUE ~ as.numeric(!!sym(flag)))) %>%
      select(RegionName, RegionCode, Date, !!level, !!flag)
  } else {
    temp_tibble <- 
      agg_tibble %>%
      mutate(!!level := ifelse(is.na(!!sym(natgov_level)), !!sym(level),
                               ifelse(is.na(!!sym(level)) & !is.na(!!sym(natgov_level)), !!sym(natgov_level),
                                      ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) > !!sym(natgov_level)),!!sym(level),
                                             ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) < !!sym(natgov_level)), !!sym(natgov_level), !!sym(level)))))) %>%
      select(RegionName, RegionCode, Date, !!level)
  }
  if(length(state_all) > 0){
    state_all <- left_join(state_all, temp_tibble, by = c("RegionName" = "RegionName",
                                                          "RegionCode" = "RegionCode", 
                                                          "Date" = "Date"))
  } else {
    state_all <- temp_tibble
  }
}

## Level 3 - Inheritance from STATE_ALL to CITY_GOV

city_gov <- 
  brazildata %>%
  filter(CityName != "" & CityName != "State government")

temp_tibble <- state_all 

for(i in indicators){
  cols <- str_subset(names(temp_tibble), pattern = i)
  for(c in cols){
    newc <- paste0(c, "_stateall")
    names(temp_tibble)[names(temp_tibble) == c] <- newc
  }
}

city_gov <-
  left_join(city_gov, temp_tibble,by = c("RegionName" = "RegionName", "RegionCode" = "RegionCode", "Date" = "Date"))


cityall <- NULL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  stateall_level <- paste(level, "_stateall", sep = "")
  stateall_flag <- paste(flag, "_stateall", sep  = "")
  if(length(flag)>0){
    temp_tibble <- 
      city_gov %>%
      mutate(!!level := ifelse(is.na(!!sym(stateall_level)), !!sym(level),
                               ifelse(is.na(!!sym(level)) & !is.na(!!sym(stateall_level)) & (!!sym(stateall_flag) == 1), !!sym(stateall_level),
                                      ifelse(!is.na(!!sym(level)) & !is.na(!!sym(stateall_level)) & (!!sym(level) > !!sym(stateall_level)),!!sym(level),
                                             ifelse(!is.na(!!sym(level)) & !is.na(!!sym(stateall_level)) & (!!sym(level) < !!sym(stateall_level)) & (!!sym(stateall_flag) == 1), !!sym(stateall_level), !!sym(level))))),
             !!flag := case_when(!is.na(!!sym(level)) & !is.na(!!sym(stateall_level)) & (!!sym(level) == !!sym(stateall_level)) & (!!sym(stateall_flag) == 1) ~ 1,
                                 TRUE ~ as.numeric(!!sym(flag)))) %>%
      select(RegionName, RegionCode, CityName, CityCode, Date, !!level, !!flag)
  } else {
    temp_tibble <- 
      city_gov %>%
      mutate(!!level := ifelse(is.na(!!sym(stateall_level)), !!sym(level),
                               ifelse(is.na(!!sym(level)) & !is.na(!!sym(stateall_level)), !!sym(stateall_level),
                                      ifelse(!is.na(!!sym(level)) & !is.na(!!sym(stateall_level)) & (!!sym(level) > !!sym(stateall_level)),!!sym(level),
                                             ifelse(!is.na(!!sym(level)) & !is.na(!!sym(stateall_level)) & (!!sym(level) < !!sym(stateall_level)), !!sym(stateall_level), !!sym(level)))))) %>%
      select(RegionName, RegionCode, CityName, CityCode, Date, !!level)
  }
  if(length(cityall) > 0){
    cityall <- left_join(cityall, temp_tibble, by = c("RegionName" = "RegionName",
                                                          "RegionCode" = "RegionCode", 
                                                          "Date" = "Date",
                                                      "CityName" = "CityName",
                                                      "CityCode" = "CityCode"))
  } else {
    cityall <- temp_tibble
  }
}

nat_gov <- 
 brazildata %>%
 filter(Jurisdiction == "NAT_GOV") %>%
 arrange(Date) %>%
 select(-contains("Notes")) %>%
 group_by(Date) %>%
 slice_head(1) %>%
 select(ends_with("Name"), ends_with("Code"), Jurisdiction, Date, contains(indicators)) %>%
 mutate(RegionCode = "BRA_ALL")

cityall$Jurisdiction <- "CITY_ALL"
state_all$Jurisdiction <- "STATE_ALL"

imputed_all <- bind_rows(nat_gov, cityall, state_all) %>% fill(CountryCode, CountryName, .direction = "updown")

write.csv(imputed_all, "./oxcgrtbrazil_imputedlatest.csv")

## stata friendly version
imputed_all_stata <- janitor::clean_names(imputed_all)
stata_dateorigin <- as.Date("1960-01-01")

imputed_all_stata <- 
  imputed_all_stata %>%
  mutate(date = as.numeric(as.Date(date) - stata_dateorigin)) %>%
  rename(c7_restrictions_movement = c7_restrictions_on_internal_movement) 
#         h4_emergency_investment = h4_emergency_investment_in_healthcare,
#         stringency_legacy_for_disp = stringency_legacy_index_for_display,
#         gov_response_index_for_disp = government_response_index_for_display, 
#         containment_health_for_disp = containment_health_index_for_display, 
#         economic_support_for_disp = economic_support_index_for_display)

haven::write_dta(imputed_all_stata, path = "./oxcgrtbrazil_imputedlatest.dta", version = 12)

# 
# ## Aggregate up to NAT_ALL from STATE_ALL
# 
# nat_gov <- 
#   brazildata %>%
#   filter(RegionName == "Country-wide") %>%
#   arrange(Date)
# 
# i <- paste(indicators, collapse = "|")
# i <- str_subset(names(nat_gov), pattern = i) 
# 
# nat_gov <- 
#   nat_gov %>%
#   select(Date,RegionName, RegionCode, all_of(i), -contains("Notes"))
# 
# temp_tibble <- state_all 
# 
# nat_gov <-
#   bind_rows(nat_gov, temp_tibble)
# 
# natall <- NULL
# 
# for(i in indicators){
#   level <- str_subset(str_subset(names(nat_gov), pattern = i), pattern = "(Notes|Flag)", negate = T)
#   flag <- str_subset(str_subset(names(nat_gov), pattern = i), pattern = "Flag")
#   if(length(flag) > 0){
#     temp_tibble <- 
#       nat_gov %>% 
#       select(RegionName, RegionCode, Date, !!sym(level), !!sym(flag)) %>%
#       group_by(Date) %>%
#       arrange(Date, !!sym(level), RegionCode) %>%
#       slice_tail(1) 
#     temp_tibble <- 
#       temp_tibble %>% 
#       mutate(!!flag := ifelse(!is.na(!!sym(flag)),ifelse(CityCode == "STATE_GOV", !!sym(flag), 0), NA)) %>%
#       select(-starts_with("City"))
#   } else{
#     temp_tibble <- 
#       citydata %>% 
#       select(RegionName, RegionCode, CityName, CityCode, Date, !!sym(level)) %>%
#       group_by(RegionName, Date) %>%
#       arrange(RegionName, Date, !!sym(level), CityCode) %>%
#       slice_tail(1) %>%
#       select(-starts_with("City"))
#   }
#   if(length(agg_tibble) > 0){
#     agg_tibble <- left_join(agg_tibble, temp_tibble, by = c("RegionName" = "RegionName",
#                                                             "RegionCode" = "RegionCode",
#                                                             "Date" = "Date"))
#   } else{
#     agg_tibble <- temp_tibble
#   } 
# }

  