library(renv)
renv::load()
library(tidyverse)
library(RCurl)
library(rlang)
library(here)
renv::restore()
#init()
#brazil_nonimp <- getURL("http://oxcgrtportal.azurewebsites.net/api/csvdownload?type=subnational_brazil")

## set path correctly
brazildatapath <- "https://raw.githubusercontent.com/saptahash/BSGtracker-Brazil/main/OxCGRTBrazil_nonimputed_latest.csv"
brazildata <- read.csv(brazildatapath)

## define indicators
indicators <- unique((str_extract(names(brazildata), pattern = "(C|H|E)[0-9]")))
indicators <- indicators[!is.na(indicators)]
indicators <- str_subset(indicators, pattern = "Notes", negate = T)
flags <- str_subset(indicators, pattern = "Flag")
indicators <- str_subset(indicators, pattern = "Flag", negate = T)

## these operations apply to only certain indicators
indicators <- indicators[!indicators %in% c("E3", "E4", "H4", "H5", "E1", "E2", "E4")]

## Two structures needed -> Inheritance and AggregateUp

citydata <- 
  brazildata %>%
  filter(CityName != '') %>%
  select(-starts_with("Confirmed"))


## get city base of aggregate up 
agg_tibble <- NULL
temp_tibble <- NULL

#### STEP 1 - Aggregate Up to get STATE_ALL


city_gov_condensed <- NULL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  if(length(flag) > 0){
    maxlevel <- paste0("max", level)
    meanlevel <- paste0("mean", level)
    meanflag <- paste0("mean", flag)
    temp_tibble <- 
      citydata %>%
      group_by(Date, RegionName) %>%
      select(RegionName, RegionCode, CityCode, CityName, Date, !!sym(level), !!sym(flag)) %>%
      filter(!is.na(!!sym(level))) %>%
      summarise(!!maxlevel := max(!!sym(level)), 
                !!meanlevel := mean(!!sym(level), na.rm = T),
                !!meanflag := mean(!!sym(flag), na.rm = T)) %>%
      mutate(!!meanflag := ifelse(!!sym(meanlevel) == 0, NA, !!sym(meanflag)))
  } else {
    maxlevel <- paste0("max", level)
    meanlevel <- paste0("mean", level)
    temp_tibble <- 
      citydata %>%
      group_by(Date,RegionName) %>%
      select(RegionName, RegionCode, CityCode, CityName, Date, !!sym(level)) %>%
      filter(!is.na(!!sym(level))) %>%
      summarise(!!maxlevel := min(!!sym(level)), #max level is now min level
                !!meanlevel := mean(!!sym(level), na.rm = T))
  }
  if(length(city_gov_condensed) == 0){
    city_gov_condensed <- temp_tibble
  } else {
    city_gov_condensed <- left_join(city_gov_condensed, temp_tibble, by = c("Date", "RegionName"))
  }
}

i <- paste(indicators, collapse = "|")
i <- str_subset(names(brazildata), pattern = i) 

state_gov <- 
  brazildata %>%
  filter(Jurisdiction == "STATE_GOV") %>%
  arrange(Date) %>%
  select(-contains("Notes")) %>%
  select(Date, RegionName, RegionCode, contains(indicators))

temp_tibble <- left_join(state_gov, city_gov_condensed, by = c("Date", "RegionName"))

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  maxlevel <- paste0("max", level)
  meanlevel <- paste0("mean", level)
  meanflag <- paste0("mean", flag)
  if(length(flag > 0)){
    temp_tibble <- 
      temp_tibble %>%
      mutate(!!flag := ifelse(!is.na(!!sym(flag)),
                              ifelse(!!sym(level) < !!sym(maxlevel),
                                     ifelse(!!sym(meanflag) == 1 & (!!sym(meanlevel) == !!sym(maxlevel)),
                                            1,
                                            0),
                                     !!sym(flag)),
                              ifelse(is.na(!!sym(meanflag)), 
                                     NA, 
                                     ifelse((!!sym(meanflag) == 1) & (!!sym(meanlevel) == !!sym(maxlevel)), 1, 0))),
             !!level := ifelse(!is.na(!! sym(level)),
                               ifelse(!!sym(level) <= !!sym(maxlevel), !!sym(maxlevel), !!sym(level)),
                               !!sym(maxlevel)))
  } else {
    temp_tibble <- 
      temp_tibble %>%
      mutate(!!level := ifelse(!is.na(!! sym(level)),
                               ifelse(!!sym(level) <= !!sym(maxlevel), !!sym(maxlevel), !!sym(level)),
                               !!sym(maxlevel)))
  }
}

i <- paste(indicators, collapse = "|")
i <- str_subset(names(nat_gov), pattern = i) 

agg_tibble <- 
  temp_tibble %>%
  select(-contains(c("mean", "max")))
  

# 
# for(i in indicators){
#   level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
#   flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
#   if(length(flag) > 0){
#     temp_tibble <- 
#       citydata %>% 
#       select(RegionName, RegionCode, CityName, CityCode, Date, !!sym(level), !!sym(flag)) %>%
#       group_by(RegionName, Date) %>%
#       filter(!is.na(!!sym(level))) %>%
#       arrange(RegionName, Date, !!sym(level), CityCode) %>%
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
#       filter(!is.na(!!sym(level))) %>%
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

temp_tibble <- state_gov

for(i in indicators){
  cols <- str_subset(names(temp_tibble), pattern = i)
  for(c in cols){
    newc <- paste0(c, "_stategov")
    names(temp_tibble)[names(temp_tibble) == c] <- newc
  }
}

city_gov <-
  left_join(city_gov, temp_tibble,by = c("RegionName" = "RegionName", "RegionCode" = "RegionCode", "Date" = "Date"))


cityall <- NULL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  stateall_level <- paste(level, "_stategov", sep = "")
  stateall_flag <- paste(flag, "_stategov", sep  = "")
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


## Inheritance from NAT_GOV to CITY_GOV

nat_gov <- 
  brazildata %>%
  filter(Jurisdiction == "NAT_GOV") %>%
  arrange(Date) %>%
  select(-contains("Notes")) %>%
  group_by(Date) %>%
  slice_head(1) %>%
  select(ends_with("Name"), ends_with("Code"), Jurisdiction, Date, contains(indicators)) %>%
  mutate(RegionCode = "BRA_ALL")

temp_tibble <- 
  nat_gov %>%
  select(-ends_with("Name"), -ends_with("Code"), -Jurisdiction)

for(i in indicators){
  cols <- str_subset(names(temp_tibble), pattern = i)
  for(c in cols){
    newc <- paste0(c, "_natgov")
    names(temp_tibble)[names(temp_tibble) == c] <- newc
  }
}

cityall <-
  left_join(cityall, temp_tibble,by = c("Date" = "Date"))

cityall_final <- NULL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  natgov_level <- paste(level, "_natgov", sep = "")
  natgov_flag <- paste(flag, "_natgov", sep  = "")
  if(length(flag)>0){
    temp_tibble <- 
      cityall %>%
      mutate(!!level := ifelse(is.na(!!sym(natgov_level)), !!sym(level),
                               ifelse(is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(natgov_flag) == 1), !!sym(natgov_level),
                                      ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) > !!sym(natgov_level)),!!sym(level),
                                             ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) < !!sym(natgov_level)) & (!!sym(natgov_flag) == 1), !!sym(natgov_level), !!sym(level))))),
             !!flag := case_when(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) == !!sym(natgov_level)) & (!!sym(natgov_flag) == 1) ~ 1,
                                 TRUE ~ as.numeric(!!sym(flag)))) %>%
      select(RegionName, RegionCode, CityName, CityCode, Date, !!level, !!flag)
  } else {
    temp_tibble <- 
      cityall %>%
      mutate(!!level := ifelse(is.na(!!sym(natgov_level)), !!sym(level),
                               ifelse(is.na(!!sym(level)) & !is.na(!!sym(natgov_level)), !!sym(natgov_level),
                                      ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) > !!sym(natgov_level)),!!sym(level),
                                             ifelse(!is.na(!!sym(level)) & !is.na(!!sym(natgov_level)) & (!!sym(level) < !!sym(natgov_level)), !!sym(natgov_level), !!sym(level)))))) %>%
      select(RegionName, RegionCode, CityName, CityCode, Date, !!level)
  }
  if(length(cityall_final) > 0){
    cityall_final <- left_join(cityall_final, temp_tibble, by = c("RegionName" = "RegionName",
                                                      "RegionCode" = "RegionCode", 
                                                      "Date" = "Date",
                                                      "CityName" = "CityName",
                                                      "CityCode" = "CityCode"))
  } else {
    cityall_final <- temp_tibble
  }
}

cityall_final$Jurisdiction <- "CITY_ALL"
state_all$Jurisdiction <- "STATE_ALL"

#imputed_all <- bind_rows(nat_gov, cityall, state_all) %>% fill(CountryCode, CountryName, .direction = "updown")

vars <- names(cityall_final)

city_gov <- 
  brazildata %>%
  filter(Jurisdiction == "CITY_GOV") %>%
  select(all_of(vars))

vars <- names(state_all)

state_gov <- 
  brazildata %>%
  filter(Jurisdiction == "STATE_GOV") %>%
  select(all_of(vars))

#### get NAT_ALL from upward inheritance from STATE_GOV

state_all_condensed <- NULL

for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  if(length(flag) > 0){
    maxlevel <- paste0("max", level)
    meanlevel <- paste0("mean", level)
    meanflag <- paste0("mean", flag)
    temp_tibble <- 
      state_all %>%
      group_by(Date) %>%
      select(RegionName, RegionCode, Date, !!sym(level), !!sym(flag)) %>%
      filter(!is.na(!!sym(level))) %>%
      summarise(!!maxlevel := max(!!sym(level)), 
                !!meanlevel := mean(!!sym(level), na.rm = T),
                !!meanflag := mean(!!sym(flag), na.rm = T)) %>%
      mutate(!!meanflag := ifelse(!!sym(meanlevel) == 0, NA, !!sym(meanflag)))
  } else {
    maxlevel <- paste0("max", level)
    meanlevel <- paste0("mean", level)
    temp_tibble <- 
      state_all %>%
      group_by(Date) %>%
      select(RegionName, RegionCode, Date, !!sym(level)) %>%
      filter(!is.na(!!sym(level))) %>%
      summarise(!!maxlevel := min(!!sym(level)), 
                !!meanlevel := mean(!!sym(level), na.rm = T))
  }
  if(length(state_all_condensed) == 0){
    state_all_condensed <- temp_tibble
  } else {
    state_all_condensed <- left_join(state_all_condensed, temp_tibble, by = c("Date" = "Date"))
  }
}

i <- paste(indicators, collapse = "|")
i <- str_subset(names(brazildata), pattern = i) 

nat_gov <- 
  brazildata %>%
  filter(Jurisdiction == "NAT_GOV") %>%
  arrange(Date) %>%
  select(-contains("Notes")) %>%
  group_by(Date) %>%
  slice_head(1) %>%
  select(Date, contains(indicators))
  
temp_tibble <- left_join(nat_gov, state_all_condensed, by = c("Date" = "Date"))
  
for(i in indicators){
  level <- str_subset(str_subset(names(citydata), pattern = i), pattern = "(Notes|Flag)", negate = T)
  flag <- str_subset(str_subset(names(citydata), pattern = i), pattern = "Flag")
  maxlevel <- paste0("max", level)
  meanlevel <- paste0("mean", level)
  meanflag <- paste0("mean", flag)
  if(length(flag > 0)){
    temp_tibble <- 
      temp_tibble %>%
      mutate(!!flag := ifelse(!is.na(!!sym(flag)),
                              ifelse(!!sym(level) < !!sym(maxlevel),
                                     ifelse(!!sym(meanflag) == 1 & (!!sym(meanlevel) == !!sym(maxlevel)),
                                            1,
                                            0),
                                     !!sym(flag)),
                              ifelse(is.na(!!sym(meanflag)), 
                                     NA, 
                                     ifelse((!!sym(meanflag) == 1) & (!!sym(meanlevel) == !!sym(maxlevel)), 1, 0))),
             !!level := ifelse(!is.na(!! sym(level)),
                               ifelse(!!sym(level) <= !!sym(maxlevel), !!sym(maxlevel), !!sym(level)),
                               !!sym(maxlevel)))
  } else {
    temp_tibble <- 
      temp_tibble %>%
      mutate(!!level := ifelse(!is.na(!! sym(level)),
                               ifelse(!!sym(level) <= !!sym(maxlevel), !!sym(maxlevel), !!sym(level)),
                               !!sym(maxlevel)))
  }
}

i <- paste(indicators, collapse = "|")
i <- str_subset(names(nat_gov), pattern = i) 

## write the correct columns into NAT_ALL
nat_all <- 
  temp_tibble %>%
  select(Date, all_of(i)) %>%
  mutate(Jurisdiction = "NAT_ALL", 
         RegionName = "",
         RegionCode = "BRA_ALL",
         CityCode = "",
         CityName = "")

nat_gov <- 
  brazildata %>%
  filter(Jurisdiction == "NAT_GOV") %>%
  arrange(Date) %>%
  select(-contains("Notes")) %>%
  group_by(Date) %>%
  slice_head(1) %>%
  select(ends_with("Name"), ends_with("Code"), Jurisdiction, Date, contains(indicators)) %>%
  mutate(RegionCode = "BRA_ALL")

imputed_all <- bind_rows(nat_gov, nat_all, cityall_final, city_gov, state_all, state_gov) %>% fill(CountryCode, CountryName, .direction = "updown")
  
## code credit to Andrew Wood
imputed_all$C1Stringency <- 100*(imputed_all$`C1_School.closing`-0.5*(1-imputed_all$C1_Flag))/3
imputed_all$C2Stringency <- 100*(imputed_all$`C2_Workplace.closing`-0.5*(1-imputed_all$C2_Flag))/3
imputed_all$C3Stringency <- 100*(imputed_all$`C3_Cancel.public.events`-0.5*(1-imputed_all$C3_Flag))/2
imputed_all$C4Stringency <- 100*(imputed_all$`C4_Restrictions.on.gatherings`-0.5*(1-imputed_all$C4_Flag))/4
imputed_all$C5Stringency <- 100*(imputed_all$`C5_Close.public.transport`-0.5*(1-imputed_all$C5_Flag))/2
imputed_all$C6Stringency <- 100*(imputed_all$`C6_Stay.at.home.requirements`-0.5*(1-imputed_all$C6_Flag))/3
imputed_all$C7Stringency <- 100*(imputed_all$`C7_Restrictions.on.internal.movement`-0.5*(1-imputed_all$C7_Flag))/2
imputed_all$C8Stringency <- 100*(imputed_all$`C8_International.travel.controls`)/4

imputed_all$H1Stringency <- 100*(imputed_all$`H1_Public.information.campaigns`-0.5*(1-imputed_all$H1_Flag))/2
imputed_all$H2Stringency <- 100*(imputed_all$`H2_Testing.policy`)/3
imputed_all$H3Stringency <- 100*(imputed_all$`H3_Contact.tracing`)/2
imputed_all$H6Stringency <- 100*(imputed_all$H6_Facial.Coverings-0.5*(1-imputed_all$H6_Flag))/4

for(i in indicators){
  level <- str_subset(str_subset(names(imputed_all), pattern = i), pattern = "(Notes|Flag|Stringency)", negate = T)
  flag <- str_subset(str_subset(names(imputed_all), pattern = i), pattern = "Flag")
  stringency <- paste0(i, "Stringency")
  imputed_all <- 
    imputed_all %>%
    group_by(Jurisdiction, RegionName, CityName) %>%
    arrange(Jurisdiction, RegionName, CityName,Date) %>%
    fill(!!stringency, .direction = "down") %>%
    mutate(!!stringency := ifelse(is.na(!!sym(stringency)) | !!sym(level) == 0 | is.na(!!sym(level)), 0, !!sym(stringency)))
}

imputed_all$StringencyIndex <- rowMeans(imputed_all[,c("C1Stringency", "C2Stringency", "C3Stringency",
                                                       "C4Stringency", "C5Stringency", "C6Stringency",
                                                       "C7Stringency", "C8Stringency", "H1Stringency")], na.rm = T)

imputed_all$ContainmentHealthIndex <- rowMeans(imputed_all[,c("C1Stringency", "C2Stringency", "C3Stringency",
                                                              "C4Stringency", "C5Stringency", "C6Stringency",
                                                              "C7Stringency", "C8Stringency", "H1Stringency", "H2Stringency",
                                                              "H3Stringency")], na.rm = T)

imputed_all$ContainmentHealthIndex_new <- rowMeans(imputed_all[,c("C1Stringency", "C2Stringency", "C3Stringency",
                                                                  "C4Stringency", "C5Stringency", "C6Stringency",
                                                                  "C7Stringency", "C8Stringency", "H1Stringency", "H2Stringency",
                                                                  "H3Stringency", "H6Stringency")], na.rm = T)

imputed_all <- imputed_all %>% select(-ends_with("Stringency"))  

write.csv(imputed_all, "./oxcgrtbrazil_imputedlatest.csv")

## stata friendly version
imputed_all_stata <- janitor::clean_names(imputed_all)
stata_dateorigin <- as.Date("1960-01-01")

imputed_all_stata <- 
  imputed_all_stata %>%
  mutate(date = lubridate::ymd(date),
         date = as.numeric(as.Date(date) - stata_dateorigin)) %>%
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

