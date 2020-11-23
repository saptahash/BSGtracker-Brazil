library(ggplot2)
library(feather)
library(lubridate)
library(ggrepel)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggthemes)
renv::install("datapasta")
renv::snapshot()

# Graphs to produce: 
# 1. Scatter Plot
# 2. Line Plots 
# 3. Chloropleths

## Import Data -----------------------------------------
oxcgrtdata <- read_feather("./brazil_stateall_risk_of_openness.feather")
city_all <- read_feather("./brazil_cityall_risk_of_openness.feather") 
regionmap <- read.csv("brazil_citystateregions_geo_pb.csv")

## merge state IBGE codes -------------------------
state_codes <- 
  city_all %>%
  select(RegionName, RegionCode, CityCode) %>%
  mutate(StateCode = str_extract(CityCode, pattern = "_[0-9][0-9]"),
         StateCode = str_extract(CityCode, pattern = "[0-9][0-9]")) %>%
  select(RegionCode, StateCode, RegionName)

state_codes <- unique(state_codes)

oxcgrtdata <- 
  left_join(oxcgrtdata, state_codes, by = c("RegionName", "RegionCode"))

## STATE_TOTAL graphs -----------------------------------

## STATE_TOTAL subset data ===============================

temp_tibble <- 
  oxcgrtdata %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date < "2020-10-10") %>%
  select(RegionName, RegionCode, StateCode, Date, pop, risk_of_openness, moveave_newcases, cases_controlled, test_and_trace, community_understanding, manage_imported_cases, endemic_factor, StringencyIndexForDisplay) %>%
  filter(RegionName != "") %>%
  mutate(RegionCode = str_extract(RegionCode, pattern = "[A-Z][A-Z]$"))

key_states <- c("SP", "RJ", "RS", "CE", "BA", "PA", "AM", "GO", "PE")

## STATE_TOTAL scatter plots ========================================
month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", 
                  "September", "October", "November", "December")

month_labels <- c("January", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", 
                  "Setembro", "Outubro", "November", "December")

region_levels <- c("Mid West", "North", "Northeast", "South", "Southeast")
region_labels <- c("Centro-Oeste", "Norte", "Nordeste", "Sul", "Sudeste")

base_tibble <- 
  temp_tibble %>%
  mutate(month = month(Date)) %>%
  group_by(month, RegionName, RegionCode, StateCode) %>%
  summarise(mean_rooi = mean(risk_of_openness, na.rm = T),
            mean_stringency = mean(StringencyIndexForDisplay, na.rm = T), 
            mean_rooi = ifelse(is.finite(mean_rooi), mean_rooi, NA),
            mean_newcases = mean(moveave_newcases, na.rm = T),
            mean_newcases = ifelse(is.finite(mean_newcases), mean_newcases, NA),
            pop = mean(pop, na.rm = T)) %>%
  mutate(month = month.name[month],
         month = factor(month, levels = month_levels, labels = month_labels)) %>%
  filter(month != "January" & month != "Outubro")

base_tibble <- left_join(base_tibble, unique(regionmap %>% select(GeoRegion, RegionName)), by = c("RegionName"))

base_tibble <- 
  base_tibble %>%
  ungroup() %>%
  mutate(GeoRegion = ifelse(is.na(GeoRegion), "Mid West", as.character(GeoRegion)),
         StateCode = ifelse(RegionName == "Distrito Federal", 53, StateCode),
         GeoRegion = factor(GeoRegion, levels = region_levels, labels = region_labels))

plot <-
  ggplot(base_tibble, aes(x = mean_rooi, y = mean_stringency, label = RegionCode)) +# color = factor(lightup_state), label = CountryCode)) + 
  geom_point(aes(size = pop, colour = GeoRegion)) + 
  #lims(colour = c("0", "1")) + 
  scale_color_viridis_d(name = "Região") +
  expand_limits(y = c(0,100), x = c(0,1)) +
  geom_text_repel(data = subset(base_tibble, RegionCode %in% key_states), 
                  size = 3, colour = "black") +
  #geom_hline(yintercept = 50, size = 0.7, linetype = 2) + 
  #geom_vline(xintercept = 0.5, size = 0.7, linetype = 2) +
  labs(x = "Risco de Abertura", 
       y = "Índice de Rigidez", 
       #         title = "Stringency Index and Openness Risk over last quarter",
       #         subtitle = "(Bubble Size reflects number of new cases)", 
       caption = "O tamanho da bolha representa a população \n Fonte: OxCGRT Brazilian Sub-National Covid-19 Policy Responses. Disponível em: https://github.com/OxCGRT/Brazil-covid-policy") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10)) +
  guides(size = F) + 
  #scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2), 0.5)) +
  #scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week\n (7 days prior to date)")) +
  scale_size(range = c(2,7)) +
  theme(
    # Remove panel border
    #panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
    #panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~ month)

ggsave(plot = plot,
       filename = "../graphs/brazilrooi_stateall_scatterplotdetail_monthly.png", 
       width = 12, 
       height = 8)


## STATE_TOTAL lineplots ==========================================
for(i in unique(temp_tibble$RegionName)){
  filename <- paste0("../graphs/policy-briefs/brazilrooi_state_", i, "_lineplot.png")
  base_tibble <- 
    temp_tibble %>%
    mutate(dum = ifelse((RegionName == !!i), "1", "0")) %>%
    filter((Date < ymd("2020-10-01")) & Date > ymd("2020-02-25"))
  
  plot <-
    ggplot() + 
    geom_line(data = base_tibble, aes(y = risk_of_openness, group = RegionName, size = dum,  x = Date, colour = dum)) + 
    scale_color_manual(values = c("0" = alpha("gray", 0.4), "1" = "red"), 
                       breaks = c("1"), 
                       name = "", 
                       labels = c(as.character(i))) + 
    scale_size_manual(values = c("1" = 1, "0" = 0.5)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") +
    expand_limits(y = c(0, 1)) +
    guides(size = F) +
    labs(x = "Data", 
         y = "Risco de Abertura",
         caption = "Fonte: OxCGRT Brazilian Sub-National Covid-19 Policy Responses. Disponível em: https://github.com/OxCGRT/Brazil-covid-policy") +
    theme(
      # Remove panel border
      #panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
      #panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "black"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      legend.key = element_blank(),
      legend.key.size = unit(3, "line"),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
      #panel.border = element_rect(colour = "black", fill=NA, size=1)
    )
  
  ggsave(plot = plot, 
         filename = filename,
         width = 12, 
         height = 8)
  message(paste0("Completed:", i))
}

## STATE_TOTAL Chloropleth maps ==================================
geopath <- "../data/input/shapefiles/BRUFE250GC_SIR.shp"

month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", 
                  "September", "October", "November", "December")

base_tibble <- 
  temp_tibble %>%
  mutate(month = month(Date)) %>%
  group_by(month, RegionName, StateCode) %>%
  summarise(mean_rooi = mean(risk_of_openness, na.rm = T),
            mean_stringency = mean(StringencyIndexForDisplay, na.rm = T), 
            mean_rooi = ifelse(is.finite(mean_rooi), mean_rooi, NA),
            mean_newcases = mean(moveave_newcases, na.rm = T),
            mean_newcases = ifelse(is.finite(mean_newcases), mean_newcases, NA),
            pop = mean(pop, na.rm = T)) %>%
  mutate(month = month.name[month],
         month = factor(month, levels = month_levels, labels = month_labels)) %>%
  filter(month != "January" & month != "Outubro")

base_tibble <- left_join(base_tibble, unique(regionmap %>% select(GeoRegion, RegionName)), by = c("RegionName"))

base_tibble <- 
  base_tibble %>%
  ungroup() %>%
  mutate(GeoRegion = ifelse(is.na(GeoRegion), "Mid West", as.character(GeoRegion)),
         StateCode = ifelse(RegionName == "Distrito Federal", 53, StateCode))


list.files(geopath)
brazilshp <- st_read(geopath)

brazilshp <- 
  left_join(brazilshp, base_tibble, by = c("CD_GEOCUF" = "StateCode"))

plot <- 
  ggplot(brazilshp) +
  geom_sf(aes(fill = mean_rooi)) + 
  ggthemes::theme_map() +
  labs(title = "",
       caption = "Fonte: OxCGRT Brazilian Sub-National Covid-19 Policy Responses. Disponível em: https://github.com/OxCGRT/Brazil-covid-policy" ) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"), 
        legend.position = "right", 
        strip.text = element_text(size = 10)) +
  scale_fill_viridis_c(option = "viridis", name = "Risco de Abertura", na.value = "white", direction = -1, 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
  facet_wrap(~month)

ggsave(plot = plot,
       filename = "../graphs/brazilrooi_state_chloropleth_monthly.png", 
       width = 12, 
       height = 8)


# END of STATE_TOTAL graphs ==========================================================

# CITY_TOTAL GRAPHS --------------------------------------------------------

## CITY_TOTAL scatter plots ==========================================================
key_cities <- 
  regionmap %>%
  filter(PolicyBrief == "Yes") %>%
  pull(CityName)

temp_tibble <- 
  city_all %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date < "2020-10-10") %>%
  select(CityName, CityCode, Date, RegionName,  risk_of_openness, pop_100k, moveave_newcases, cases_controlled, test_and_trace, community_understanding, manage_imported_cases, endemic_factor, StringencyIndexForDisplay) %>%
  filter(CityName != "")

month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", 
                  "September", "October", "November", "December")

base_tibble <- 
  temp_tibble %>%
  mutate(month = month(Date)) %>%
  group_by(month, CityName) %>%
  summarise(mean_rooi = mean(risk_of_openness, na.rm = T),
            mean_stringency = mean(StringencyIndexForDisplay, na.rm = T), 
            mean_rooi = ifelse(is.finite(mean_rooi), mean_rooi, NA),
            mean_newcases = mean(moveave_newcases, na.rm = T),
            pop_100k = mean(pop_100k),
            mean_newcases = ifelse(is.finite(mean_newcases), mean_newcases, NA)) %>%
  mutate(month = month.name[month],
         month = factor(month, levels = month_levels, labels = month_labels)) %>%
  filter(month != "January" & month != "Outubro")

base_tibble <- left_join(base_tibble, regionmap, by = c("CityName"))

base_tibble <- 
  base_tibble %>%
  mutate(GeoRegion = factor(GeoRegion, levels = region_levels, labels = region_labels))

plot <-
  ggplot(base_tibble, aes(x = mean_rooi, y = mean_stringency, label = CityName)) + #, label = CityName)) +# color = factor(lightup_state), label = CountryCode)) + 
  geom_point(aes(size = pop_100k, colour = GeoRegion)) + 
  scale_colour_viridis_d(name = "Região") +
  #lims(colour = c("0", "1")) + 
  geom_text_repel(data = subset(base_tibble, CityName %in% key_cities),
                  size = 3, colour = "black") +
#  geom_hline(yintercept = 50, size = 0.7, linetype = 2) + 
#  geom_vline(xintercept = 0.5, size = 0.7, linetype = 2) +
  labs(x = "Risco de Abertura", 
       y = "Índice de Rigidez", 
       #         title = "Stringency Index and Openness Risk over last quarter",
       #         subtitle = "(Bubble Size reflects number of new cases)", 
       caption = "O tamanho da bolha representa a população \n Fonte: OxCGRT Brazilian Sub-National Covid-19 Policy Responses. Disponível em: https://github.com/OxCGRT/Brazil-covid-policy") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"), 
        plot.subtitle = element_text(hjust = 0.5, size = 10)) +
  guides(size = F) + 
  expand_limits(y = c(0, 100)) +
  #scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2))) +
  #scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week\n (7 days prior to date)")) +
  scale_size(range = c(1,4)) +
  theme(
    # Remove panel border
    #panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
    #panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~ month) # ~ GeoRegion)

ggsave(plot = plot,
       filename = "../graphs/brazilrooi_cityall_scatterplot_monthly.png", 
       width = 12, 
       height = 8)


## CITY_TOTAL line plots ==========================================================
for(i in key_cities){
  filename <- paste0("../graphs/policy-briefs/brazilrooi_city_", i, "_lineplot.png")
  base_tibble <- 
    temp_tibble %>%
    mutate(dum = ifelse((CityName == !!i), "1", "0")) %>%
    filter((Date < ymd("2020-10-01")) & Date > ymd("2020-02-25"))
  
  plot <-
    ggplot() + 
    geom_line(data = base_tibble, aes(y = risk_of_openness, group = CityName, size = dum,  x = Date, colour = dum)) + 
    scale_color_manual(values = c("0" = alpha("gray", 0.4), "1" = "red"), 
                       breaks = c("1"), 
                       name = "", 
                       labels = c(as.character(i))) + 
    scale_size_manual(values = c("1" = 1, "0" = 0.5)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") +
    expand_limits(y = c(0, 1)) +
    guides(size = F) +
    labs(x = "Data", 
         y = "Risco de Abertura",
         caption = "Fonte: OxCGRT Brazilian Sub-National Covid-19 Policy Responses. Disponível em: https://github.com/OxCGRT/Brazil-covid-policy") +
    theme(
      # Remove panel border
      #panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
      #panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "black"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      legend.key = element_blank(),
      legend.key.size = unit(3, "line"),
      legend.text = element_text(size = 12),
      legend.position = "bottom"
      #panel.border = element_rect(colour = "black", fill=NA, size=1)
    )
  
  ggsave(plot = plot, 
         filename = filename,
         width = 12, 
         height = 8)
  message(paste0("Completed:", i))
  
}















# 
# 
# 
# 
# 
# base_tibble <- 
#   temp_tibble %>%
#   mutate(dum = ifelse((RegionName == "Sao Paulo"), "1", "0")) %>%
#   filter((Date < ymd("2020-10-01")) & Date > ymd("2020-02-25"))
# 
# ggplot() + 
#   geom_line(data = base_tibble, aes(y = risk_of_openness, group = RegionName, size = dum,  x = Date, colour = dum)) + 
#   scale_color_manual(values = c("0" = alpha("gray", 0.4), "1" = "red"), 
#                      breaks = c("1"), 
#                      name = "", 
#                      labels = c("Sao Paulo")) + 
#   scale_size_manual(values = c("1" = 1, "0" = 0.5)) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%d-%b") +
#   expand_limits(y = c(0, 1)) +
#   guides(size = F) +
#   labs(x = "Date", 
#        y = "Risk of Openness",
#        caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker") +
#   theme(
#     # Remove panel border
#     #panel.border = element_blank(),  
#     # Remove panel grid lines
#     panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
#     #panel.grid.minor = element_blank(),
#     # Remove panel background
#     panel.background = element_blank(),
#     # Add axis line
#     axis.line = element_line(colour = "grey"),
#     plot.caption = element_text(hjust = 0.5, face = "italic"),
#     legend.key = element_blank(),
#     legend.key.size = unit(3, "line"),
#     legend.text = element_text(size = 12),
#     legend.position = "bottom"
#     #panel.border = element_rect(colour = "black", fill=NA, size=1)
#   )
# 
# 
# # View(
# #   temp_tibble %>%
# #     #select(RegionCode, Date, ConfirmedCases, totalCases)
# #     filter(risk_of_openness ==1)
# # )
# # 
# # View(
# #   oxcgrtdata %>%
# #     filter(risk_of_openness == 1)
# # )
# 
# plot <-
#   ggplot(temp_tibble, aes(x = Date, group = 1)) + 
#   geom_line(aes(y = risk_of_openness)) + 
#   geom_line(aes(y = StringencyIndexForDisplay/100), colour = "red") + 
#   scale_y_continuous(
#     name = "Risk of Openness", 
#     sec.axis = sec_axis(~.*100, name = "Stringency Index")) + 
#   #scale_x_date(breaks = seq.Date(lubridate::ymd(min(lineplot_rollback$Date)), lubridate::ymd(date),30), 
#   #             date_labels = "%d-%b") + 
#   theme(axis.text.x = element_text(size = 6.5), 
#         axis.text.y.right = element_text(colour = "red"), 
#         axis.title.y.right = element_text(colour = "red"), 
#         strip.text = element_text(size = 15))+
#        # plot.caption = element_text(hjust = 0.5, face = "italic"), 
#       #  plot.title = element_text(hjust = 0.5)) + 
#   #labs(caption = paste("Data represented until ",date,"\n Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
#   #     or bsg.ox.ac.uk/covidtracker", sep = "")
#   #     #       ,title = "Openness Risk Index and Stringency Index of twelve countries over time"
#   #) + 
#   facet_wrap(~ RegionName)
# 
# ggsave(plot = plot,
#        filename = "./brazilrooi_lineplots.png", 
#        width = 12, 
#        height = 8)
# 
# 
# base_tibble <- 
#   temp_tibble %>%
#   group_by(RegionCode) %>%
#   filter(!is.na(test_and_trace) & !is.na(manage_imported_cases)) %>%
#   arrange(RegionCode, Date) %>%
#   slice_tail(1)
# 
# plot <- 
#   ggplot(base_tibble, aes(x = risk_of_openness, y = StringencyIndexForDisplay, label = RegionName)) + # color = factor(lightup_state), y = StringencyIndex,label = CountryCode)) +  #color = factor(outoflockdown), 
#   geom_point(aes(size = moveave_newcases)) + 
#   #lims(colour = c("0", "1")) + 
#   geom_text_repel(data = base_tibble, #subset(plot_rollback %>% filter(Date == date), lightup_state == 1 | key_country == 1 | ((openness_risk > 0.5) & (StringencyIndex < 50))), 
#                   size = 3, colour = "black") + 
#   #    annotate(geom = "text", x = 0.01, y = 37, label = "Countries below this range are scaling back lockdown", 
#   #             size = 2.5, hjust = "left") +
#   #    geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
#   geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
#   geom_segment(aes(x = 0.5, xend = 1, y = 50, yend = 50), color = "black", linetype = 2) +
#   #    geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 100), color = "black", linetype = 2) +
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.caption = element_text(hjust = 0.5, face = "italic"), 
#         plot.subtitle = element_text(hjust = 0.5, size = 10)) +
#   labs(x = "Risk of Openness", 
#        y = "Stringency Index", 
#        #         subtitle = "(Bubble size reflects number of new cases)", 
#        #         title = scatter.plot.title,
#        caption = "Bubble Size reflects number of new cases \n Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker") + 
#   guides(size = F) + 
#   #    viridis::scale_colour_viridis(discrete = T) +
#   scale_y_continuous(breaks = c(25, 50, 75, 100)) + 
#   scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2))) +
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in past week")) +
#   scale_size(range = c(3,9)) + 
#   annotate(geom = "label", x = 1, y = 99, label = "Group 1", 
#            color = "black") +
#   annotate(geom = "label", x = 1, y = 1, label = "Group 2", 
#            color = "red") + 
#   geom_label( x = 0, y = 50, label = "Group 3", 
#               color = "black", angle = 90) + 
#   annotate(geom = "text", x = 0, y = 99, label = "", 
#            color = "black") + 
#   annotate(geom = "rect", xmin = 0.5, ymin = 50, xmax = 1, ymax = 100, alpha = 0.075) +
#   annotate(geom = "rect", xmin = 0.5, ymin = 0, xmax = 1, ymax = 50, alpha = 0.15) +
#   facet_wrap(~Date)
# 
# ggsave(plot = plot,
#        filename = "./brazilrooi_scatterplotdetail.png", 
#        width = 12, 
#        height = 8)
# 
# # scatter plot panel 
# dateseq_scatter <- c("2020-04-15", "2020-05-15", "2020-06-15", "2020-07-15", "2020-08-15",
#                      "2020-09-15")
# 
# base_tibble <-
#   temp_tibble %>%
#   filter(Date %in% as.Date(dateseq_scatter))
# 
# plot <-
#   ggplot(base_tibble, aes(x = risk_of_openness, y = StringencyIndexForDisplay, label = RegionName)) +# color = factor(lightup_state), label = CountryCode)) + 
#   geom_point(aes(size = moveave_newcases)) + 
#   #lims(colour = c("0", "1")) + 
#   geom_text_repel(data = subset(base_tibble, StringencyIndexForDisplay < 50 | risk_of_openness < 0.5), 
#                   size = 3, colour = "black") +
#   geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
#   geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
#   labs(x = "Risk of Openness", 
#        y = "Stringency Index", 
#        #         title = "Stringency Index and Openness Risk over last quarter",
#        #         subtitle = "(Bubble Size reflects number of new cases)", 
#        caption = "Bubble Size reflects number of new cases \n Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker") + 
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.caption = element_text(hjust = 0.5, face = "italic"), 
#         plot.subtitle = element_text(hjust = 0.5, size = 10)) +
#   guides(size = F) + 
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
#   scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
#   scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2), 0.5)) +
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week\n (7 days prior to date)")) +
#   scale_size(range = c(2,7)) +
#   facet_wrap(~ Date)
# 
# ggsave(plot = plot,
#        filename = "./brazilrooi_scatterplotpanel.png", 
#        width = 12, 
#        height = 8)
# 
# 
# month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", 
#                   "September", "October", "November", "December")
# 
# base_tibble <- 
#   temp_tibble %>%
#   mutate(month = month(Date)) %>%
#   group_by(month, RegionName, StateCode) %>%
#   summarise(mean_rooi = mean(risk_of_openness, na.rm = T),
#             mean_stringency = mean(StringencyIndexForDisplay, na.rm = T), 
#             mean_rooi = ifelse(is.finite(mean_rooi), mean_rooi, NA),
#             mean_newcases = mean(moveave_newcases, na.rm = T),
#             mean_newcases = ifelse(is.finite(mean_newcases), mean_newcases, NA),
#             pop = mean(pop, na.rm = T)) %>%
#   mutate(month = month.name[month],
#          month = factor(month, levels = month_levels)) %>%
#   filter(month != "January" & month != "October")
# 
# base_tibble <- left_join(base_tibble, unique(regionmap %>% select(GeoRegion, RegionName)), by = c("RegionName"))
# 
# base_tibble <- 
#   base_tibble %>%
#   ungroup() %>%
#   mutate(GeoRegion = ifelse(is.na(GeoRegion), "Districto Federal", as.character(GeoRegion)),
#          StateCode = ifelse(RegionName == "Distrito Federal", 53, StateCode))
# 
# plot <-
#   ggplot(base_tibble, aes(x = mean_rooi, y = mean_stringency, label = RegionName)) +# color = factor(lightup_state), label = CountryCode)) + 
#   geom_point(aes(size = pop, colour = GeoRegion)) + 
#   #lims(colour = c("0", "1")) + 
#   scale_color_viridis_d(name = "Region") +
#   expand_limits(y = c(0,100)) +
#   geom_text_repel(data = subset(base_tibble, (mean_stringency < 50 & mean_rooi > 0.5) & month != "March"), 
#                   size = 3, colour = "black") +
#   geom_hline(yintercept = 50, size = 0.7, linetype = 2) + 
#   geom_vline(xintercept = 0.5, size = 0.7, linetype = 2) +
#   labs(x = "Risk of Openness", 
#        y = "Stringency Index", 
#        #         title = "Stringency Index and Openness Risk over last quarter",
#        #         subtitle = "(Bubble Size reflects number of new cases)", 
#        caption = "Bubble Size reflects population \n Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker") + 
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.caption = element_text(hjust = 0.5, face = "italic"), 
#         plot.subtitle = element_text(hjust = 0.5, size = 10)) +
#   guides(size = F) + 
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
#   scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
#   scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2), 0.5)) +
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week\n (7 days prior to date)")) +
#   scale_size(range = c(2,7)) +
#   theme(
#     # Remove panel border
#     #panel.border = element_blank(),  
#     # Remove panel grid lines
#     panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
#     #panel.grid.minor = element_blank(),
#     # Remove panel background
#     panel.background = element_blank(),
#     # Add axis line
#     axis.line = element_line(colour = "grey"),
#     plot.caption = element_text(hjust = 0.5, face = "italic"),
#     panel.border = element_rect(colour = "black", fill=NA, size=1)) +
#   facet_wrap(~ month)
# 
# ggsave(plot = plot,
#        filename = "./brazilrooi_stategov_scatterplotdetail_monthly.png", 
#        width = 12, 
#        height = 8)
# 
# base_tibble <- 
#   temp_tibble %>%
#   mutate(month = month(Date)) %>%
#   group_by(RegionName) %>%
#   mutate(mean_rooi = rollmean(risk_of_openness, k = 7, fill = NA),
#          mean_stringency = rollmean(StringencyIndexForDisplay, k = 7, fill = NA),
#          mean_newcases = rollmean(moveave_newcases, k = 7, fill = NA),
#          mean_rooi = ifelse(is.finite(mean_rooi), mean_rooi, NA),
#          mean_newcases = ifelse(is.finite(mean_newcases), mean_newcases, NA)) %>%
#   mutate(month = month.name[month],
#          month = factor(month, levels = month_levels)) %>%
#   filter(day(Date) == 15) %>%
#   filter(month != "January" & month != "October")
# 
# plot <-
#   ggplot(base_tibble, aes(x = mean_rooi, y = mean_stringency, label = RegionName)) +# color = factor(lightup_state), label = CountryCode)) + 
#   geom_point(aes(size = mean_newcases)) + 
#   #lims(colour = c("0", "1")) + 
#   geom_text_repel(data = subset(base_tibble, (mean_stringency < 50 | mean_rooi < 0.5)), 
#                   size = 3, colour = "black") +
#   geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
#   geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
#   labs(x = "Risk of Openness", 
#        y = "Stringency Index", 
#        #         title = "Stringency Index and Openness Risk over last quarter",
#        #         subtitle = "(Bubble Size reflects number of new cases)", 
#        caption = "Bubble Size reflects number of new cases \n Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker") + 
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.caption = element_text(hjust = 0.5, face = "italic"), 
#         plot.subtitle = element_text(hjust = 0.5, size = 10)) +
#   guides(size = F) + 
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
#   scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
#   scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2), 0.5)) +
#   #scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week\n (7 days prior to date)")) +
#   scale_size(range = c(2,7)) +
#   facet_wrap(~ month)
# 
# ggsave(plot = plot,
#        filename = "./brazilrooi_scatterplotdetail_monthly_15th.png", 
#        width = 12, 
#        height = 8)
# 
# ## Chloropleth Maps ---------------------------------------------------
# geopath <- "../data/input/shapefiles/BRUFE250GC_SIR.shp"
# 
# month_levels <- c("January", "February", "March", "April", "May", "June", "July", "August", 
#                   "September", "October", "November", "December")
# 
# base_tibble <- 
#   temp_tibble %>%
#   mutate(month = month(Date)) %>%
#   group_by(month, RegionName, StateCode) %>%
#   summarise(mean_rooi = mean(risk_of_openness, na.rm = T),
#             mean_stringency = mean(StringencyIndexForDisplay, na.rm = T), 
#             mean_rooi = ifelse(is.finite(mean_rooi), mean_rooi, NA),
#             mean_newcases = mean(moveave_newcases, na.rm = T),
#             mean_newcases = ifelse(is.finite(mean_newcases), mean_newcases, NA),
#             pop = mean(pop, na.rm = T)) %>%
#   mutate(month = month.name[month],
#          month = factor(month, levels = month_levels)) %>%
#   filter(month != "January" & month != "October")
# 
# base_tibble <- left_join(base_tibble, unique(regionmap %>% select(GeoRegion, RegionName)), by = c("RegionName"))
# 
# base_tibble <- 
#   base_tibble %>%
#   ungroup() %>%
#   mutate(GeoRegion = ifelse(is.na(GeoRegion), "Mid West", as.character(GeoRegion)),
#          StateCode = ifelse(RegionName == "Distrito Federal", 53, StateCode))
# 
# 
# list.files(geopath)
# brazilshp <- st_read(geopath)
# 
# brazilshp <- 
#   left_join(brazilshp, base_tibble, by = c("CD_GEOCUF" = "StateCode"))
# 
# ggplot(brazilshp) +
#   geom_sf(aes(fill = mean_rooi)) + 
#   ggthemes::theme_map() +
#   labs(title = "",
#        caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker" ) + 
#   theme(plot.title = element_text(hjust = 0.5), 
#         plot.caption = element_text(hjust = 0.5, face = "italic"), 
#         legend.position = "right", 
#         strip.text = element_text(size = 10)) +
#   scale_fill_viridis_c(option = "viridis", name = "Risk of Openness", na.value = "grey50", direction = -1, 
#                        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
#   facet_wrap(~Date)
# 
#   
# plot <- 
#   ggplot(data = brazilshp) + 
#   geom_sf(aes(fill = mean_rooi)) + 
#   scale_fill_viridis_c() +
#   facet_wrap(~month)
# 
# ggsave(plot = plot,
#        filename = "./brazilrooi_state_chloropleth_monthly_15th.png", 
#        width = 12, 
#        height = 8)