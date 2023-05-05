## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
source("packages.R")
#rm(list=ls()) # Clear out environment if necessary


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load(file = "../data/Rdata/shark_tp_data.Rdata")
shark_tp_data


## ----fig.width=16, fig.height=6--------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %>% 
  #filter(station_name %in% c("CS-O1", "CS-O2")) %>% 
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Temp)) +
  facet_wrap(~ transmitter_id)


## ----fig.width=16, fig.height=6--------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %>% 
  #filter(station_name %in% c("CS-O1", "CS-O2")) %>% 
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Temp)) +
  facet_wrap(~ station_name_long)

shark_tp_data %>% 
  #filter(station_name %in% c("CS-O1", "CS-O2")) %>% 
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Temp)) +
  facet_wrap(~ station_name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_daily_temps <- 
  shark_tp_data %>% 
  mutate(date = date(detection_timestamp)) %>% 
  filter(!is.na(Temp)) %>% 
  group_by(date) %>% 
  summarise(daily_max_temp = max(Temp),
            daily_min_temp = min(Temp),
            daily_mean_temp = mean(Temp))
shark_daily_temps
#save(shark_daily_temps, file = "../data/Rdata/shark_daily_temps.Rdata")


## ----echo=FALSE, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
source(knitr::purl("../code/wrangling_SSTdata.Rmd", quiet=TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#SST_comb3 <-   SST_comb3 %>%   filter(date < as.Date("2022-11-01"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_daily_temps_plot <- 
shark_daily_temps %>% 
ggplot() +
  geom_line(aes(x = date, y = daily_max_temp), colour = "red") +
  geom_smooth(aes(x = date, y = daily_max_temp), colour = "red") +
  geom_line(aes(x = date, y = daily_min_temp), colour = "blue") +
  geom_smooth(aes(x = date, y = daily_min_temp), colour = "blue") +
  geom_line(aes(x = date, y = daily_mean_temp), colour = "grey40") +
  geom_smooth(aes(x = date, y = daily_mean_temp), colour = "grey40") +
  geom_line(aes(x = week_date, y = weekly_rolling_mean), data = SSTcomb, colour = "black") +
  geom_smooth(aes(x = week_date, y = weekly_rolling_mean), data = SSTcomb, colour = "black") +
  geom_line(aes(x = date, y = temp), data = CRWdata, colour = "orange") +
  scale_x_date(date_breaks = "1 months", date_labels = '%Y-%m') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  #annotate("text", x = as.Date("2021-11-01"), y = 30.5, label = "a") +
  xlab("\nMonth of year") + 
  ylab("Temp (°C)")

shark_daily_temps_plot



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ggsave(filename = "../output/shark_daily_temps_plot.png", plot = shark_daily_temps_plot, width = 16, height = 10, units = "cm")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_weekly_temps <- 
  shark_tp_data %>% 
  mutate(week_date = date(floor_date(detection_timestamp, "week"))) %>%
  filter(!is.na(Temp)) %>% 
  group_by(week_date) %>% 
  summarise(weekly_max_temp = max(Temp),
            weekly_min_temp = min(Temp),
            weekly_mean_temp = mean(Temp))
shark_weekly_temps
#save(shark_weekly_temps, file = "../data/Rdata/shark_weekly_temps.Rdata")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_weekly_temps_plot <- 
shark_weekly_temps %>% 
  ggplot() +
  geom_line(aes(x = week_date, y = weekly_min_temp), colour = "blue") +
  #geom_smooth(aes(x = week_date, y = weekly_min_temp), colour = "blue", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  geom_smooth(aes(x = week_date, y = weekly_min_temp), colour = "blue", method = "loess", level = 0.95) +
  geom_line(aes(x = week_date, y = weekly_mean_temp), colour = "grey30") +
  geom_smooth(aes(x = week_date, y = weekly_mean_temp), colour = "grey30", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  geom_line(aes(x = week_date, y = weekly_max_temp), colour = "red") +
  geom_smooth(aes(x = week_date, y = weekly_max_temp), colour = "red", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  scale_x_date(date_breaks = "1 months", date_labels = '%Y-%m') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Date") +
  ylab("Averaged weekly depths \n (m below sealevel)")
shark_weekly_temps_plot

