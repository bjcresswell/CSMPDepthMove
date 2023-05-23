## ----include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
source("../packages.R")
#rm(list=ls()) # Clear out environment if necessary


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load(file = "../../data/Rdata/shark_tp_data.Rdata")
shark_tp_data


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------
setwd("../")
source("00_wrangle/wrangling_SSTdata.R")


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %>% 
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Depth)) +
  scale_y_reverse() +
  facet_wrap(~ transmitter_id)


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %>% 
  #filter(transmitter_id == "A69-9004-14037") %>%  # If you want to look at one individual
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Depth)) +
  scale_y_reverse() +
  facet_wrap(~ station_name_long)

shark_tp_data %>% 
  #filter(transmitter_id == "A69-9004-14037") %>%  # If you want to look at one individual
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Depth)) +
  scale_y_reverse() +
  facet_wrap(~ station_name)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_daily_depths <- 
  shark_tp_data %>% 
  mutate(date = date(detection_timestamp)) %>% 
  filter(!is.na(Depth)) %>% 
  group_by(date) %>% 
  summarise(daily_max_depth = max(Depth),
            daily_min_depth = min(Depth),
            daily_mean_depth = mean(Depth),
            daily_depth_sd = sd(Depth)) %>% 
  mutate(daily_min_depth = if_else(daily_min_depth <0, 0, daily_min_depth))
shark_daily_depths
#save(shark_daily_depths, file = "../data/Rdata/shark_daily_depths.Rdata")


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_daily_depth_plot <- 
shark_daily_depths %>%   
  ggplot() +
  geom_line(aes(x = date, y = daily_min_depth), colour = "skyblue") +
  #geom_smooth(aes(x = date, y = daily_min_depth), colour = "blue",  method = "gam", formula = y ~ s(x, bs = 'cs')) + # method = "gam" implements mgcv
  geom_line(aes(x = date, y = daily_mean_depth), colour = "dodgerblue") +
  geom_smooth(aes(x = date, y = daily_mean_depth), colour = "dodgerblue",  method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_line(aes(x = date, y = daily_max_depth), colour = "midnightblue") +
  geom_smooth(aes(x = date, y = daily_max_depth), colour = "midnightblue",  method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  #geom_smooth(aes(x = date, y = daily_max_depth), colour = "red") + # Basic smooth, rather than gam
  theme_minimal() +
  scale_y_reverse() +
  #scale_x_date(date_breaks = "1 months", date_labels = '%Y-%m') +
  #theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  #xlab("Date") +
  ylab("Depth \n (m below sealevel)")

shark_daily_depth_plot


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_weekly_depths <- 
shark_tp_data %>% 
  mutate(week_date = date(floor_date(detection_timestamp, "week"))) %>% 
  filter(!is.na(Depth)) %>% 
  group_by(week_date) %>% 
  summarise(weekly_max_depth = max(Depth),
            weekly_min_depth = min(Depth),
            weekly_mean_depth = mean(Depth),
            weekly_depth_sd = sd(Depth)) %>% 
  mutate(weekly_min_depth = if_else(weekly_min_depth <0, 0, weekly_min_depth))
shark_weekly_depths


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_weekly_depth_plot <- 
shark_weekly_depths %>% 
  ggplot() +
  geom_line(aes(x = week_date, y = weekly_min_depth), colour = "blue") +
  geom_line(aes(x = week_date, y = weekly_mean_depth), colour = "grey30") +
  geom_smooth(aes(x = week_date, y = weekly_mean_depth), colour = "grey30", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  geom_line(aes(x = week_date, y = weekly_max_depth), colour = "red") +
  geom_smooth(aes(x = week_date, y = weekly_max_depth), colour = "red", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  scale_y_reverse() +
  theme_minimal() +
  xlab("Date") +
  ylab("Averaged weekly depths \n (m below sealevel)")
shark_weekly_depth_plot


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_individual_daily_depths <- 
  shark_tp_data %>% 
  mutate(date = date(detection_timestamp)) %>% 
  filter(!is.na(Depth)) %>% 
  group_by(date, transmitter_id, Sex, TL) %>% 
  summarise(daily_max_depth = max(Depth),
            daily_min_depth = min(Depth),
            daily_mean_depth = mean(Depth),
            daily_depth_sd = sd(Depth))%>% 
  mutate(daily_min_depth = if_else(daily_min_depth <0, 0, daily_min_depth)) %>% 
  full_join(SST)
shark_individual_daily_depths


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_individual_daily_depth_plot <- 
shark_individual_daily_depths %>%   
  ggplot() +
  geom_line(aes(x = date, y = daily_min_depth), colour = "skyblue") +
  #geom_smooth(aes(x = date, y = daily_min_depth), colour = "blue",  method = "gam", formula = y ~ s(x, bs = 'cs')) + # method = "gam" implements mgcv
  geom_line(aes(x = date, y = daily_mean_depth), colour = "dodgerblue") +
  geom_smooth(aes(x = date, y = daily_mean_depth), colour = "dodgerblue",  method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_line(aes(x = date, y = daily_max_depth), colour = "midnightblue") +
  geom_smooth(aes(x = date, y = daily_max_depth), colour = "midnightblue",  method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  #geom_smooth(aes(x = date, y = daily_max_depth), colour = "red") + # Basic smooth, rather than gam
  theme_minimal() +
  scale_y_reverse() +
  #scale_x_date(date_breaks = "1 months", date_labels = '%Y-%m') +
  #theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  #xlab("Date") +
  ylab("Depth \n (m below sealevel)") +
  facet_wrap(facets = "transmitter_id")

shark_individual_daily_depth_plot


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_individual_daily_depths %>%   
  ggplot() +
  geom_line(aes(x = date, y = daily_min_depth), colour = "skyblue") +
  #geom_smooth(aes(x = date, y = daily_min_depth), colour = "blue",  method = "gam", formula = y ~ s(x, bs = 'cs')) + # method = "gam" implements mgcv
  geom_line(aes(x = date, y = daily_mean_depth), colour = "dodgerblue") +
  geom_smooth(aes(x = date, y = daily_mean_depth), colour = "dodgerblue",  method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_line(aes(x = date, y = daily_max_depth), colour = "midnightblue") +
  geom_smooth(aes(x = date, y = daily_max_depth), colour = "midnightblue",  method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  #geom_smooth(aes(x = date, y = daily_max_depth), colour = "red") + # Basic smooth, rather than gam
  theme_minimal() +
  scale_y_reverse() +
  #scale_x_date(date_breaks = "1 months", date_labels = '%Y-%m') +
  #theme(axis.text.x = element_text(angle = 45)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  #xlab("Date") +
  ylab("Depth \n (m below sealevel)") +
  facet_wrap(facets = "Sex")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %>%
  select(transmitter_id, Sex) %>% 
  distinct() %$%
  summary(Sex)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_individual_weekly_depths <- 
shark_tp_data %>% 
  mutate(week_date = date(floor_date(detection_timestamp, "week"))) %>% 
  filter(!is.na(Depth)) %>% 
  group_by(week_date, transmitter_id, Sex, TL) %>% 
  summarise(weekly_max_depth = max(Depth),
            weekly_min_depth = min(Depth),
            weekly_mean_depth = mean(Depth),
            weekly_depth_sd = sd(Depth))%>% 
  mutate(weekly_min_depth = if_else(weekly_min_depth <0, 0, weekly_min_depth))

shark_individual_weekly_depths


## ----fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
shark_individual_weekly_depth_plot <- 
shark_individual_weekly_depths %>% 
  ggplot() +
  geom_line(aes(x = week_date, y = weekly_min_depth), colour = "blue") +
  geom_line(aes(x = week_date, y = weekly_mean_depth), colour = "grey30") +
  geom_smooth(aes(x = week_date, y = weekly_mean_depth), colour = "grey30", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  geom_line(aes(x = week_date, y = weekly_max_depth), colour = "red") +
  geom_smooth(aes(x = week_date, y = weekly_max_depth), colour = "red", method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.95) +
  scale_y_reverse() +
  theme_minimal() +
  xlab("Date") +
  ylab("Averaged weekly depths \n (m below sealevel)") +
  facet_wrap(facets = "transmitter_id")

shark_individual_weekly_depth_plot

