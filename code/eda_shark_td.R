## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
source("packages.R")
rm(list=ls()) # Clear out environment if necessary


## -----------------------------------------------------------------------------
#load(file = "../data/Rdata/shark_daily_temps.Rdata")
#load(file = "../data/Rdata/shark_daily_depths.Rdata")
#load(file = "../data/Rdata/shark_tp_data.Rdata")


## ----echo=FALSE, message=FALSE------------------------------------------------
source(knitr::purl("../code/eda_shark_depth.Rmd", quiet=TRUE))
source(knitr::purl("../code/eda_shark_temp.Rmd", quiet=TRUE))


## -----------------------------------------------------------------------------
shark_tp_data %>% 
  #filter(transmitter_id == "A69-9004-14037") %>%  # If you want to look at one individual
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Depth)) +
  scale_y_reverse()

shark_tp_data %>% 
  #filter(transmitter_id == "A69-9004-14037") %>%   # If you want to look at one individual
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Temp)) 


## -----------------------------------------------------------------------------
daily_tdplot <- shark_daily_depth_plot/shark_daily_temps_plot
daily_tdplot


## -----------------------------------------------------------------------------
#ggsave(filename = "../output/tdplot.png", plot = tdplot, width = 16, height = 10, units = "cm")


## -----------------------------------------------------------------------------
shark_daily_td <- 
  full_join(shark_daily_depths, shark_daily_temps)


## -----------------------------------------------------------------------------
shark_td_plot <- 
shark_daily_td %>% 
  ggplot() +
  geom_point(aes(x = daily_max_temp, y = daily_max_depth)) +
  #geom_smooth(aes(x = daily_max_temp, y = daily_max_depth), method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_smooth(aes(x = daily_max_temp, y = daily_max_depth), method = "lm") +
  theme_minimal() +
  xlab("Daily maximum temp (°C)") + 
  ylab("Daily maximum depth (m below sea surface)")

