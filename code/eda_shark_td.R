## ----include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
getwd()
source("../packages.R")
#rm(list=ls()) # Clear out environment if necessary


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#load(file = "../data/Rdata/shark_daily_temps.Rdata")
#load(file = "../data/Rdata/shark_daily_depths.Rdata")
#load(file = "../data/Rdata/shark_tp_data.Rdata")


## ----echo=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
source(knitr::purl("eda_shark_depth.Rmd", quiet=TRUE))


## ----echo=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
source(knitr::purl("eda_shark_temp.Rmd", quiet=TRUE))


## ----echo=FALSE, message=FALSE, warning=FALSE, fig.width=16, fig.height=6------------------------------------------------------------------------------------------------------
setwd("../")
source("00_wrangle/wrangling_SSTdata.R")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %>% 
  #filter(transmitter_id == "A69-9004-14037") %>%  # If you want to look at one individual
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Depth)) +
  scale_y_reverse()

shark_tp_data %>% 
  #filter(transmitter_id == "A69-9004-14037") %>%   # If you want to look at one individual
  ggplot() +
  geom_point(aes(x = detection_timestamp, y = Temp)) 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
daily_tdplot <- shark_daily_depth_plot/shark_daily_temps_plot
daily_tdplot


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#ggsave(filename = "../output/tdplot.png", plot = tdplot, width = 16, height = 10, units = "cm")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_daily_td <- 
  SST %>% 
  full_join(shark_daily_depths) %>% 
  full_join(shark_daily_temps)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_td_lm_plot <- 
shark_daily_td %>% 
  ggplot() +
  geom_point(aes(x = SST, y = daily_max_depth)) +
  #geom_smooth(aes(x = daily_max_temp, y = daily_max_depth), method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_smooth(aes(x = SST, y = daily_max_depth), method = "lm") +
  theme_minimal() +
  ylab("Daily maximum depth (mbsl)") + 
  xlab("SST")
shark_td_lm_plot


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_individual_daily_td <- 
 shark_individual_daily_depths %>% 
  full_join(shark_individual_daily_temps)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_td_lm_individ_plot <- 
shark_individual_daily_td %>% 
  ggplot() +
  geom_point(aes(x = SST, y = daily_max_depth)) +
  #geom_smooth(aes(x = daily_max_temp, y = daily_max_depth), method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_smooth(aes(x = SST, y = daily_max_depth), method = "lm") +
  theme_minimal() +
  ylab("Daily maximum depth (mbsl)") + 
  xlab("SST") +
  facet_wrap(facets = "transmitter_id")
shark_td_lm_individ_plot


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# END

