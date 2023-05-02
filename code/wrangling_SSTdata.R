## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------
library(tidyverse)


## ----warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
#rm(list=ls()) # Clear out environment if necessary


## -------------------------------------------------------------------------------------------------------------------------------------------------
#shark_tp_data %>% summarise(min(detection_timestamp), max(detection_timestamp))


## ----load-SST11, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------
SST11mic <- read_csv('../data/SST/g4.areaAvgTimeSeries.MODISA_L3m_NSST_8d_4km_R2019_0_sst.20211019-20230305.146E_13S_146E_13S.csv', 
                     skip = 7) %>% 
  rename(temp = "MODISA_L3m_NSST_8d_4km_R2019_0_sst") %>% 
  filter(temp >0,
         as_date(time) >= as_date("2021-10-20")) %>% 
  mutate(date = date(time),
         satellite = factor('11m_night'), 
         .keep = "unused") %>% 
  mutate(year = year(date),
         week = week(date),           # Need to use isoweek for this dataset
         year_week = paste(year, week)) %>% 
  mutate(year_week2 = str_c(str_sub(isoyear(date), 3, 4),  "/", formatC(isoweek(date), format = "f", digits = 0, width = 2, flag = "0")))
         

SST11mic   


## ----load-SST4, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------
SST4mic <- read_csv('../data/SST/g4.areaAvgTimeSeries.MODISA_L3m_SST4_8d_4km_R2019_0_sst4.20211019-20230305.146E_13S_146E_13S.csv', 
                     skip = 7) %>% 
  rename(temp = "MODISA_L3m_SST4_8d_4km_R2019_0_sst4") %>% 
  filter(temp >0,
          as_date(time) >= as_date("2021-10-20"))%>% 
  mutate(date = date(time),        
         satellite = factor('4m_night'),
         .keep = "unused") %>% 
  mutate(year = year(date),
         week = week(date),                # Need to use normal week for the 2 nighttime datasets or it will calculate first obs for 2023 as Wk 53, 2022!!
         year_week = paste(year, week)) %>% 
  mutate(year_week2 = str_c(str_sub(isoyear(date), 3, 4),  "/", formatC(isoweek(date), format = "f", digits = 0, width = 2, flag = "0")))
         


## ----load-SST11day, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------
SST11micday <- read_csv('../data/SST/g4.areaAvgTimeSeries.MODISA_L3m_SST_8d_4km_R2019_0_sst.20211019-20230305.146E_13S_146E_13S.csv', 
                     skip = 7) %>% 
  rename(temp = "mean_MODISA_L3m_SST_8d_4km_R2019_0_sst") %>% 
  filter(temp >0,
          as_date(time) >= as_date("2021-10-20"))%>% 
  mutate(date = date(time), 
         satellite = factor('11m_day'),
         .keep = "unused") %>% 
  mutate(year = year(date),
         week = week(date),                          # Need to use normal week for the 2 nighttime datasets or it will calculate first obs for 2023 as Wk 53, 2022!!
         year_week = paste(year, week)) %>% 
  mutate(year_week2 = str_c(str_sub(isoyear(date), 3, 4),  "/", formatC(isoweek(date), format = "f", digits = 0, width = 2, flag = "0")))
         


## -------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_point(aes(x = date, y = temp), data = SST4mic, colour = "blue") +
  geom_line(aes(x = date, y = temp), data = SST4mic, colour = "blue") +
  geom_point(aes(x = date, y = temp), data = SST11mic, colour = "red") +
  geom_line(aes(x = date, y = temp), data = SST11mic, colour = "red") +
  geom_point(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  geom_line(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  theme_minimal()


## -------------------------------------------------------------------------------------------------------------------------------------------------
SSTcomb <- 
  full_join(SST4mic, SST11mic) %>% 
  full_join(SST11micday) %>% 
  arrange(year_week2)


## -------------------------------------------------------------------------------------------------------------------------------------------------
SST_comb2 <- 
SSTcomb %>% 
  group_by(year, week, year_week2) %>% 
  summarise(mean_temp = mean(temp)) %>% 
  ungroup() %>% 
  mutate(lag_mean_temp = lag(mean_temp),
         roll_mean = (lag_mean_temp + mean_temp)/2)


SST_comb3 <- 
SSTcomb %>% 
  group_by(date) %>% 
  summarise(mean_temp = mean(temp)) %>% 
  ungroup() %>% 
  mutate(lag_mean_temp = lag(mean_temp),
         roll_mean = (lag_mean_temp + mean_temp)/2)




## -------------------------------------------------------------------------------------------------------------------------------------------------
SST_comb2 %>% 
  ggplot(aes(x = year_week2, y = roll_mean)) +
  geom_point(colour = "blue") +
 theme(axis.text.x = element_text(angle = 90))
SST_comb2 %>% 
  ggplot(aes(x = year_week2, y = mean_temp)) +
  geom_point(colour = "blue") +
  geom_line() +
 theme(axis.text.x = element_text(angle = 90))


## -------------------------------------------------------------------------------------------------------------------------------------------------
SST_plot <- SST_comb3 %>% 
  ggplot(aes(x = date, y = roll_mean)) +
  geom_point(colour = "blue") +
  geom_line(colour = "grey") +
  theme(axis.text.x = element_text(angle = 90))

SST_comb2


## -------------------------------------------------------------------------------------------------------------------------------------------------
SSTcomb %>% 
  mutate(year = year(date),
         week = isoweek(date),
         year_week = factor(paste(year, week, sep = "-"))) %>% 
  group_by(year, week, year_week) %>% 
  summarise(mean_temp = mean(temp)) %>% 
  ungroup() %>% 
  mutate(prior_mean_temp = lag(mean_temp),
         movavg = (prior_mean_temp + mean_temp)/2)


## -------------------------------------------------------------------------------------------------------------------------------------------------
SSTcomb %>% 
  mutate(year = year(date),
         week = isoweek(date),
         year_week = lubridate::parse_date_time(paste(year, week, 1, sep="/"),'Y/W/w')) %>% 
  group_by(year, week, year_week) %>% 
  summarise(mean_temp = mean(temp)) %>% 
  ungroup() %>% 
  mutate(prior_mean_temp = lag(mean_temp),
         movavg = (prior_mean_temp + mean_temp)/2)


## -------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_point(aes(x = date, y = temp), data = SSTcomb, colour = "blue") +
  geom_line(aes(x = date, y = temp), data = SSTcomb, colour = "blue")


## -------------------------------------------------------------------------------------------------------------------------------------------------
SST_2021a <- 
  SSTcomb %>% 
  mutate(year = year(time),
         week = isoweek(time),
         year_week = factor(paste(year, week, sep = "-"))) %>% 
  filter(year == 2021) 
  
SST_2021b <- 
  SSTcomb %>% 
  mutate(year = year(time),
         week = week(time),
         year_week = factor(paste(year, week, sep = "-"))) %>% 
  filter(year == 2021) 
  


SST_2022



SST_2023


## -------------------------------------------------------------------------------------------------------------------------------------------------
SST_2021a
SST_2021b


## -------------------------------------------------------------------------------------------------------------------------------------------------
n <- 2

SST_comb2 <- 
SSTcomb %>% 
  mutate(year = year(date),
         week = isoweek(date),
         year_week = factor(paste(year, week, sep = "-")),
         prior_temp = lag(temp),
         movavg = (prior_temp + temp)/2) 
  #group_by(fnight = (week-1)%/% 2 + 1) %>% 
  #mutate(mean_temp = mean(temp)) %>%

SST_comb2


## -------------------------------------------------------------------------------------------------------------------------------------------------
SST_comb2 %>% 
ggplot() +
  geom_point(aes(x = year_week, y = movavg), colour = "blue") +
  geom_line(aes(x = year_week, y = movavg), colour = "blue")

