## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
source("packages.R")
#rm(list=ls()) # Clear out environment if necessary


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AMM_SST <- 
  read_excel('../data/SST/CRWdata_NCSI_LongTerm.xlsx', skip = 1) %>% 
  mutate(Month = as_date(paste(2000, Month, 01, sep = "-")))                 # Need to dummy code in year and day in order to have as time series 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AMMplot <- 
AMM_SST %>% 
  ggplot() +
  geom_line(aes(x = Month, y = SST)) +
  geom_point(aes(x = Month, y = SST)) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Averaged monthly mean SST \n (°C)")
AMMplot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#shark_tp_data %>% summarise(min(detection_timestamp), max(detection_timestamp))


## ----load-SST11, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
SST11mic <- read_csv('../data/SST/g4.areaAvgTimeSeries.MODISA_L3m_NSST_8d_4km_R2019_0_sst.20211019-20230305.146E_13S_146E_13S.csv', skip = 7) %>% 
  mutate(date = date(time),
         temp = MODISA_L3m_NSST_8d_4km_R2019_0_sst, 
         satellite = factor('11m_night')) %>% 
  select(date, temp, satellite) %>% 
  filter(temp >0,
         as_date(date) >= as_date("2021-10-20"),
         as_date(date) <= as_date("2022-10-31"))


## ----load-SST4, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
SST4mic <- read_csv('../data/SST/g4.areaAvgTimeSeries.MODISA_L3m_SST4_8d_4km_R2019_0_sst4.20211019-20230305.146E_13S_146E_13S.csv', skip = 7) %>% 
  mutate(date = date(time),
         temp = MODISA_L3m_SST4_8d_4km_R2019_0_sst4,
         satellite = factor('4m_night')) %>% 
  select(date, temp, satellite) %>% 
  filter(temp >0,
         as_date(date) >= as_date("2021-10-20"),
         as_date(date) <= as_date("2022-10-31")) 


## ----load-SST11day, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
SST11micday <- read_csv('../data/SST/g4.areaAvgTimeSeries.MODISA_L3m_SST_8d_4km_R2019_0_sst.20211019-20230305.146E_13S_146E_13S.csv', skip = 7) %>% 
  mutate(date = date(time),
         temp = mean_MODISA_L3m_SST_8d_4km_R2019_0_sst,
         satellite = factor('11m_day')) %>% 
  select(date, temp, satellite) %>% 
  filter(temp >0,
         as_date(date) >= as_date("2021-10-20"),
         as_date(date) <= as_date("2022-10-31")) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CRWdata <- read_excel('../data/SST/CRWdata_NCSislands.xlsx') %>% 
  mutate(date = as_date(paste(YYYY, MM, DD, sep = "-")),
         temp = SST_MEAN,
         satellite = factor("CRW_NESDIS")) %>% 
  select(date, temp, satellite) %>% 
  filter(temp >0,
         as_date(date) >= as_date("2021-10-20"),
         as_date(date) <= as_date("2022-10-31"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
CRWplot <- 
ggplot() +
  geom_line(aes(x = date, y = temp), data = CRWdata, colour = "red") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Temp (°C)")
CRWplot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MODISplot <- 
ggplot() +
  geom_line(aes(x = date, y = temp), data = SST4mic, colour = "grey") +
  geom_line(aes(x = date, y = temp), data = SST11mic, colour = "grey60", ) +
  geom_line(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 45,  vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Temp (°C)")
  

MODISplot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#MODIS_comb <- 
#  full_join(SST4mic, SST11mic) %>% 
#  full_join(SST11micday)

#MODIS_comb %>% 
#  ggplot() +
#  geom_line(aes(x = date, y = temp), colour = "black") +
#  geom_smooth(aes(x = date, y = temp))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MODIS_mean <- 
  full_join(SST4mic, SST11mic) %>% 
  full_join(SST11micday) %>% 
  mutate(week_date = floor_date(date, "week")) %>% 
  group_by(week_date) %>% 
  summarise(mean_week_temp = mean(temp)) %>% 
  mutate(lag_mean_week_temp = lag(mean_week_temp),
         weekly_rolling_mean = (lag_mean_week_temp + mean_week_temp)/2) 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MODISmean_vs_CRWplot <- 
ggplot() +
  geom_line(aes(x = week_date, y = mean_week_temp), data = MODIS_mean, colour = "blue") +
  geom_smooth(aes(x = week_date, y = mean_week_temp), data = MODIS_mean, colour = "blue", method = "gam", formula = y ~ s(x, bs = 'cs')) +
  geom_line(aes(x = date, y = temp), data = CRWdata, colour = "black") +
  geom_smooth(aes(x = date, y = temp), data = CRWdata, colour = "black", method = "gam", formula = y ~ s(x, bs = 'cs')) +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 45,  vjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Temp (°C)")
  
MODISmean_vs_CRWplot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_line(aes(x = week_date, y = mean_week_temp), data = MODIS_mean, colour = "blue") +
  #geom_line(aes(x = date, y = temp), data = SST4mic, colour = "blue") +
  #eom_line(aes(x = date, y = temp), data = SST11mic, colour = "red") +
  #geom_line(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  geom_line(aes(x = date, y = temp), data = CRWdata, colour = "red") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Temp (°C)")
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  #geom_point(aes(x = date, y = temp), data = SST4mic, colour = "blue") +
  geom_line(aes(x = date, y = temp), data = SST4mic, colour = "blue") +
  #geom_point(aes(x = date, y = temp), data = SST11mic, colour = "red") +
  geom_line(aes(x = date, y = temp), data = SST11mic, colour = "red") +
  #geom_point(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  geom_line(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  #geom_point(aes(x = date, y = temp), data = CRWdata, colour = "orange") +
  geom_line(aes(x = date, y = temp), data = CRWdata, colour = "orange") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Temp (°C)")
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_line(aes(x = date, y = temp), data = SST4mic, colour = "blue") +
  geom_line(aes(x = date, y = temp), data = SST11mic, colour = "red") +
  geom_line(aes(x = date, y = temp), data = SST11micday, colour = "black") +
  geom_line(aes(x = date, y = temp), data = CRWdata, colour = "orange") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Temp (°C)")
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SSTcomb <- 
  full_join(SST4mic, SST11mic) %>% 
  full_join(SST11micday) %>% 
  full_join(CRWdata) %>% 
  mutate(week_date = floor_date(date, "week")) %>% 
  group_by(week_date) %>% 
  summarise(mean_week_temp = mean(temp)) %>% 
  mutate(lag_mean_week_temp = lag(mean_week_temp),
         weekly_rolling_mean = (lag_mean_week_temp + mean_week_temp)/2) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#SST_all_plot <- 
 ggplot() +
  geom_line(aes(x = week_date, y = weekly_rolling_mean), data = SSTcomb, colour = "blue") +
  geom_line(aes(x = week_date, y = weekly_rolling_mean), data = MODIS_mean, colour = "red") +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5)) +
  scale_x_date(date_breaks = "1 months", date_labels = '%Y-%m')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


