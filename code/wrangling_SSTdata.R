# Script to wrangle in climatology (AMM) and daily SSTs

#getwd()
#setwd("code")
setwd("../")
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AMM_SST <- 
  read_excel('data/SST/osprey_climatology.xlsx', skip = 1) %>% 
  mutate(Month = as_date(paste(2021, Month, 01, sep = "-")))                 # Need to dummy code in year and day in order to have as time series 

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
print(AMMplot)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Check what time span required for SST data
#shark_tp_data %>% summarise(min(detection_timestamp), max(detection_timestamp))

## ----load-geopolSST, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
SST <- read_csv('data/SST/sst_geopolar_osprey_19850101-20230430.csv', skip = 5) %>% 
  mutate(Date = ymd(dmy(Date))) %>% 
  filter(#SST >0,
         as_date(Date) >= as_date("2021-10-28"),
         as_date(Date) <= as_date("2022-10-31"))

## ----plot-geopolSST, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
SST_plot <- 
  ggplot() +
  geom_line(aes(x = Date, y = SST), data = SST, colour = "black") +
  geom_line(aes(x = Date, y = SST_N), data = SST, colour = "blue") +
  geom_line(aes(x = Date, y = SST_S), data = SST, colour = "orange") +
  geom_line(aes(x = Date, y = SST_E), data = SST, colour = "green4") +
  geom_line(aes(x = Date, y = SST_W), data = SST, colour = "yellow") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month/year") + 
  ylab("Temp (°C)")

print(SST_plot)

#ggsave(filename = "output/SSTcomps.png", SST_plot)


## ----plot-AMM&SST, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
AMM_forplot <-  read_excel('data/SST/osprey_climatology.xlsx', skip = 1) %>% 
  mutate(Month = as_date(paste(2022, Month, 01, sep = "-"))) %>%                  # Need to dummy code in year and day in order to have as time series 
  bind_rows(AMM_SST) %>% 
  filter(as_date(Month) >= as_date("2021-10-28"),
         as_date(Month) <= as_date("2022-11-30"))



SSTvsClimatology <- 
AMM_forplot %>% 
ggplot() +
  geom_line(aes(x = Month, y = SST)) +
  geom_line(aes(x = Date, y = SST), data = SST, colour = "black") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month/year") + 
  ylab("Temp (°C)")

print(SSTvsClimatology)
#ggsave(filename = "output/SSTvsClimatology.png", SSTvsClimatology)


## ----anomalies, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------

anomaly <- 
SST %>% 
  mutate(month = month(Date, label = TRUE)) %>% 
  group_by(month) %>% 
  summarise(MM = mean(SST)) %>% 
  bind_cols(AMM_SST) %>% 
  mutate(Month_diff = MM - SST) 
anomaly

anomaly_plot <- 
anomaly %>% 
  ggplot() +
  geom_line(aes(x = Month, y = MM), colour = "red") +
  geom_line(aes(x = Month, y = SST), colour = "blue") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month/year") + 
  ylab("Temp (°C)")

anomaly_plot


avg_anomaly <- 
  anomaly %>% 
  summarise(mean = mean(Month_diff), sd = sd(Month_diff))

avg_anomaly





