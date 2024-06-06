## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
#rm(list=ls()) # Clear out environment if necessary

source("code/packages.R")
source("code/extras.R")

getwd()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AMM_SST <- 
  read_excel('data/SST/osprey_climatology.xlsx', skip = 1) %>% 
  mutate(Month = as_date(paste(2021, Month, 15, sep = "-"))) %>%  # Use the middle/15th day of the month for plotting later
  bind_rows(read_excel('data/SST/osprey_climatology.xlsx', skip = 1) %>% 
              mutate(Month = as_date(paste(2022, Month, 15, sep = "-")))) %>% 
  filter(as_date(Month) >= as_date("2021-10-01"),
         as_date(Month) <= as_date("2022-10-31")) %>% 
  rename(AMM = SST)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
AMM_SST %>% 
  ggplot() +
  geom_line(aes(x = Month, y = AMM)) +
  geom_point(aes(x = Month, y = AMM)) +
  theme_bjc() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month of year") + 
  ylab("Averaged monthly mean SST \n (°C)")


## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SST <- read_csv('data/SST/sst_geopolar_osprey_19850101-20230430.csv', skip = 5) %>% 
  mutate(date = ymd(dmy(Date)), .keep = "unused", .before = "SST") %>% 
  filter(#SST >0,
         as_date(date) >= as_date("2021-10-01"),
         as_date(date) <= as_date("2022-10-31"))
save(SST, file = "data/RData/SST.Rdata")


## ----message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#SST_plot <- 
  ggplot() +
  geom_line(aes(x = date, y = SST), data = SST, colour = "black") +
  geom_line(aes(x = date, y = SST_N), data = SST, colour = "blue") +
  geom_line(aes(x = date, y = SST_S), data = SST, colour = "orange") +
  geom_line(aes(x = date, y = SST_E), data = SST, colour = "green4") +
  geom_line(aes(x = date, y = SST_W), data = SST, colour = "yellow") +
  theme_bjc() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  theme(axis.text.x = element_text(angle = 30,  vjust = 0.8)) +
  xlab("Month/year") + 
  ylab("Temp (°C)")
#SST_plot
#ggsave(filename = "output/SSTcomps.png", SST_plot)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_line(aes(x = Month, y = AMM), data = AMM_SST, col = "blue4") +
  geom_point(aes(x = Month, y = AMM), data = AMM_SST, col = "blue4", alpha =.5, size = 1) +
  geom_line(aes(x = date, y = SST), data = SST, colour = "black") +
  theme_bjc() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y') +
  #theme(axis.text.x = element_blank()) +
  xlab("Month") + 
  ylab("Temp (°C)")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
study_MM <- 
SST %>% 
  mutate(month = month(date, label = TRUE),
         year = year(date)) %>% 
  mutate(Month = as_date(paste(year, month, 15, sep = "-"))) %>%  # Use the middle/15th day of the month for plotting later
  group_by(year, Month) %>% 
  summarise(MM = mean(SST))%>% 
  left_join(AMM_SST) %>% 
  mutate(Month_diff = MM - AMM) 
study_MM


## ----fig.width = 110/25.4, fig.height = 70/25.4----------------------------------------------------------------------------------------------------------------------------------------------------------------
SSTvAMMplot <- 
ggplot() +
  geom_hline(yintercept = 28.69, colour = "grey60") +                # Plot the MMM
  geom_line(aes(x = date, y = SST), data = SST, colour = "grey20") +
  geom_line(aes(x = Month, y = AMM), data = AMM_SST, colour = "dodgerblue") +
  geom_point(aes(x = Month, y = AMM), data = AMM_SST, fill = "dodgerblue", colour = "black",
             stroke = 0.8, pch = 22, alpha = 0.9, size = 2) +
  geom_line(aes(x = Month, y = MM), data = study_MM, colour = "firebrick") +
  geom_point(aes(x = Month, y = MM), data = study_MM, fill = "firebrick", colour = "black",
             stroke = 0.8, pch = 24, alpha = 0.9, size = 2) +
  theme_bjc() +
  scale_x_date(date_breaks = "1 month", date_labels = '%b-%y', limits = c(as_date("2021-10-14"), as_date("2022-10-16"))) +
  theme(axis.text.x = element_text(size = 9, angle = 45,  vjust = 1, hjust = 1)) +
  theme(axis.text.y = element_text(size = 9)) +
  theme(axis.title = element_text(size = 9)) +
  xlab("Month/year") + 
  ylab("SST (°C)")
SSTvAMMplot


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggsave(filename = "output/SSTvAMMplot.pdf", SSTvAMMplot, width = 170, height = 100, units = "mm", dpi = 300)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
SST %>% 
    filter(SST == max(SST))

SST %>% 
    filter(SST == min(SST))  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
study_MM %>% 
  ungroup() %>% 
  summarise_at(vars(MM),list(min = min, median=median, mean = mean, max = max))


study_MM %>% 
  ungroup() %>% 
  summarise_at(vars(AMM),list(min = min, median=median, mean = mean, max = max))





## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# END

