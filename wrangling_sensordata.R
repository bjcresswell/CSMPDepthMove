## ----include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
source('code/packages.R')
#rm(list=ls()) # Clear out environment if necessary
getwd()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load("../CSMPWrangling/data/RData/alldata.RData")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
osp_sens_data <- 
  alldata %>% 
  filter(installation_name == 'Osprey') %>% 
  filter(Type != "pinger") %>% 
  droplevels()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(osp_sens_data$station_name_long)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load("../CSMPWrangling/data/RData/project_tag_list.RData")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
osp_sens_data %>% 
  filter(Type == "temp")  %>% 
  group_by(transmitter_id, Scientific_name) %>% 
  distinct(Scientific_name)  %$% 
  summary(Scientific_name)

project_tag_list %>% 
  #filter(Type != "pinger") %$% 
  filter(Type == "temp")  %>% 
  droplevels() %$% 
  summary(Scientific_name)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
project_tag_list %>% 
  filter(Org_type == "Teleost") %>% 
  filter(Type == "temp") %>% 
  #distinct(transmitter_id) %>% 
  anti_join(osp_sens_data) %>% 
  arrange(transmitter_id)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
osp_sens_data %>% 
group_by(station_name_long) %>% 
  summarise(first_detection = min(detection_timestamp),
            last_detection = max(detection_timestamp)) %>% 
  mutate(deploy_duration = round(last_detection - first_detection, 0))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#osp_receiver_times <- 
osp_sens_data %>% 
  ggplot(mapping = aes(x = detection_timestamp, y = station_name_long)) + 
  xlab("Date") + 
  ylab("VR2W Location") +
  geom_point() +
  geom_vline(xintercept = as.numeric(osp_sens_data$detection_timestamp[c(5, 709325)])) +
  theme_light()
#osp_receiver_times
#ggsave(plot = osp_receiver_times, filename = "../output/osp_receiver_times.png", device = 'png', width = 180, height = 100, units = "mm")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
osp_sens_data %>% 
  dplyr:: select(transmitter_id, Serial, Org_type, Type) %>% 
  distinct() %>% 
  arrange(transmitter_id)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_temp_data <- 
  osp_sens_data %>%
  filter(Org_type == "Elasmobranch") %>% 
  filter(ID %% 2 != 1)  %>% 
  #dplyr::select(transmitter_id) %>% 
  mutate(ID = ID+1) %>% 
  mutate(transmitter_id = factor(paste(Freq, Space, ID, sep = '-'))) %>% 
  mutate(Temp = `Sensor.Value` * Slope + Intercept)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_press_data <- 
  osp_sens_data %>%
  filter(Org_type == "Elasmobranch") %>% 
  filter(Type =="press") %>% 
  mutate(Depth = `Sensor.Value` * Slope + Intercept) %>% 
  mutate(Depth = if_else(Depth <0, 0, Depth)) # Have a few negative values (up to c. -3) - clearly cannot be out of the water so will reset to 0


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data <- 
  shark_temp_data %>% 
  full_join(shark_press_data) %>% 
  dplyr::select(transmitter_id, detection_timestamp, station_name, station_name_long, Scientific_name, Date_tagged, 
         Location_tagged, Site_tagged, Sex, FL, TL, Lat, Long, Temp, Depth) %>% 
  filter(!grepl("Lagoon", station_name_long),        # just a few shark detections (2 individs 14041 and 14059) from lagoon1 so filtering out
#         station_name_long != "Overnight_Mooring"   # may need to leave ONM out as it only has shallow/hot detections but will leave in for now
         ) %>%  
  filter(detection_timestamp < as.Date("2022-11-01 00:00:01")) %>% 
  arrange(detection_timestamp) %>% 
  droplevels()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data <- shark_tp_data %>% 
  mutate(date = date(detection_timestamp)) %>% 
  mutate(hour = hour(detection_timestamp)) %>% 
  mutate(tod = case_when(hour %in% (7:16) ~ "Day",
                         hour %in% (5:6) ~ "Dawn",
                         hour %in% (17:18) ~ "Dusk",
                         TRUE ~ "Night")) %>% 
  mutate(Season = factor(case_when(
    date >= date("2022-01-01") & date<= date("2022-03-31") ~ "Summer",
    date >= date("2022-07-01") & date<= date("2022-09-30") ~ "Winter",
    date < date("2022-07-01") & date > date("2022-03-31") ~ "Autumn",
    date < date("2022-12-31")  ~ "Spring",
    date > date("2022-09-30") ~ "Spring"))) %>% 
  filter(date > date("2021-10-27"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data # No. of observations ~ 725,224
# Note this number is lower than the total # of shark observations as we've filtered out post-Oct2022
 osp_sens_data %>%
  filter(Org_type == "Elasmobranch") 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
shark_tp_data %$% summary(transmitter_id) # No. of individuals detected - all 15 sharks show up at some point & have 1000's of obs for each
#shark_tp_data  %>% distinct(transmitter_id) # Alternative summary
shark_tp_data %$% summary(station_name_long) # No. of individuals detected - all 15 sharks show up at some point & have 1000's of obs for each



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
save(shark_tp_data, file = "data/Rdata/shark_tp_data.Rdata")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
teleost_temp_data <- 
  osp_sens_data %>%
  filter(Org_type == "Teleost") %>% 
  filter(ID %% 2 == 1)  %>% 
  #dplyr::select(transmitter_id) %>% 
  mutate(ID = ID+1) %>% 
  mutate(transmitter_id = factor(paste(Freq, Space, ID, sep = '-'))) %>% 
  mutate(Temp = `Sensor.Value` * Slope + Intercept)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
teleost_press_data <- 
  osp_sens_data %>%
  filter(Org_type == "Teleost") %>% 
  filter(Type =="press") %>% 
  mutate(Depth = `Sensor.Value` * Slope + Intercept) %>% 
  mutate(Depth = if_else(Depth <0, 0, Depth)) # Have a few negative values (up to c. -3) - clearly cannot be out of the water so will reset to 0



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
teleost_tp_data <- 
  teleost_temp_data %>% 
  full_join(teleost_press_data) %>% 
  select(transmitter_id, detection_timestamp, station_name, station_name_long, Scientific_name, Date_tagged, 
         Location_tagged, Site_tagged, Sex, FL, TL, Lat, Long, Temp, Depth) %>% 
  arrange(detection_timestamp) %>%   
  droplevels()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
teleost_tp_data <- teleost_tp_data %>% 
  mutate(date = date(detection_timestamp)) %>% 
  mutate(hour = hour(detection_timestamp)) %>% 
  mutate(tod = case_when(hour %in% (7:16) ~ "Day",
                         hour %in% (5:6) ~ "Dawn",
                         hour %in% (17:18) ~ "Dusk",
                         TRUE ~ "Night")) %>% 
  mutate(Season = factor(case_when(
    date >= date("2022-01-01") & date<= date("2022-03-31") ~ "Summer",
    date >= date("2022-07-01") & date<= date("2022-09-30") ~ "Winter",
    date < date("2022-07-01") & date > date("2022-03-31") ~ "Autumn",
    date < date("2022-12-31")  ~ "Spring",
    date > date("2022-09-30") ~ "Spring"))) %>% 
  filter(date > date("2021-10-27"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
teleost_tp_data # No. of observations ~ 41,000
teleost_tp_data %$% summary(transmitter_id) # No. of individuals detected - only 8 individuals. Already established we are missing 2 GTs


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
save(teleost_tp_data, file = "data/Rdata/teleost_tp_data.Rdata")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#END

