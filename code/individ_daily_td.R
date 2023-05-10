### Depth
shark_1_d <- 
  shark_tp_data %>% 
  mutate(date = date(detection_timestamp),
         Depth = if_else(Depth <0, 0, Depth)) %>% 
  filter(!is.na(Depth)) %>% 
  group_by(transmitter_id, date, TL, Sex) %>% 
  summarise(daily_max_depth = max(Depth),
            daily_min_depth = min(Depth),
            daily_mean_depth = mean(Depth),
            daily_depth_sd = sd(Depth))

shark_1_d

### Temp
shark_1_t <- 
  shark_tp_data %>% 
  mutate(date = date(detection_timestamp)) %>% 
  filter(!is.na(Temp)) %>% 
  group_by(transmitter_id, date, TL, Sex) %>% 
  summarise(daily_max_temp = max(Temp),
            daily_min_temp = min(Temp),
            daily_mean_temp = mean(Temp),
            daily_temp_sd = sd(Temp))

shark_1_t

## We also will need SST as a predictor for max depth 
#The only data set we have at a daily temporal scale is CRW (for the moment) so we'll use that for now

shark_1_td <- 
  full_join(shark_1_d, shark_1_t) %>% 
  full_join(CRWdata) %>% 
  rename(SST = temp) %>% 
  mutate(Season = factor(case_when(
    date >= date("2022-01-01") & date<= date("2022-03-31") ~ "Summer",
    date >= date("2022-07-01") & date<= date("2022-09-30") ~ "Winter",
    date < date("2022-07-01") & date > date("2022-03-31") ~ "Autumn",
    date < date("2022-12-31")  ~ "Spring",
    date > date("2022-09-30") ~ "Spring"))) %>% 
  filter(date > date("2021-10-27"))

# Checks
shark_1_td %$% summary(Season) # Should all be assigned to a season
shark_1_td %>% filter(is.na(Season)) # Should be no NAs
