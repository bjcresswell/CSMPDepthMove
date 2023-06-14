## ----include=FALSE------------------------------------------------------------
#source("packages.R")
#rm(list=ls()) # Clear out environment if necessary```


## ----load-dep2023i, message=FALSE, warning=FALSE------------------------------
NHorn2023_D1 <- read_csv('data/ROV/2023-02-22 08-35-42 vehicle1.csv') %>% 
  mutate(Time = as_hms(clock.currentTime)) %>% 
  select(Time, altitudeRelative, temperature.temperature1, temperature.temperature2) %>% 
  filter(Time > parse_hms("08:39:00")) %>%  # Beginning of the first 90m+ transect
  filter(Time < parse_hms("08:59:00"))  # End of dive


## ----load-dep2023ii, message=FALSE, warning=FALSE-----------------------------
NHorn2023_D2 <- read_csv('data/ROV/2023-02-22 08-35-42 vehicle1.csv') %>% 
  mutate(Time = as_hms(clock.currentTime)) %>% 
  select(Time, altitudeRelative, temperature.temperature1, temperature.temperature2) %>% 
  filter(Time > parse_hms("09:11:30")) %>%  # ROV redeployed and sent back down to ~50m
  filter(Time < parse_hms("09:35:00"))  # End of dive


## ----load-dep2021i, message=FALSE, warning=FALSE------------------------------
OspEnt2021 <- read_csv('data/ROV/2021-02-23 11-32-50 vehicle1.csv') %>% 
  mutate(Time = as_hms(clock.currentTime)) %>% 
  select(Time, altitudeRelative, temperature.temperature1, temperature.temperature2)  %>% 
  filter(Time > parse_hms("12:02:30")) %>%  # Beginning of the first 95m+ transect
  filter(Time < parse_hms("13:20:30"))  # End of dive


## ----load-dep2021ii, message=FALSE, warning=FALSE-----------------------------
NHorn2021_D1 <- read_csv('data/ROV/2021-02-24 07-43-14 vehicle1.csv') %>% 
  mutate(Time = as_hms(clock.currentTime)) %>% 
  select(Time, altitudeRelative, temperature.temperature1, temperature.temperature2)  %>% 
  filter(Time > parse_hms("09:08:30"))  %>%  # Beginning of the first ~50m transect
  filter(Time < parse_hms("09:40:30"))  # End of dive


## ----load-dep2021iii, message=FALSE, warning=FALSE----------------------------
NHorn2021_D2 <- read_csv('data/ROV/2021-02-24 10-59-22 vehicle1.csv') %>% 
  mutate(Time = as_hms(clock.currentTime)) %>% 
  select(Time, altitudeRelative, temperature.temperature1, temperature.temperature2)  %>% 
  filter(Time > parse_hms("11:28:00"))  %>%  # Beginning of the first ~50m transect
  filter(Time < parse_hms("12:22:30"))  # End of dive


## -----------------------------------------------------------------------------
#sumtemp2023 <- 
ggplot() +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2023_D1, colour = 'slateblue', size = 0.2) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2023_D2, colour = 'navy', size = 0.2) +
  scale_x_continuous(limits = c(24, 31)) +
  theme_minimal() +
  xlab("Temp (°C)") + 
  ylab("Depth (m below sea level")

#sumtemp2023


## -----------------------------------------------------------------------------
#sumtemp2021 <- 
ggplot() +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2021_D1, colour = 'grey70', size = 0.5) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2021_D2, colour = 'grey50', size = 0.5) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = OspEnt2021, colour = 'grey30', size = 0.5) +
  scale_x_continuous(limits = c(24, 31)) +
  theme_minimal() +
  xlab("Temp (°C)") + 
  ylab("Depth (m below sea level")

#sumtemp2021


## -----------------------------------------------------------------------------
ROV_temp_comb <- 
  NHorn2021_D1 %>% 
  full_join(NHorn2021_D2) %>% 
  full_join(OspEnt2021) %>% 
  full_join(NHorn2023_D1) %>% 
  full_join(NHorn2023_D2) %>% 
  mutate(depth = -altitudeRelative,
         temp = temperature.temperature2,
         .keep = "none") %>% 
  arrange(depth)


## -----------------------------------------------------------------------------
ROV_temp_comb %>% 
  ggplot() +
  geom_point(aes(x = depth, y = temp), size = 0.2) +
  geom_smooth(aes(x = depth, y = temp))

ROV_temp_comb %>% 
  ggplot() +
  geom_point(aes(x = temp, y = depth), size = 0.2) +
  geom_smooth(aes(x = temp, y = depth),  method = "gam", formula = y ~ s(x, bs = 'cs'))

BLUEROVplot <- 
  ROV_temp_comb %>% 
  ggplot() +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2023_D1, colour = 'grey10', size = 0.3) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2023_D2, colour = 'grey20', size = 0.3) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2021_D1, colour = 'grey30', size = 0.3) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = NHorn2021_D2, colour = 'grey45', size = 0.3) +
  geom_point(aes(x = temperature.temperature2, y = altitudeRelative), data = OspEnt2021, colour = 'grey60', size = 0.3) +
  #geom_smooth(aes(x = temp, y = depth)) +
  #geom_smooth(aes(x = temp, y = depth),  method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.99, colour = 'red') +
  #geom_hline(yintercept = -50, colour = "blue", lty = '22') +
  #geom_vline(xintercept = 28.21, colour = "blue", lty = '22') +
  scale_x_continuous(limits = c(24.5, 30.5), breaks = c(24, 25, 26, 27, 28, 29, 30)) +
  theme_minimal() +
  xlab("Temp (°C)") + 
  ylab("Depth (m below sea level")

ROV_GAMplot <- 
  ROV_temp_comb %>% 
  ggplot() +
  #geom_smooth(aes(x = temp, y = depth)) +
  geom_smooth(aes(x = temp, y = depth),  method = "gam", formula = y ~ s(x, bs = 'cs'), level = 0.99, colour = 'red') +
  geom_hline(yintercept = -50, colour = "blue", lty = '22') +
  geom_vline(xintercept = 28.21, colour = "blue", lty = '22') +
  scale_x_continuous(limits = c(24.5, 30.5), breaks = c(24, 25, 26, 27, 28, 29, 30)) +
  theme_minimal() +
  xlab("Temp (°C)") + 
  ylab("Depth (m below sea level")

BLUEROVplot + ROV_GAMplot

