library(oce)
library(ocedata)

getwd()
ctd_raw <- read.ctd('data/CTD/FK200802_CTD010_20200820.cnv')

osprey_ctd <- 
  ctd_raw@data %>% 
  as_tibble() %>% 
  filter(depth <101)
