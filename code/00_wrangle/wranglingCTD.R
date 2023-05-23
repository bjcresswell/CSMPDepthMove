library(oce)
library(ocedata)

getwd()
#setwd('code')
ctd_raw <- read.ctd('../data/CTD/FK200802_CTD010_20200820.cnv')

osprey_ctd <- 
  ctd_raw@data %>% 
  as.tibble() %>% 
  filter(depth <150)