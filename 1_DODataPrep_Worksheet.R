library(dplyr)
library(lubridate)
library(zoo)

gall_final2 <- read.csv("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/1_Code/1_MetabolismEstimates_ARModel/MetabolismEstimates_ARModel/0_Gall_Interp_Only_site2.csv")

obs_per_day <- gall_final2 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#if still need to drop specific days
gallKEEP <- gallKEEP %>%
  group_by(day) %>%
  filter(day != 70) %>%    
  ungroup()

#Make sure both time formats are in the correct format, because that is what we will bind with
gallKEEP <- gallKEEP %>%
  mutate(
    time_utc = as.POSIXct(time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC"),
    time_mt_aligned = as.POSIXct(time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver"))

gall_final2 <- gall_final2 %>%
  mutate(
    time_utc = as.POSIXct(time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC"),
    time_mt_aligned = as.POSIXct(time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver"),
    solar.time = as.POSIXct(solar.time, format = "%m/%d/%Y %H:%M"))

#BIND THE DATASETS (INTERPOLATED TO MAIN FRAME)

days_interp <- unique(gall_final2$day)

gallKEEP <- gallKEEP %>%
  filter(!day %in% days_interp)

gall2 <- bind_rows(gallKEEP, gall_final2) %>%
  arrange(time_utc)


gall2$solar.time<-convert_UTC_to_solartime(gall2$time_utc, longitude= long, time.type="mean solar")
gall2$light<- calc_light(gall2$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall2$Kc <- Kcor(gall2$temp,1)
