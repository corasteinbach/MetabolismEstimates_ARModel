#Manipulating data so it meets the requirements for the AR model 
#Requirements:
#A day "begins" on the first measurement after 04:00 solar.time, see my code below.  It ends on the last measurement before 04:00 the following day
#Each day needs exactly 144 estimates.  So interpolate if missing a few.
#If missing a lot delete the entire day from just after 4 AM to just before 4 AM the day after.  You know the data will be right when you take the length of your enitire time series and divide by 144 and you get an integer number of days.
###########################--Site 1--#########################################################
library(tidyverse)
library(lubridate)
library(streamMetabolizer)
library(deSolve)
library(dplyr)

Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MeabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.36
long<-  -111.2
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall$time_utc <- with_tz(ymd_hms(gall$time_mt_aligned, tz = "America/Denver"), "UTC")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")

##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall$solar.time<-convert_UTC_to_solartime(gall$time_utc, longitude= long, time.type="mean solar")
gall$light<- calc_light(gall$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall$Kc <- Kcor(gall$temp,1)



######Site 1

gall1<- gall[gall$site_id==1,]

head(gall1)
tail(gall1)

plot(gall1$solar.time,gall1$oxy, type="l")

gall1<- gall1[gall1$solar.time > ymd_hms("2025-05-07 04:05:25") &  gall1$solar.time < ymd_hms("2025-10-27 03:59:25") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall1)
tail(gall1)
length(gall1$oxy)

##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall1$day <- (as.numeric(factor(floor_date(gall1$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall1 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to separate only the # of days without 144 obs
lessthan144_site1 <-obs_per_day %>% 
  filter(n_obs != 144)

#drop days with <= 113 obs. (this would be gaps of more than 3 hours)
gallKEEP <- gall1 %>%
  group_by(day) %>%
  filter(n() >= 113) %>%    # KEEP days with 114 or more obs
  ungroup()

#now need to interpolate obs for days with less than 144 days
obs_per_day <- gallKEEP %>%
  count(day)

days_to_interp <- obs_per_day %>%
  filter(n != 144) %>%
  pull(day)

gall_interp_only <- gallKEEP %>%
  filter(day %in% days_to_interp)

#save the "gall_interp_only" as its own csv for interpolation step
write.csv(gall_interp_only, "0_Gall_Interp_Only_site1.csv")

#Interpolating obs now, go to other Rscript (I modified some code I used when cleaning the DO data)

plot(gall1$solar.time,gall1$oxy_cor, type="l")

#run this line of code
gall1$osat<- calc_DO_sat(temp.water=gall1$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))

#rename whatever object makes it down here to 'gall1' & remove unnecessary columns
gall1 <- gall1 %>% select(-"X.2", -"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", -"oxy", -"percentsat", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                           "Kc", -"X.1","osat")
#Checking once more that there are 144 obs. per day
obs_per_day <- gall1 %>%
  count(day)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall1, "0_Gall1Data_144obsperday.csv")
#DONE HERE

#Bob code starts again 
gall1$osat<- calc_DO_sat(temp.water=gall1$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))

gall1sum<-gall1 %>% group_by(day) %>% summarise(sumlight=sum(light), logq=log(mean(Q)))


gall1_metab_list<- list(T=length(gall1$solar.time), D=length(gall1$solar.time)/144,
                        y=gall1$oxy_cor, light=gall1$light,oxysat=gall1$osat,day=gall1$day,
                        Kc=gall1$Kc,sumlight=gall1sum$sumlight, logQ=gall1sum$logq, z=1, ts=1/144)

###########################--Site 2--#########################################################
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MeabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.11
long<-  -111.2
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall$time_utc <- with_tz(ymd_hms(gall$time_mt_aligned, tz = "America/Denver"), "UTC")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")

##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall$solar.time<-convert_UTC_to_solartime(gall$time_utc, longitude= long, time.type="mean solar")
gall$light<- calc_light(gall$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall$Kc <- Kcor(gall$temp,1)



######Site 2

gall2<- gall[gall$site_id==2,]

head(gall2)
tail(gall2)

plot(gall2$solar.time,gall2$oxy, type="l")

gall2<- gall2[gall2$solar.time > ymd_hms("2025-05-07 04:05:25") &  gall1$solar.time < ymd_hms("2025-10-27 03:59:25") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall2<-gall2[!is.na(gall2$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall2)
tail(gall2)
length(gall2$oxy)

##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall2$day <- (as.numeric(factor(floor_date(gall2$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall2 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to separate only the # of days without 144 obs
lessthan144_site2 <-obs_per_day %>% 
  filter(n_obs != 144)

#drop days with <= 113 obs. (this would be gaps of more than 3 hours)
gallKEEP <- gall2 %>%
  group_by(day) %>%
  filter(n() >= 113) %>%    # KEEP days with 114 or more obs
  ungroup()

#now need to interpolate obs for days with less than 144 days
obs_per_day <- gallKEEP %>%
  count(day)

days_to_interp <- obs_per_day %>%
  filter(n != 144) %>%
  pull(day)

gall_interp_only2 <- gallKEEP %>%
  filter(day %in% days_to_interp)

#save the "gall_interp_only" as its own csv for interpolation step
write.csv(gall_interp_only2, "0_Gall_Interp_Only_site2.csv")

#Interpolating obs now, go to other Rscript (I modified some code I used when cleaning the DO data)

plot(gall2$solar.time,gall2$oxy_cor, type="l")

#run this line of code
gall2$osat<- calc_DO_sat(temp.water=gall2$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))

#rename whatever object makes it down here to 'gall1' & remove unnecessary columns
gall2 <- gall2 %>% select( -"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", -"oxy", -"percentsat", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc", -"X.1","osat")
#Checking once more that there are 144 obs. per day
obs_per_day <- gall2 %>%
  count(day)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall2, "0_Gall2Data_144obsperday.csv")

###########################--Site 3--#########################################################
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.25
long<-  -111.25


#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
gall$time_mt_aligned <- as.POSIXct(gall$time_mt_aligned, format = "%m/%d/%Y %H:%M", tz = "America/Denver")

gall$time_utc <- with_tz(ymd_hms(gall$time_mt_aligned, tz = "America/Denver"), "UTC")

##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall$solar.time<-convert_UTC_to_solartime(gall$time_utc, longitude= long, time.type="mean solar")
gall$light<- calc_light(gall$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall$Kc <- Kcor(gall$temp,1)



######Site 3

gall3<- gall[gall$site_id==3,]

head(gall3)
tail(gall3)


gall3<- gall3[gall3$solar.time > ymd_hms("2025-05-07 04:02:13") &  gall3$solar.time < ymd_hms("2025-10-27 03:57:13") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall3<-gall3[!is.na(gall3$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall3)
tail(gall3)
length(gall3$oxy)

plot(gall3$solar.time,gall3$oxy, type="l")

##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall3$day <- (as.numeric(factor(floor_date(gall3$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall3 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to separate only the # of days without 144 obs
lessthan144_site3 <-obs_per_day %>% 
  filter(n_obs != 144)

#drop days with <= 113 obs. (this would be gaps of more than 3 hours)
gallKEEP <- gall3 %>%
  group_by(day) %>%
  filter(n() >= 113) %>%    # KEEP days with 114 or more obs
  ungroup()

#now need to interpolate obs for days with less than 144 days
obs_per_day <- gallKEEP %>%
  count(day)

days_to_interp <- obs_per_day %>%
  filter(n != 144) %>%
  pull(day)

gall_interp_only3 <- gallKEEP %>%
  filter(day %in% days_to_interp)

#save the "gall_interp_only" as its own csv for interpolation step
write.csv(gall_interp_only3, "0_Gall_Interp_Only_site3.csv")

#Interpolating obs now, go to other Rscript (I modified some code I used when cleaning the DO data)

plot(gall3$solar.time,gall3$oxy_cor, type="l")

#run this line of code
gall3$osat<- calc_DO_sat(temp.water=gall3$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))

#rename whatever object makes it down here to 'gall1' & remove unnecessary columns
gall3 <- gall3 %>% select( -"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                           "sensor", "site_id", "temp", -"oxy", -"percentsat", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                           "Kc", -"X.1","osat")
#Checking once more that there are 144 obs. per day
obs_per_day <- gall3 %>%
  count(day)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall3, "0_Gall3Data_144obsperday.csv")

###########################--Site 4--#########################################################
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.30
long<-  -111.21


######Site 4

gall4 <- gall[gall$site_id==4,]

head(gall4)
tail(gall4)

#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall4$time_utc <- as.POSIXct(gall4$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
gall4$time_mt_aligned <- as.POSIXct(gall4$time_mt_aligned, format = "%m/%d/%Y %H:%M", tz = "America/Denver")

gall4$time_utc <- with_tz(ymd_hms(gall4$time_mt_aligned, tz = "America/Denver"), "UTC")

##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall4$solar.time<-convert_UTC_to_solartime(gall4$time_utc, longitude= long, time.type="mean solar")
gall4$light<- calc_light(gall4$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall4$Kc <- Kcor(gall4$temp,1)


gall4<- gall4[gall4$solar.time > ymd_hms("2025-05-06 04:08:22") &  gall4$solar.time < ymd_hms("2025-10-27 04:05:22") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall4<-gall4[!is.na(gall4$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall4)
tail(gall4)
length(gall4$oxy)

plot(gall4$solar.time,gall4$oxy, type="l")

##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall4$day <- (as.numeric(factor(floor_date(gall4$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall4 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to separate only the # of days without 144 obs
lessthan144_site4 <-obs_per_day %>% 
  filter(n_obs != 144)

#drop days with <= 113 obs. (this would be gaps of more than 3 hours)
gallKEEP <- gall4 %>%
  group_by(day) %>%
  filter(n() >= 113) %>%    # KEEP days with 114 or more obs
  ungroup()

#now need to interpolate obs for days with less than 144 days
obs_per_day <- gallKEEP %>%
  count(day)

days_to_interp <- obs_per_day %>%
  filter(n != 144) %>%
  pull(day)

gall_interp_only4 <- gallKEEP %>%
  filter(day %in% days_to_interp)

#save the "gall_interp_only" as its own csv for interpolation step
write.csv(gallKEEP, "0_Gall_Interp_Only_site4.csv")

#Working with obs now, go to other csv I just saved and manually find those days missing obs!

plot(gall4$solar.time,gall4$oxy_cor, type="l")

###########################Site 5###########################################
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.36
long<-  -111.2
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall5<- gall[gall$site_id==5,]

gall5 <- gall5 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall5$time_mt_aligned = as.POSIXct(gall5$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall5$time_utc <- with_tz(ymd_hms(gall5$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall5$unixtime <- as.numeric(gall5$time_utc)

head(gall5)
tail(gall5)

gall5$solar.time<-convert_UTC_to_solartime(gall5$time_utc, longitude= long, time.type="mean solar")
gall5$light<- calc_light(gall5$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall5$Kc <- Kcor(gall5$temp,1)

gall5<- gall5[gall5$solar.time > ymd_hms("2025-05-07 04:00:00") &  gall5$solar.time < ymd_hms("2025-10-24 04:00:00") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall5<-gall5[!is.na(gall5$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall5)
tail(gall5)
length(gall5$oxy)

plot(gall5$solar.time,gall5$oxy, type="l")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall5$day <- (as.numeric(factor(floor_date(gall5$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall5 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to separate only the # of days without 144 obs
lessthan144_site5 <-obs_per_day %>% 
  filter(n_obs != 144)

#drop days with <= 113 obs. (this would be gaps of more than 3 hours)
gallKEEP <- gall5 %>%
  group_by(day) %>%
  filter(n() >= 113) %>%    # KEEP days with 114 or more obs
  ungroup()

#now need to interpolate obs for days with less than 144 days
obs_per_day <- gallKEEP %>%
  count(day)

days_to_interp <- obs_per_day %>%
  filter(n != 144) %>%
  pull(day)

gall_interp_only <- gallKEEP %>%
  filter(day %in% days_to_interp)

#save the "gall_interp_only" as its own csv for interpolation step
write.csv(gall5, "0_Gall_Interp_Only_site5.csv")

#Working with obs now, go to other csv I just saved and manually find those days missing obs!

###########################Site 6###########################################
Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.55
long<-  -111.23
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall6<- gall[gall$site_id==6,]

gall6 <- gall6 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall6$time_mt_aligned = as.POSIXct(gall6$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall6$time_utc <- with_tz(ymd_hms(gall6$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall6$unixtime <- as.numeric(gall6$time_utc)

head(gall6)
tail(gall6)

gall6$solar.time<-convert_UTC_to_solartime(gall6$time_utc, longitude= long, time.type="mean solar")
gall6$light<- calc_light(gall6$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall6$Kc <- Kcor(gall6$temp,1)

gall6<- gall6[gall6$solar.time > ymd_hms("2025-05-06 04:00:00") &  gall6$solar.time < ymd_hms("2025-10-27 04:00:00") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall6<-gall6[!is.na(gall6$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall6)
tail(gall6)
length(gall6$oxy)

plot(gall6$solar.time,gall6$oxy, type="l")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall6$day <- (as.numeric(factor(floor_date(gall6$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall6 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to separate only the # of days without 144 obs
lessthan144_site6 <-obs_per_day %>% 
  filter(n_obs != 144)

#drop days with <= 113 obs. (this would be gaps of more than 3 hours)
gallKEEP <- gall6 %>%
  group_by(day) %>%
  filter(n() >= 113) %>%    # KEEP days with 114 or more obs
  ungroup()

#now need to interpolate obs for days with less than 144 days
obs_per_day <- gallKEEP %>%
  count(day)

days_to_interp <- obs_per_day %>%
  filter(n != 144) %>%
  pull(day)

#save the "gall_interp_only" as its own csv for interpolation step
write.csv(gallKEEP, "0_Gall_Interp_Only_site6.csv")

#Working with obs now, go to other csv I just saved and manually find those days missing obs!