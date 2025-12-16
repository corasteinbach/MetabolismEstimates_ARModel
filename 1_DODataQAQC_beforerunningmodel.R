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

gall1<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_Gall1Data_144obsperday.csv")
lat<- 45.36
long<-  -111.2
#Remove UTC time columns, we are rebuilding them from aligned time
gall1 <- gall1 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall1$time_mt_aligned = as.POSIXct(gall1$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall1$time_utc <- with_tz(ymd_hms(gall1$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall1$unixtime <- as.numeric(gall1$time_utc)
#Remove solar time column before rebuilding
gall1 <- gall1 %>% select(-"solar.time")
gall1 <- gall1 %>% select(-"light", -"Kc")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall1$solar.time<-convert_UTC_to_solartime(gall1$time_utc, longitude= long, time.type="mean solar")
gall1$light<- calc_light(gall1$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall1$Kc <- Kcor(gall1$temp,1)



######Site 1

#gall1<- gall[gall$site_id==1,]

#head(gall1)
tail(gall1)

plot(gall1$solar.time,gall1$oxy, type="l")

# #gall1<- gall1[gall1$solar.time > ymd_hms("2025-05-07 04:05:25") &  gall1$solar.time < ymd_hms("2025-10-27 03:59:25") ,  ] #start near 4 AM first day.  End one time step earlier on last day
# gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
# head(gall1)
# tail(gall1)
# length(gall1$oxy)

#Remove day column and re calculate
gall1 <- gall1 %>% select(-"day")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall1$day <- (as.numeric(factor(floor_date(gall1$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall1 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to check for NA's in the data! 
colSums(is.na(gall1)) #looks like just Q now......... 

#show where Q is NA
missingQ <- gall1 %>% filter(!complete.cases(.))

#Export the dataset so I can manually fill in Q, since it looks like I just need to drag that days value into the NA spots
write.csv(gall1, "FIXQ_Gall1.csv")

#Bring the dataset in. Wash, rinse, repeat
gall1 <- read.csv("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/1_Code/1_MetabolismEstimates_ARModel/MetabolismEstimates_ARModel/FIXQ_Gall1.csv")

#DO THE QAQC checks :)
colSums(is.na(gall1))

#OOOOH ADD IN THE OSAT
gall1$osat<- calc_DO_sat(temp.water=gall1$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))


#Then clean it up 
gall1 <- gall1 %>% select(-"X.1", -"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")

#Reorder the columns 
gall1 <- gall1 %>% select("day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "oxy_cor", "osat", "Q","solar.time","light",          
                          "Kc","percentsat_cor")

#Once more make sure each "day" has 144 obs
finaleobs_per_day <- gall1 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall1, "0_Gall_Site1Data_144obsperday.csv")
#DONE HERE

#############Site 2 QAQC########################################################
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

gall2<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_Gall2Data_144obsperday.csv")
lat<- 45.11
long<-  -111.2
#Remove UTC time columns, we are rebuilding them from aligned time
gall2 <- gall2 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall2$time_mt_aligned = as.POSIXct(gall2$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall2$time_utc <- with_tz(ymd_hms(gall2$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall2$unixtime <- as.numeric(gall2$time_utc)
#Remove solar time column before rebuilding
gall2<- gall2 %>% select(-"solar.time")
gall2 <- gall2 %>% select(-"light", -"Kc")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall2$solar.time<-convert_UTC_to_solartime(gall2$time_utc, longitude= long, time.type="mean solar")
gall2$light<- calc_light(gall2$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall2$Kc <- Kcor(gall2$temp,1)

#head(gall2)
#tail(gall2)

plot(gall2$solar.time,gall2$oxy, type="l")

# #gall1<- gall1[gall1$solar.time > ymd_hms("2025-05-07 04:05:25") &  gall1$solar.time < ymd_hms("2025-10-27 03:59:25") ,  ] #start near 4 AM first day.  End one time step earlier on last day
# gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
# head(gall1)
# tail(gall1)
# length(gall1$oxy)

#Remove day column and re calculate
gall2 <- gall2 %>% select(-"day")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall2$day <- (as.numeric(factor(floor_date(gall2$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall2 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to check for NA's in the data! 
colSums(is.na(gall2)) 

#DO THE QAQC checks :)
colSums(is.na(gall2))

#OOOOH ADD IN THE OSAT
gall2$osat<- calc_DO_sat(temp.water=gall2$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))


#Then clean it up 
gall2 <- gall2 %>% select(-"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")

#Reorder the columns 
gall2 <- gall2 %>% select("day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "oxy_cor", "osat", "Q","solar.time","light",          
                          "Kc","percentsat_cor")

#Once more make sure each "day" has 144 obs
finaleobs_per_day <- gall2 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall2, "0_Gall_Site2Data_144obsperday.csv")
#DONE HERE

#############Site 3 QAQC########################################################
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

gall3<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MetabolismEstimates_ARModel/0_Gall3Data_144obsperday.csv")
lat<- 45.25
long<-  -111.25
#Remove UTC time columns, we are rebuilding them from aligned time
gall3 <- gall3 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall3$time_mt_aligned = as.POSIXct(gall3$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall3$time_utc <- with_tz(ymd_hms(gall3$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall3$unixtime <- as.numeric(gall3$time_utc)
#Remove solar time column before rebuilding
gall3<- gall3 %>% select(-"solar.time")
gall3 <- gall3 %>% select(-"light", -"Kc")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall3$solar.time<-convert_UTC_to_solartime(gall3$time_utc, longitude= long, time.type="mean solar")
gall3$light<- calc_light(gall3$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall3$Kc <- Kcor(gall3$temp,1)

#head(gall2)
#tail(gall2)

plot(gall3$solar.time,gall3$oxy, type="l")

# #gall1<- gall1[gall1$solar.time > ymd_hms("2025-05-07 04:05:25") &  gall1$solar.time < ymd_hms("2025-10-27 03:59:25") ,  ] #start near 4 AM first day.  End one time step earlier on last day
# gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
# head(gall1)
# tail(gall1)
# length(gall1$oxy)

#Remove day column and re calculate
gall3 <- gall3 %>% select(-"day")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall3$day <- (as.numeric(factor(floor_date(gall3$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall3 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to check for NA's in the data! 
colSums(is.na(gall3)) 

#DO THE QAQC checks :)
colSums(is.na(gall3))

#OOOOH ADD IN THE OSAT
gall3$osat<- calc_DO_sat(temp.water=gall3$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))


#Then clean it up 
gall3 <- gall3 %>% select(-"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")

#Reorder the columns 
gall3 <- gall3 %>% select("day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "oxy_cor", "osat", "Q","solar.time","light",          
                          "Kc","percentsat_cor")

#Once more make sure each "day" has 144 obs
finaleobs_per_day <- gall3 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

lessthan144_site2 <-finaleobs_per_day %>% 
  filter(n_obs != 144)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall3, "0_Gall_Site3Data_144obsperday.csv")
#DONE HERE

#############Site 4 QAQC########################################################

Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall4 <- read.csv("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/1_Code/1_MetabolismEstimates_ARModel/MetabolismEstimates_ARModel/0_Gall_Interp_Only_site4.csv")
lat<- 45.30
long<-  -111.21
#Remove UTC time columns, we are rebuilding them from aligned time
gall4 <- gall4 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall4$time_mt_aligned = as.POSIXct(gall4$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall4$time_utc <- with_tz(ymd_hms(gall4$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall4$unixtime <- as.numeric(gall4$time_utc)
#Remove solar time column before rebuilding
gall4<- gall4 %>% select(-"solar.time")
gall4 <- gall4 %>% select(-"light", -"Kc")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall4$solar.time<-convert_UTC_to_solartime(gall4$time_utc, longitude= long, time.type="mean solar")
gall4$light<- calc_light(gall4$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall4$Kc <- Kcor(gall4$temp,1)

#head(gall4)
#tail(gall4)

plot(gall4$time_mt_aligned,gall4$oxy, type="l")

gall4<- gall4[gall4$solar.time > ymd_hms("2025-05-07 04:00:00") &  gall4$solar.time < ymd_hms("2025-10-27 04:00:00") ,  ] #start near 4 AM first day.  End one time step earlier on last day
# gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
# head(gall1)
# tail(gall1)
# length(gall1$oxy)


#Remove day column and re calculate
gall4 <- gall4 %>% select(-"day")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall4$day <- (as.numeric(factor(floor_date(gall4$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall4 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to check for NA's in the data! 
colSums(is.na(gall4)) 

#DO THE QAQC checks :)
#show where Q is NA
missingQ <- gall4 %>% filter(!complete.cases(.))

#OOOOH ADD IN THE OSAT
gall4$osat<- calc_DO_sat(temp.water=gall4$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))


#Then clean it up 
gall4 <- gall4 %>% select(-"X", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")

#Reorder the columns 
gall4 <- gall4 %>% select("day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "oxy_cor", "osat", "Q","solar.time","light",          
                          "Kc","percentsat_cor")

#Once more make sure each "day" has 144 obs
finaleobs_per_day <- gall4 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

lessthan144_site4 <-finaleobs_per_day %>% 
  filter(n_obs != 144)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall4, "0_Gall_Site4Data_144obsperday.csv")

#######Site 5#############################################################

gall5 <- read.csv("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/1_Code/1_MetabolismEstimates_ARModel/MetabolismEstimates_ARModel/0_Gall_Interp_Only_site5.csv")

head(gall5)
tail(gall5)

Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

lat<- 45.48
long<-  -111.27
#Remove UTC time columns, we are rebuilding them from aligned time
gall5 <- gall5 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall5$time_mt_aligned = as.POSIXct(gall5$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall5$time_utc <- with_tz(ymd_hms(gall5$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall5$unixtime <- as.numeric(gall5$time_utc)
#Remove solar time column before rebuilding
gall5<- gall5 %>% select(-"solar.time")
gall5 <- gall5 %>% select(-"light", -"Kc")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall5$solar.time<-convert_UTC_to_solartime(gall5$time_utc, longitude= long, time.type="mean solar")
gall5$light<- calc_light(gall5$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall5$Kc <- Kcor(gall5$temp,1)

#head(gall5)
#tail(gall5)

gall5<- gall5[gall5$solar.time > ymd_hms("2025-05-07 04:00:00") &  gall5$solar.time < ymd_hms("2025-10-27 04:00:00") ,  ] #start near 4 AM first day.  End one time step earlier on last day
# gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
# head(gall1)
# tail(gall1)
# length(gall1$oxy)

plot(gall5$solar.time,gall5$oxy, type="l")

#Remove day column and re calculate
gall5 <- gall5 %>% select(-"day")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall5$day <- (as.numeric(factor(floor_date(gall5$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall5 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to check for NA's in the data! 
colSums(is.na(gall5)) 

#DO THE QAQC checks :)
#show where Q is NA or anything else!
missingdata <- gall5 %>% filter(!complete.cases(.))

#OOOOH ADD IN THE OSAT
gall5$osat<- calc_DO_sat(temp.water=gall5$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))


#Then clean it up 
gall5 <- gall5 %>% select(-"X", -"X.1", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")
gall5 <- gall5 %>% select(-"percentsat", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")

#Reorder the columns 
gall5 <- gall5 %>% select("day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "oxy_cor", "osat", "Q","solar.time","light",          
                          "Kc","percentsat_cor")

#Once more make sure each "day" has 144 obs
finaleobs_per_day <- gall5 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

lessthan144_site5 <-finaleobs_per_day %>% 
  filter(n_obs != 144)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall5, "0_Gall_Site5Data_144obsperday.csv")

#######Site 6#############################################################

gall6 <- read.csv("C:/Users/Cora.Steinbach/OneDrive - The University of Montana (1)/Documents/Graduate School_UniversityofMontana/MASTERS PROJECT/1_Code/1_MetabolismEstimates_ARModel/MetabolismEstimates_ARModel/0_Gall_Interp_Only_site6.csv")

head(gall6)
tail(gall6)

Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

lat<- 45.55
long<-  -111.23
#Remove UTC time columns, we are rebuilding them from aligned time
gall6 <- gall6 %>% select(-"unixtime", -"time_utc")
#convert mt time into POSIXct object
gall6$time_mt_aligned = as.POSIXct(gall6$time_mt_aligned, format= "%m/%d/%Y %H:%M", tz= "America/Denver")
#My time conversion, since utc time switches time anchors within a day. I have already aligned time using MDT. converted directly from unix long ago
gall6$time_utc <- with_tz(ymd_hms(gall6$time_mt_aligned, tz = "America/Denver"), "UTC")
#Build unix time from UTC time
gall6$unixtime <- as.numeric(gall6$time_utc)
#Remove solar time column before rebuilding
gall6<- gall6 %>% select(-"solar.time")
gall6 <- gall6 %>% select(-"light", -"Kc")
##gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")
##gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall6$solar.time<-convert_UTC_to_solartime(gall6$time_utc, longitude= long, time.type="mean solar")
gall6$light<- calc_light(gall6$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall6$Kc <- Kcor(gall6$temp,1)

#head(gall5)
#tail(gall5)

gall6 <- gall6[gall6$solar.time > ymd_hms("2025-05-06 04:00:00") &  gall6$solar.time < ymd_hms("2025-10-27 04:00:00") ,  ] #start near 4 AM first day.  End one time step earlier on last day
# gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
# head(gall1)
# tail(gall1)
# length(gall1$oxy)

plot(gall6$solar.time,gall6$oxy, type="l")

#Remove day column and re calculate
gall6 <- gall6 %>% select(-"day")
##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall6$day <- (as.numeric(factor(floor_date(gall6$solar.time-3600*4, unit = "day"))))

##########Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall6 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to check for NA's in the data! 
colSums(is.na(gall6)) 

#DO THE QAQC checks :)
#show where Q is NA or anything else!
missingdata <- gall6 %>% filter(!complete.cases(.))

#OOOOH ADD IN THE OSAT
gall6$osat<- calc_DO_sat(temp.water=gall6$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))


#Then clean it up 
gall6 <- gall6 %>% select(-"X", -"X.1", -"percentsat", "day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "percentsat_cor", "oxy_cor","Q","solar.time","light",          
                          "Kc","osat")

#Reorder the columns 
gall6 <- gall6 %>% select("day", "unixtime", "time_mt_aligned", "time_utc",       
                          "sensor", "site_id", "temp", "oxy_cor", "osat", "Q","solar.time","light",          
                          "Kc","percentsat_cor")

#Once more make sure each "day" has 144 obs
finaleobs_per_day <- gall6 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

lessthan144_site6 <-finaleobs_per_day %>% 
  filter(n_obs != 144)

#Save final csv with 144 obs per day as a csv & put in Dropbox for Bob
write.csv(gall6, "0_Gall_Site6Data_144obsperday.csv")
