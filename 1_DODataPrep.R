#Manipulating data so it meets the requirements for the AR model 
#Requirements:
#A day "begins" on the first measurement after 04:00 solar.time, see my code below.  It ends on the last measurement before 04:00 the following day
#Each day needs exactly 144 estimates.  So interpolate if missing a few.
#If missing a lot delete the entire day from just after 4 AM to just before 4 AM the day after.  You know the data will be right when you take the length of your enitire time series and divide by 144 and you get an integer number of days.

library(tidyverse)
library(lubridate)
library(streamMetabolizer)
library(deSolve)


Kcor<-function (temp,K600) {
  K600/(600/(1800.6-(temp*120.1)+(3.7818*temp^2)-(0.047608*temp^3)))^-0.5
}
bpcalc_atm<- function(bpst, alt) {
  bpst*exp((-9.80665*0.0289644*alt)/(8.31447*(273.15+15)))
}

gall<- read.csv("C:/Users/Cora.Steinbach/Dropbox/Cora/GallatinRiverData/MeabolismEstimates_ARModel/0_DOData_AllSites_AllMonths_withQ_Gallatin2025.csv")
lat<- 45.36
long<-  -111.2
#My time conversion, since utc time was converted directly from unix long ago
gall$time_utc <- as.POSIXct(gall$time_utc, format = "%m/%d/%Y %H:%M", tz = "UTC")

gall$time<-as_datetime(gall$unixtime) #converts unit time to a R time object in UTC

gall$solar.time<-convert_UTC_to_solartime(gall$time_utc, longitude= long, time.type="mean solar")
gall$light<- calc_light(gall$solar.time, latitude=lat, longitude=long, max.PAR =2326, attach.units = F)
gall$Kc <- Kcor(gall$temp,1)



######Site 1

gall1<- gall[gall$site_id==1,]

head(gall1)
tail(gall1)

plot(gall1$solar.time,gall1$oxy, type="l")

gall1<- gall1[gall1$solar.time > ymd_hms("2025-05-07 04:09:00") &  gall1$solar.time < ymd_hms("2025-10-27 03:53:00") ,  ] #start near 4 AM first day.  End one time step earlier on last day
gall1<-gall1[!is.na(gall1$oxy),]  ##I want to delete this line so that there are no NAs in the data.
head(gall1)
tail(gall1)
length(gall1$oxy)

##code below puts a numeric on each day starting at 4 am.  I would like an award for figuring out how to do this.
gall1$day <- (as.numeric(factor(floor_date(gall1$solar.time-3600*4, unit = "day"))))

#Cora code starts
#code to count number of obs per day (using their integer values)
obs_per_day <- gall1 %>%
  group_by(day) %>%
  summarise(n_obs = n()) %>%
  ungroup()

#code to seperate only the # of days without 144 obs
lessthan144_site1 <-obs_per_day %>% 
  filter(n_obs != 144)

#function to drop days with > 113 obs. (this would be gaps of more than 3 hours)
drop_overfilled_days <- function(df, day_col = "day", threshold = 113) {
  df %>%
    group_by(.data[[day_col]]) %>%
    filter(n() <= threshold) %>%
    ungroup()
}

#then run function on the dataset, dropped only 2 days from site 1
gall1_removeddays <- drop_overfilled_days(gall1, day_col = "day", threshold = 113)

#now need to interpolate obs for days with less than 144 days
#Bob code starts again 
gall1$osat<- calc_DO_sat(temp.water=gall1$temp, pressure.air=bpcalc_atm(bpst=1013, alt=2020))

gall1sum<-gall1 %>% group_by(day) %>% summarise(sumlight=sum(light), logq=log(mean(Q)))


gall1_metab_list<- list(T=length(gall1$solar.time), D=length(gall1$solar.time)/144,
                        y=gall1$oxy_cor, light=gall1$light,oxysat=gall1$osat,day=gall1$day,
                        Kc=gall1$Kc,sumlight=gall1sum$sumlight, logQ=gall1sum$logq, z=1, ts=1/144)
