library(tidyverse)
library(lubridate)
library(adehabitatLT)

dat<- read.csv("Snail Kite Gridded Data_large.csv", as.is = T)
dat$ESTtime<- as_datetime(dat$ESTtime)


dat.spdf<- dat
coordinates(dat.spdf)<- ~utmlong + utmlat
proj4string(dat.spdf)<- CRS("+init=epsg:32617")
dat.traj<- as.ltraj(xy = coordinates(dat.spdf), date = dat.spdf$ESTtime, id = dat.spdf$id)


#Fill in time gaps with NA vals (based on 1 h) and round w/in 5 min

refda<- dat[1,"ESTtime"]  #set reference time
dat.trajNA<- setNA(dat.traj, refda, 1, units = "hour")
dat.traj_round<- round_track_time(dat = dat.trajNA, int = 3600, tol = 5/60*3600)

#Remove NAs

dat.traj_round2<- na.omit.ltraj(dat.traj_round)



#Calculate the # of obs usable by max time interval (1, 2, 12, 24 hrs)

test.dat<- ld(dat.traj_round2)
tot.obs<- ld(dat.traj_round) %>% nrow()
hr_1<- test.dat %>% filter(dt == 3600) %>% count() %>% .$n
hr_2<- test.dat %>% filter(dt == 7200) %>% count() %>% .$n

no_impute<- data.frame(NA_obs = (tot.obs - hr_1)/tot.obs, Orig_obs = hr_1/tot.obs, Impute_obs = 0, Impute = "Not Imputed")
impute<- data.frame(NA_obs = (tot.obs - hr_1 - hr_2)/tot.obs, Orig_obs = (hr_1+hr_2)/tot.obs, Impute_obs = hr_2/tot.obs, Impute = "Imputed")

summ.tab<- rbind(no_impute, impute) %>% gather(key, value, -Impute)
summ.tab$key<- factor(summ.tab$key, levels = c("NA_obs","Impute_obs","Orig_obs"))


ggplot() +
  geom_bar(data = summ.tab, aes(x=Impute, y=value, fill = key), stat = "identity",
           position = "stack") +
  scale_fill_viridis_d("Type of Data", labels = c("NA", "Imputed", "Original")) +
  labs(y = "Proportion of Regularized Dataset\n") +
  theme_bw()






#---------------------------------









round_track_time=function(dat, int, tol) {  #replacement for sett0() when wanting to only round some of the times
  
  for (i in 1:length(dat)) {
    tmp=matrix(NA,nrow(dat[[i]]),2)
    
    for (j in 1:nrow(dat[[i]])) {
      if (is.na(dat[[i]]$dt[j])) {
        tmp[j, 1:2]<- NA
      } else if (dat[[i]]$dt[j] > (int - tol) & dat[[i]]$dt[j] < (int + tol)) {
        tmp[j, 1:2]<- c(int, as.numeric(round(dat[[i]]$date[j], units = "hours")))
      } else {
        tmp[j, 1:2]<- c(dat[[i]]$dt[j], dat[[i]]$date[j])
      }
    }
    dat[[i]]$dt<- tmp[,1]
    dat[[i]]$date<- tmp[,2] %>% as.POSIXct(origin = '1970-01-01', tz = "UTC")
  }
  dat
}
