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
tot.obs<- dat %>% group_by(id) %>% count();  tot.obs$id<- as.character(tot.obs$id)
hr_1<- test.dat %>% group_by(id) %>% filter(dt == 3600) %>% count(); hr_1$id<- as.character(hr_1$id)
hr_2<- test.dat %>% group_by(id) %>% filter(dt == 7200) %>% count(); hr_2$id<- as.character(hr_2$id)
hr_12<- test.dat %>% group_by(id) %>% filter(dt %% 3600 == 0 & dt <= 12*3600) %>% count()
hr_12$id<- as.character(hr_12$id)
hr_24<- test.dat %>% group_by(id) %>% filter(dt %% 3600 == 0 & dt <= 24*3600) %>% count()
hr_24$id<- as.character(hr_24$id)


summ.tab<- tot.obs %>% left_join(hr_1, by = "id") %>% left_join(hr_2, by = "id") %>%
  left_join(hr_12, by = "id") %>% left_join(hr_24, by = "id")

names(summ.tab)<- c("id","N","hr_1","hr_2","hr_12","hr_24")
summ.tab<- as.matrix(summ.tab)
summ.tab[is.na(summ.tab)]<- 0
summ.tab<- data.frame(summ.tab) %>% mutate_all(function(x) as.numeric(as.character(x)))
summ.tab$hr_2<- summ.tab$hr_2*2 + summ.tab$hr_1

#Make proportional to N per ID
summ.tab2<- matrix(NA,nrow(summ.tab), ncol(summ.tab)-1)

for (i in 1:nrow(summ.tab)) {
  summ.tab2[i,]<- as.numeric(summ.tab[i,-1]/summ.tab[i,2])  
}
summ.tab2<- cbind(id = summ.tab$id, summ.tab2) %>% data.frame()
colnames(summ.tab2)<- colnames(summ.tab)

summ.tab.long<- summ.tab2 %>% gather(key, value, -id)
summ.tab.long$id<- as.factor(summ.tab.long$id)
summ.tab.long$key<- factor(summ.tab.long$key, levels = c("hr_1","hr_2","hr_12","hr_24","N"))


### Plot
ggplot(summ.tab.long, aes(x=key, y=value, color = id)) +
  geom_hline(aes(yintercept = 1), size = 0.5, linetype = 2) +
  geom_point(size = 3) +
  theme_bw() +
  labs(x="\nMax Time Interval Included", y="Proportion of Total Observations\n",
       title = "Observed Data") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  guides(color = guide_legend("ID")) + 
  scale_x_discrete(labels = c("1 hr","2 hrs","12 hrs","24 hrs","N"))














#Calculate the percentage of imputed data from whole dataset by max time interval (1, 2, 12, 24 hrs)

hr_1<- tot.obs; hr_1$n<- 0; hr_1$id<- as.character(hr_1$id)
hr_2<- test.dat %>% group_by(id) %>% filter(dt == 7200) %>% count(); hr_2$id<- as.character(hr_2$id)

grtr.12h <- function(dt) {  #cut traj when gap larger than 12 hrs
  return(dt > (1*3600*12))
}
dat.traj12h<- cutltraj(dat.traj, "grtr.12h(dt)", nextr = TRUE)
dat.traj12h
dat.traj12h_NA<- setNA(dat.traj12h, refda, 1, units = "hour")
dat.traj12h_round<- round_track_time(dat = dat.traj12h_NA, int = 3600, tol = 5/60*3600)
dat.traj12h_round<- ld(dat.traj12h_round)

hr_12<- dat.traj12h_round %>% group_by(id) %>% filter(is.na(x)) %>% count()
hr_12$id<- as.character(hr_12$id)

grtr.1d <- function(dt) {  #cut traj when gap larger than 1 day
  return(dt > (1*3600*24))
}

dat.traj1d<- cutltraj(dat.traj, "grtr.1d(dt)", nextr = TRUE)
dat.traj1d
dat.traj1d_NA<- setNA(dat.traj1d, refda, 1, units = "hour")
dat.traj1d_round<- round_track_time(dat = dat.traj1d_NA, int = 3600, tol = 5/60*3600)
dat.traj1d_round<- ld(dat.traj1d_round)

hr_24<- dat.traj1d_round %>% group_by(id) %>% filter(is.na(x)) %>% count()
hr_24$id<- as.character(hr_24$id)


summ.tab_imp<- tot.obs %>% left_join(hr_1, by = "id") %>% left_join(hr_2, by = "id") %>%
  left_join(hr_12, by = "id") %>% left_join(hr_24, by = "id")

names(summ.tab_imp)<- c("id","N","hr_1","hr_2","hr_12","hr_24")
summ.tab_imp<- as.matrix(summ.tab_imp)
summ.tab_imp[is.na(summ.tab_imp)]<- 0
summ.tab_imp<- data.frame(summ.tab_imp) %>% mutate_all(function(x) as.numeric(as.character(x)))

#Make proportional to N per ID
summ.tab_imp2<- matrix(NA,nrow(summ.tab_imp), ncol(summ.tab_imp)-1)

for (i in 1:nrow(summ.tab_imp)) {
  summ.tab_imp2[i,]<- as.numeric(summ.tab_imp[i,-1]/summ.tab_imp[i,2])
}
summ.tab_imp2<- cbind(id = summ.tab_imp$id, summ.tab_imp2) %>% data.frame()
colnames(summ.tab_imp2)<- colnames(summ.tab_imp)

summ.tab_imp.long<- summ.tab_imp2 %>% gather(key, value, -id)
summ.tab_imp.long$id<- as.factor(summ.tab_imp.long$id)
summ.tab_imp.long$key<- factor(summ.tab_imp.long$key, levels = c("hr_1","hr_2","hr_12","hr_24","N"))


### Plot
ggplot(summ.tab_imp.long, aes(x=key, y=value, color = id)) +
  geom_hline(aes(yintercept = 1), size = 0.5, linetype = 2) +
  geom_point(size = 3) +
  theme_bw() +
  labs(x="\nMax Time Interval Included", y="Proportion of Total Observations\n",
       title = "Hourly Imputed Data") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  guides(color = guide_legend("ID")) + 
  scale_x_discrete(labels = c("1 hr","2 hrs","12 hrs","24 hrs","N"))








### Comparing only observed to imputed data

#Determine N per max time interval (obs + imputations)
summ.tab_full<- summ.tab[,3:6] + summ.tab_imp[,3:6]
summ.tab[,3:6]<- summ.tab[,3:6]/summ.tab_full
summ.tab<- summ.tab[,-2]


#Make proportional to N per ID
summ.tab2<- matrix(NA,nrow(summ.tab), ncol(summ.tab)-1)

for (i in 1:nrow(summ.tab)) {
  summ.tab2[i,]<- as.numeric(summ.tab[i,-1]/summ.tab[i,2])  
}
summ.tab2<- cbind(id = summ.tab$id, summ.tab2) %>% data.frame()
colnames(summ.tab2)<- colnames(summ.tab)

summ.tab.long<- summ.tab2 %>% gather(key, value, -id)
summ.tab.long$id<- as.factor(summ.tab.long$id)
summ.tab.long$key<- factor(summ.tab.long$key, levels = c("hr_1","hr_2","hr_12","hr_24"))


### Plot
ggplot(summ.tab.long, aes(x=key, y=value, color = id)) +
  geom_hline(aes(yintercept = 1), size = 0.5, linetype = 2) +
  geom_point(size = 3) +
  theme_bw() +
  labs(x="\nMax Time Interval Included", y="Proportion of Dataset\n",
       title = "Observations") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  guides(color = guide_legend("ID")) + 
  scale_x_discrete(labels = c("1 hr","2 hrs","12 hrs","24 hrs"))



ggplot(summ.tab.long, aes(x=key, y=1-value, color = id)) +
  geom_hline(aes(yintercept = 1), size = 0.5, linetype = 2) +
  geom_point(size = 3) +
  theme_bw() +
  labs(x="\nMax Time Interval Included", y="Proportion of Dataset\n",
       title = "Imputations") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  guides(color = guide_legend("ID")) + 
  scale_x_discrete(labels = c("1 hr","2 hrs","12 hrs","24 hrs"))





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
