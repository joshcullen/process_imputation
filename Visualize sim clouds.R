## ID 19
dat19<- dat %>% filter(id == 19)
dat19_Ldt<- dat19 %>% mutate(dt = c(diff(ESTtime) %>% as.numeric(), NA)) #%>% filter(dt > 899)
ind<- which(dat19_Ldt$dt > 899)
dat19_Ldt<- dat19_Ldt[c(ind, ind+1),]
dat19_Ldt<- dat19_Ldt[order(dat19_Ldt$ESTtime),]
sim_tracks19<- sim_tracks_df %>% filter(id == 19)



ggplot(data = sim_tracks19, aes(x=mu.x,y=mu.y,color=sd.x)) +
  geom_path(size = 0.5) +
  geom_point(data=dat19_Ldt, aes(x=utmlong, y=utmlat), color = "red", size = 1) +
  scale_color_viridis_c(option = "magma") +
  coord_equal()

ggplot(data = sim_tracks19, aes(x=mu.x,y=mu.y,color=sd.y)) +
  geom_path(size = 0.5) +
  geom_point(data=dat19_Ldt, aes(x=utmlong, y=utmlat), color = "red", size = 1) +
  coord_equal() +
  scale_color_viridis_c(option = "magma") +
  theme_bw()





## ID 23
dat23<- dat %>% filter(id == 23)
dat23_Ldt<- dat23 %>% mutate(dt = c(diff(ESTtime) %>% as.numeric(), NA)) #%>% filter(dt > 899)
ind<- which(dat23_Ldt$dt > 899)
dat23_Ldt<- dat23_Ldt[c(ind, ind+1),]
dat23_Ldt<- dat23_Ldt[order(dat23_Ldt$ESTtime),]
sim_tracks23<- sim_tracks_df %>% filter(id == 23)



ggplot(data = sim_tracks23, aes(x=mu.x,y=mu.y,color=sd.x)) +
  geom_path(size = 0.5) +
  geom_point(data=dat23_Ldt, aes(x=utmlong, y=utmlat), color = "red", size = 1) +
  scale_color_viridis_c(option = "magma") +
  coord_equal() +
  theme_bw()

ggplot(data = sim_tracks23, aes(x=mu.x,y=mu.y,color=sd.y)) +
  geom_path(size = 0.5) +
  geom_point(data=dat23_Ldt, aes(x=utmlong, y=utmlat), color = "red", size = 1) +
  coord_equal() +
  scale_color_viridis_c(option = "magma") +
  theme_bw()







## ID 1
dat1<- dat %>% filter(id == 1)
dat1_Ldt<- dat1 %>% mutate(dt = c(diff(ESTtime) %>% as.numeric(), NA)) #%>% filter(dt > 899)
ind<- which(dat1_Ldt$dt > 899)
dat1_Ldt<- dat1_Ldt[c(ind, ind+1),]
dat1_Ldt<- dat1_Ldt[order(dat1_Ldt$ESTtime),]
sim_tracks1<- sim_tracks_df %>% filter(id == 1)



ggplot(data = sim_tracks1, aes(x=mu.x,y=mu.y,color=sd.x)) +
  geom_path(size = 0.5) +
  geom_point(data=dat1_Ldt, aes(x=utmlong, y=utmlat), color = "red", size = 1) +
  scale_color_viridis_c(option = "magma") +
  coord_equal() +
  theme_bw()

ggplot(data = sim_tracks1, aes(x=mu.x,y=mu.y,color=sd.y)) +
  geom_path(size = 0.5) +
  geom_point(data=dat1_Ldt, aes(x=utmlong, y=utmlat), color = "red", size = 1) +
  coord_equal() +
  scale_color_viridis_c(option = "magma") +
  theme_bw()
