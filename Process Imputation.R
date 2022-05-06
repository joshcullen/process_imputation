
### Process Imputation of Time Series ###

library(tidyverse)
library(lubridate)
library(rgdal)
library(crawl)
library(sf)
library(furrr)
library(ggspatial)

source('helper functions.R')


dat<- read.csv("Snail Kite Gridded Data_large.csv", as.is = T)
dat$ESTtime<- as_datetime(dat$ESTtime)
dat_red<- dat %>% dplyr::select(id, ESTtime, utmlong, utmlat) #%>% filter(id != 28)


dat_red <- dat_red %>%
  mutate(
    error_semi_major_axis = 30,
    error_semi_minor_axis = 30,
    error_ellipse_orientation = 0
  )

sf_locs <- st_as_sf(dat_red, coords = c("utmlong","utmlat")) %>% st_set_crs(32617)

future::plan(multisession)
sf_locs <- sf_locs %>% 
  dplyr::group_by(id) %>% dplyr::arrange(ESTtime) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = furrr::future_map(data,sf::st_as_sf))




#############################################################################
### STAGE 1: Ornstein-Uhlenbeck Approximate Imputation Distribution (AID) ###
#############################################################################

#set initial values for model
initial<- map(sf_locs[[2]], init_params)


# Set first val of 'fixpar' to 1 since we are providing error info; second and third are set to NA (for sigma and beta, respectively) to be estimated from model; sigma = velocity variation; beta = velocity autocorr
sf_locs <- sf_locs %>% 
  dplyr::mutate(fixpar = list(c(1,NA,NA)))


# Run crawl model on all IDs
dat_fit <- sf_locs %>% 
  dplyr::mutate(fit = furrr::future_pmap(list(d = data,fixpar = fixpar),
                                         fit_crawl, .progress = TRUE),
                params = map(fit, crawl::tidy_crwFit))



## Draw from AID

K <- 20  #number of draws

#includes observed and predicted locs
dat_fit <- dat_fit %>% 
  dplyr::mutate(sim_tracks = furrr::future_map(fit, .get_sim_tracks, iter = K, .progress = TRUE))

#filtering for only predicted locs and creating sf objects
dat_fit$sim_lines <- crawl::crw_as_sf(dat_fit$sim_tracks, ftype = "MULTILINESTRING", locType = "p")



## Plot

sf_sim_lines <- do.call(rbind,dat_fit$sim_lines) %>% 
  mutate(id = dat_fit$id)

esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() + 
  annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
  layer_spatial(sf_sim_lines[1,], size = 0.25, alpha = 0.1, aes(color = as.factor(id))) +
  # geom_point(data = dat %>% filter(id == 1), aes(utmlong, utmlat), color = "grey45") +
  # facet_wrap(~id, nrow = 2) +
  scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
  scale_y_continuous(expand = expansion(mult = c(0.35, 0.35))) +
  scale_color_viridis_d() +
  theme(legend.position = "none") +
  ggtitle("Simulated Location Paths", 
          subtitle = "snail kites (n=4), Florida, USA")



#Plot lines with original data points
ggplot(data = sf_sim_lines) +
  geom_sf(aes(color = as.factor(id)), size = 0.25, alpha = 1, show.legend = "line") +
  geom_point(data = dat, aes(utmlong, utmlat), color = "grey45", alpha = 0.6) +
  theme_bw() +
  facet_wrap(~id) +
  scale_color_discrete("ID", guide = guide_legend(override.aes = list(size = 1))) +
  labs(x="Longitude", y="Latitude")




## Extract coords from simulations

# Create list of mean locations by ID from imputed locs only
sim_tracks<- modify_depth(dat_fit$sim_tracks, 2,
                          function(x) cbind(x$alpha.sim[x$locType == "p", c("mu.x","mu.y")],
                                            time = x$ESTtime[x$locType == "p"])) %>% 
  lapply(., function(x) do.call(cbind, x)) %>%
  lapply(., function(x) cbind(mu.x = apply(x[,which(colnames(x) == "mu.x")], 1, mean),
                              mu.y = apply(x[,which(colnames(x) == "mu.y")], 1, mean),
                              sd.x = apply(x[,which(colnames(x) == "mu.x")], 1, sd),
                              sd.y = apply(x[,which(colnames(x) == "mu.y")], 1, sd),
                              time = x[,"time"])) %>%
  map(., data.frame) %>% 
  set_names(dat_fit$id)

#change from numeric to POSIXct
sim_tracks<- lapply(sim_tracks, function(x){x$time <- intToPOSIX(x$time, tz = "UTC"); x})

#make single DF and add 'id' column
id.vec<- rep(names(sim_tracks), lapply(sim_tracks, nrow) %>% unlist())
sim_tracks_df<- map_dfr(sim_tracks, `[`) %>% cbind(id = id.vec, .)


ggplot(data = sf_sim_lines) +
  geom_sf(aes(color = as.factor(id)), size = 0.25, alpha = 1, show.legend = "line") +
  geom_point(data = sim_tracks_df, aes(mu.x, mu.y), color = "grey80", alpha = 0.6, size = 1) +
  geom_point(data = dat, aes(utmlong, utmlat), color = "grey45", alpha = 0.6) +
  theme_bw() +
  facet_wrap(~id) +
  scale_color_discrete("ID", guide = guide_legend(override.aes = list(size = 1))) +
  labs(x="Longitude", y="Latitude")




### ADJUST COLORS, TRANSPARENCIES, AND ORDER OF LAYERS
## Dynamic plot
library(mapview)

dat_pts<- st_as_sf(dat, coords = c("utmlong","utmlat")) %>% st_set_crs(32617)
sim_pts<- st_as_sf(sim_tracks_df, coords = c("mu.x","mu.y")) %>% st_set_crs(32617)

mapview(list(sf_sim_lines, sim_pts, dat_pts), map.types = c("Esri.WorldImagery"))
