#################################################################################################
#### Vignette provided by Scharf, H., Hooten, M.B. & Johnson, D.S. JABES (2017) 22: 335-352. ####
#### https://doi.org/10.1007/s13253-017-0294-5                                               ####
#################################################################################################


library(rgdal); library(crawl); library(twosmove); library(smover); library(splines)
data("nfs_vignette", package = "twosmove")
head(nfs.vignette)


## location of rookery ----
c.rookery <- data.frame("lon" = -170.267, "lat" = 57.134)
## projecting ----
coordinates(nfs.vignette) <- ~lon+lat
proj4string(nfs.vignette) <- CRS("+proj=longlat")
nfs.proj <- spTransform(nfs.vignette, CRS(paste("+proj=aea +lat_1=54 +lat_2=58",
                                                "+lat_0=56 +lon_0=-170 +x_0=0 +y_0=0",
                                                "+ellps=GRS80 +datum=NAD83",
                                                "+units=km +no_defs")))
coordinates(c.rookery) <- ~lon+lat
proj4string(c.rookery) <- CRS("+proj=longlat")
c.rookery.proj <- spTransform(c.rookery, CRS(paste("+proj=aea +lat_1=54 +lat_2=58",
                                                   "+lat_0=56 +lon_0=-170 +x_0=0 +y_0=0",
                                                   "+ellps=GRS80 +datum=NAD83",
                                                   "+units=km +no_defs")))


#############################################################################
### STAGE 1: Ornstein-Uhlenbeck Approximate Imputation Distribution (AID) ###
#############################################################################

## change times to numeric, hourly values ----
unit.times <- as.numeric(as.POSIXct(nfs.vignette$time))/3600
nfs.proj$unit.times <- unit.times
## crawl fit ----
speed <- sqrt((diff(nfs.proj@coords[, 1])/diff(nfs.proj$unit.times))^2 +
                (diff(nfs.proj@coords[, 2])/diff(nfs.proj$unit.times))^2)
initial <- list(a = c(coordinates(nfs.proj)[1,1], 0,
                      coordinates(nfs.proj)[1,2], 0),
                P = diag(c(1e2,
                           var(speed),
                           1e2,
                           var(speed))))
fixPar = c(NA, NA, NA)
error_model <- list(x = ~1)
displayPar(mov.model = ~1, err.model = error_model,
           data = data.frame("y" = nfs.vignette$lat,
                             "x" = nfs.vignette$lon,
                             "times" = nfs.proj$unit.times),
           fixPar = fixPar)



pred.times <- c(seq(nfs.proj$unit.times[1], nfs.proj$unit.times[410], by = 2),
                seq(nfs.proj$unit.times[411], nfs.proj$unit.times[865], by = 2),
                seq(nfs.proj$unit.times[866], nfs.proj$unit.times[1340], by = 2),
                seq(nfs.proj$unit.times[1341], nfs.proj$unit.times[nrow(nfs.proj)], by = 2))
fit1.crawl <- crwMLE(mov.model = ~1, err.model = list(x = ~1),
                     data = data.frame("y" = nfs.proj$lon,
                                       "x" = nfs.proj$lat,
                                       "times" = nfs.proj$unit.times),
                     coord = c("y", "x"), Time.name = "times",
                     initial.state = initial, fixPar = fixPar)



## simulate from AID ----
K.crawl <- 128
simObj <- crwSimulator(object.crwFit = fit1.crawl, predTime = pred.times)
sigsq.s.draw <- rep(NA, K.crawl)
mu.star <- vector("list", K.crawl)
for(i in 1:K.crawl){
  samp <- crwPostIS(simObj, fullPost = T)
  mu.star[[i]] <- cbind(samp$alpha.sim[samp$locType == "p" ,'mu.x'],
                        samp$alpha.sim[samp$locType == "p" ,'mu.y'])
  sigsq.s.draw[i] <- samp$par[1]
}

## make object of class "fit1_crawl" and plot it with generic function from `twosmove` ----
out <- list("K.crawl" = K.crawl, "interp.times" = pred.times,
            "basis.func" = "crawl", "mu.star" = mu.star,
            s = nfs.proj@coords, "observation.times" = nfs.proj$unit.times,
            "fit1.crawl" = fit1.crawl)
class(out) <- "fit1_crawl"
plot(out)




##################################
### STAGE 2: SDE Process Model ###
##################################


## fit.sde [WARNING: SLOW for large `N.iterations`] ----
N.iterations <- 2e2
H <- function(x, beta){
  beta * sqrt(sum((x - c.rookery.proj@coords)^2))
}
N.knots.alpha <- 2^6 - 1
priors <- list("sigsq.v" = c(-1, 0), "sigsq.alpha" = c(1, 1e9))
starting.values <- list("sigsq.alpha" = 10^(-2.5), "sigsq.v" = 2e-2,
                        "alpha" = rnorm(N.knots.alpha))
tuning <- list("sigsq.v" = starting.values$sigsq.v)
fixed <- list("sigsq.v" = F, "sigsq.alpha" = T, "alpha" = F, "beta" = F)
W.alpha <- bs(pred.times[-c(length(pred.times))], df = N.knots.alpha, intercept = F)
penalty <- "ridge"
fit.nfs <- fit.sde(mu.star = mu.star, priors = priors, interp.times = pred.times,
                   tuning = tuning, starting.values = starting.values,
                   fixed = fixed, potential.func = H, init = nfs.proj@coords[1, ],
                   N.iterations = N.iterations, W.alpha = W.alpha, penalty = penalty,
                   batch.size = min(30, N.iterations))

## burnin ----
burnin <- 1
burnin <- 1:floor(N.iterations*0.5)
if(!is.null(burnin)){
  used.iterations <- (1:N.iterations)[-burnin]
} else {
  used.iterations <- 1:N.iterations
}


## check convergence ----
library(coda)
first.half <- used.iterations[1:(length(used.iterations)/2)]
second.half <- used.iterations[(length(used.iterations)/2 + 1):
                                 (length(used.iterations)/2 + length(first.half))]
gelman.diag(x = list(mcmc(unlist(fit.nfs$sigsq.v.save)[first.half]),
                     mcmc(unlist(fit.nfs$sigsq.v.save)[second.half])))


quantile(unlist(fit.nfs$sigsq.v.save)[used.iterations], probs = c(0.5, 0.025, 0.975))
exp(fit1.crawl$par)[1]
exp(fit1.crawl$ci[1, ])
