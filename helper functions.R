#define initial values for loc ('a') and 4x4 covar matrix of error ('P')
init_params <- function(d) {
  if (any(colnames(d) == "x") && any(colnames(d) == "y")) {
    ret <- list(a = c(d$x[1], 0,
                      d$y[1], 0),
                P = diag(c(10 ^ 2, 10 ^ 2,
                           10 ^ 2, 10 ^ 2)))
  } else if (inherits(d,"sf")) {
    ret <- list(a = c(sf::st_coordinates(d)[[1,1]], 0,
                      sf::st_coordinates(d)[[1,2]], 0))
  }
  ret
}
#--------------------------------------------------------
#wrapper function to fit model for each element of list
fit_crawl <- function(d, fixpar) {
  set.seed(1)
  
  fit <- crawl::crwMLE(
    mov.model =  ~ 1,
    err.model = list(x = ~1),
    if (any(colnames(d) == "activity")) {
      activity <- ~ I(activity)
    } else {activity <- NULL},
    fixPar = fixpar,
    data = d,
    method = "Nelder-Mead",
    Time.name = "ESTtime",
    initial.state = initial,
    attempts = 8,
    control = list(
      trace = 0
    ),
    initialSANN = list(
      maxit = 1500,
      trace = 0
    )
  )
  fit
}
#--------------------------------------------------------
#wrapper function for crwSimulator() and crwPostIS
.get_sim_tracks <- function(crw_fit,iter) {
  set.seed(1)
  
  simObj <- crw_fit %>% crawl::crwSimulator(predTime = '1 hour')
  
  sim_tracks = list()
  for (i in 1:iter) {
    sim_tracks[[i]] <- crawl::crwPostIS(simObj, fullPost = TRUE)
  }
  return(sim_tracks)
}