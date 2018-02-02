rm(list=ls())
library(dplyr); library(R2jags)


set.seed(493)

load(file="../rdatafiles/Xtot.RData")
N <- nrow(Xtot)
testrows <- sample(x=1:N, size=floor(0.2*N), replace=FALSE)
Xtrain <- Xtot[-testrows,] %>% arrange(time)
Xtest <- Xtot[testrows,] %>% arrange(time)


id4 <- 601140 
id3 <- 603106 
id2 <- 842301 
id1 <- 887665 
playerseasons <- matrix(
  c(id1,   2016,
    id2,   2015,
    id3,   2014,
    id4,   2015
  ),ncol=2,byrow = TRUE
)
colnames(playerseasons) <- c("globalplayerid" ,"season")

playermap <- data.frame(
  factorid = as.integer(as.factor(Xtot$globalplayerid)),
  globalplayerid = Xtot$globalplayerid
) %>% unique()
rownames(playermap) <- NULL

gamemap <- data.frame(
  factorid = as.integer(as.factor(Xtot$gameid)),
  gameid   = Xtot$gameid
) %>% unique()
rownames(gamemap) <- NULL

priormod <- glm(result ~ log(r) + theta, data=Xtrain %>% filter(as.integer(as.factor(gameid)) < 5), family="binomial")
mu0r <- summary(priormod)[["coefficients"]]["log(r)","Estimate"]
mu0theta <- summary(priormod)[["coefficients"]]["log(r)","Std. Error"]
tau0r <- summary(priormod)[["coefficients"]]["theta","Std. Error"]^2
tau0theta <- summary(priormod)[["coefficients"]]["theta","Std. Error"]^2

fit_game <- function(dat = NA, anchor = NA, discount_wt = 0.975, S = 10000, B = 500){
  
  model.game <- function(){
    
    for(i in 1:N){
      #g <- games[i]             # index of current game
      delta[i] <- del^abs(games[i]-g0)  # discount rate for game g relative to anchor game t
      # player-level raneffs
      
      logit(prob[i]) <- beta_int[player[i]]*int[i] + 
        beta_r[player[i]]*logr[i] + 
        beta_theta[player[i]]*theta[i] 
      
      p1[i] <- prob[i]^result[i]
      p2[i] <- (1-prob[i])^(1-result[i])
      p[i] <- (p1[i] * p2[i])^delta[i]  # prob = likelihood, p = discounted likelihood
      
      #result[i] ~ dbern(prob[i])
      #y[i] <- 1 # "ones trick" dummy outcomes THIS LINE CAUSES PROBLEMS
      y[i] ~ dbern(p[i]) # defines correct discounted likelihood function
    }
    
    #same priors as player model.
    # priors on random player effects
    for(j in 1:M){
      beta_int[j] ~ dnorm(beta_int0,tau_int)
      beta_r[j] ~ dnorm(beta_r0,tau_r)
      beta_theta[j] ~ dnorm(beta_theta0,tau_theta)
    }
    # Priors
    beta_int0   ~ dnorm(0, 0.1)
    beta_r0     ~ dnorm(mu0r, 0.01) #would not expect a lot of variation in distance parameter between players. Everyone should get worse as distance   increases.  
    beta_theta0 ~ dnorm(mu0theta, 0.1)
    
    # Hyperpriors
    tau_int ~ dgamma(10, 100)
    tau_r ~ dgamma(10, 0.2)
    tau_theta ~ dgamma(10, 10)
  }
  
  datlist.game <- list(
    int = rep(1, nrow(dat)), 
    logr = log(dat$r), 
    theta = dat$theta, 
    result = dat$result, 
    player = as.integer(as.factor(dat$globalplayerid)),
    N = nrow(dat), 
    M = n_distinct(dat$globalplayerid),
    mu0r = mu0r,
    mu0theta = mu0theta,
    # tau0r = tau0r,
    # tau0theta = tau0theta,
    del = discount_wt,
    games = as.integer(as.factor(dat$gameid)),
    g0 = anchor,
    y = rep(1, nrow(dat)) #"phantom data"
  )
  
  #same params as player model (may need to change)
  params <- c("beta_int","beta_r", "beta_theta","beta_int0","beta_r0", "beta_theta0", "tau_int", "tau_r", "tau_theta")
  
  M <- n_distinct(dat$globalplayerid)
  initslist <- list(list("beta_int"=rep(0,M), "beta_r"=rep(0,M), "beta_theta"=rep(0,M),
                         "beta_int0"=0,"beta_r0"=0, "beta_theta0"=0, 
                         "tau_int"=1, "tau_r"=1, "tau_theta"=1
  ))
  
  sim.game <- jags(data = datlist.game, 
                   n.iter = S, n.chains = 1, n.burnin = B, n.thin = 1,
                   inits = initslist,
                   parameters.to.save = params,
                   model.file=model.game
  )
  sim.mcmc.game <- as.data.frame(as.mcmc(sim.game)[[1]])
  return(sim.mcmc.game)
  
}

fit_games <- function(dat = NA, discount_wt = 0.975, S = 10000, B = 500){
  
  G <- n_distinct(dat$gameid)
  M <- n_distinct(dat$globalplayerid)
  
  game.mcmc.list <- as.list(rep(NA, G))
  #game.mcmc <- array(NA, c(1, 1, G))
  
  for(g in 1:G){
    print(paste(discount_wt, g, sep=" : "))
    game.mcmc <- fit_game(dat, g, discount_wt, S, B)
    game.mcmc.list[[g]] <- game.mcmc
  }
  
  return(game.mcmc.list)
}

get_season_params <- function(dat=NA, sim.mcmc.list=NA){
  
  G <- n_distinct(dat$gameid)
  M <- n_distinct(dat$globalplayerid)
  
  for(g in 1:G){
    params.game <- get_player_params(dat, sim.mcmc.list[[g]]) %>% filter(!is.na(factorid))
    if(g==1){
      params.season <- data.frame(matrix(nrow = M*G, ncol = ncol(params.game)))
      colnames(params.season) <- c(colnames(params.game))
      params.season$g <- rep(1:G, each=M)
      gameids <- gamemap %>% arrange(factorid) %>% '[['(1)
      params.season$gameid <- rep(gameids, each=M)
    }
    params.season[(M*(g-1) + 1):(M*g), 1:ncol(params.game)] <- params.game
  }
  
  n2 <- dat %>% group_by(gameid, globalplayerid) %>% summarize(nshots = n()) %>% as.data.frame()
  params.season <- merge(params.season, n2, all=TRUE)
  params.season$nshots[is.na(params.season$nshots)] <- 0
  
  return(params.season)
}

# game.mcmc.list.100 <- fit_games(Xtrain, 0.100, 10000, 500)
# game.mcmc.list.250 <- fit_games(Xtrain, 0.250, 10000, 500)
# game.mcmc.list.375 <- fit_games(Xtrain, 0.375, 10000, 500)
# game.mcmc.list.500 <- fit_games(Xtrain, 0.500, 10000, 500)
# game.mcmc.list.625 <- fit_games(Xtrain, 0.625, 10000, 500)
game.mcmc.list.750 <- fit_games(Xtrain, 0.750, 10000, 500)
game.mcmc.list.850 <- fit_games(Xtrain, 0.850, 10000, 500)
game.mcmc.list.900 <- fit_games(Xtrain, 0.900, 10000, 500)
# game.mcmc.list.950 <- fit_games(Xtrain, 0.950, 10000, 500)
# game.mcmc.list.975 <- fit_games(Xtrain, 0.975, 10000, 500)
# game.mcmc.list.999 <- fit_games(Xtrain, 0.999, 10000, 500)

# params.season.100  <- get_season_params(Xtrain, game.mcmc.list.100)
# params.season.250  <- get_season_params(Xtrain, game.mcmc.list.250)
# params.season.375  <- get_season_params(Xtrain, game.mcmc.list.375)
# params.season.500  <- get_season_params(Xtrain, game.mcmc.list.500)
# params.season.625  <- get_season_params(Xtrain, game.mcmc.list.625)
params.season.750  <- get_season_params(Xtrain, game.mcmc.list.750)
params.season.850  <- get_season_params(Xtrain, game.mcmc.list.850)
params.season.900  <- get_season_params(Xtrain, game.mcmc.list.900)
# params.season.950  <- get_season_params(Xtrain, game.mcmc.list.950)
# params.season.975  <- get_season_params(Xtrain, game.mcmc.list.975)
# params.season.999  <- get_season_params(Xtrain, game.mcmc.list.999)

# save(game.mcmc.list.100, file="../rdatafiles/gamemcmclist100.RData")
# save(game.mcmc.list.250, file="../rdatafiles/gamemcmclist250.RData")
# save(game.mcmc.list.375, file="../rdatafiles/gamemcmclist375.RData")
# save(game.mcmc.list.500, file="../rdatafiles/gamemcmclist500.RData")
# save(game.mcmc.list.625, file="../rdatafiles/gamemcmclist625.RData")
save(game.mcmc.list.750, file="../rdatafiles/gamemcmclist750.RData")
save(game.mcmc.list.850, file="../rdatafiles/gamemcmclist850.RData")
save(game.mcmc.list.900, file="../rdatafiles/gamemcmclist900.RData")
# save(game.mcmc.list.950, file="../rdatafiles/gamemcmclist950.RData")
# save(game.mcmc.list.975, file="../rdatafiles/gamemcmclist975.RData")
# save(game.mcmc.list.999, file="../rdatafiles/gamemcmclist999.RData")

# save(params.season.100,  file="../rdatafiles/paramsseason100.RData")
# save(params.season.250,  file="../rdatafiles/paramsseason250.RData")
# save(params.season.375,  file="../rdatafiles/paramsseason375.RData")
# save(params.season.500,  file="../rdatafiles/paramsseason500.RData")
# save(params.season.625,  file="../rdatafiles/paramsseason625.RData")
save(params.season.750,  file="../rdatafiles/paramsseason750.RData")
save(params.season.850,  file="../rdatafiles/paramsseason850.RData")
save(params.season.900,  file="../rdatafiles/paramsseason900.RData")
# save(params.season.950,  file="../rdatafiles/paramsseason950.RData")
# save(params.season.975,  file="../rdatafiles/paramsseason975.RData")
# save(params.season.999,  file="../rdatafiles/paramsseason999.RData")
