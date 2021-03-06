#From West & Harrison 14.3

#things I'm having trouble with:
  #positive definite sCt[,,t]
  #confidence interval plot

#aggregate team level things to do:
  #get an entire season into a data frame
  #split into fixed time bins of (30?) secs
  #start with binomial glm on first game of season to get initial estimates for r, phi (angle), and intercept parameters for prior in future games
  #weaknesses: you're gonna lose some resolution; it will water down the effect of the parameters (location, maybe decisions)
    #hey, what about random effects? useful for estimates of avg, but not...
  #interesting thing to look at: how shot location changes within games and across games!
    #could we have time as a predictor as well?
    #more interesting than shot success actually. at the team level that is.
    #something you can do now!!! r and theta as time series throughout a game. just use line graph/moving avg or somthing initially.

library(mvtnorm)
#court dimensions
xm <- 50; ym <- 94            
#num time units within a game
T <- 48/0.25                 
#pdf("DGLM_plots.pdf")
#generating shot attempts with constant prob over the course of a game
prshot <- 0.4
#shots is binary shot attempt vector
shots <- runif(n=T) < prshot  
tshot <- which(shots)
nshots <- length(tshot)

#x-y coordinates in feet. origin = basket
x <- matrix(c(xm*(2*rbeta(n=nshots, 6,8) - 1),
              ym*rbeta(n=nshots, 2, 12)), 
            ncol = 2)

par(xpd=FALSE)
plot(x, ylim=c(0,ym), xlim=c(-xm,xm), xlab="x", ylab="y", main = "Simulated Shots")
abline(h=ym/2)
points(0,0,col="red", cex=2)

#convert to theta and r
z <- matrix(c(atan(x[,2]/x[,1]),
            sqrt(rowSums(x^2))),
            ncol=2)
#a shifting factor to roughly center log(r) around 0
za <- log(ym/5)
#2 covariates and intercept
Z <- cbind(1, z[,1], log(z[,2]) - za)
plot(Z[,2], Z[,3]+za, main = "GLM Parameters", xlab = "angle", ylab="log(distance)")

#generating shot success probabilities
theta <- matrix(c(-0.5, 1.5, -5.5)) #GLM parameters (intercept, angle, log distance)
p <- length(theta)
pscore <- 1/(1+exp(-Z %*% theta))
q <- rep(NaN, T)
q[shots] <- pscore

#generating shot outcomes
y <- rep(NaN, T)
y[shots] <- runif(n=nshots) <= pscore
iy <- which(y[!is.nan(y)] == 1)

par(xpd=TRUE)
plot(0,0,type="n",xlim = c(0,T),ylim=c(0,1), ylab = "probability", xlab = "time interval", main = "GLM")
points(tshot, q[tshot], pch=4, col = "blue")
points(tshot, y[tshot], pch=1, col = "red")
legend(x=T*.8, y=1.21, legend=c("probability", "outcome"), pch = c(4,1), col=c("blue", "red"))

par(xpd=FALSE)
plot(x[iy,], ylim=c(0,ym), xlim=c(-xm,xm), col = "red", pch = 3, xlab="x", ylab="y", main = "Simulated Makes and Misses")
points(x[-iy,], col ="blue", pch = 1)
abline(h=ym/2)
points(0,0,col="red", cex=2)



#Forward Filtering

#set up DGLM and initial prior
#first, set up covariates per time interval
F <- t(Z)
p <- dim(F)[1]
#theta = state vector (GLM parameters) (px1)
#F = the data...regression vectors for all t...aka the design matrix (pxT)
#G = known evolution matrix ???????
#omega = evolution errors with 0 mean and known variance matrix W
#g(.) = function to map eta to real line (logit)

mt <- theta   
Ct <- diag(p)
#mt = prior mean vector
#Ct = prior covariance matrix
#(theta[t-1]|D[t-1]) ~ N(mt[t-1], Ct[t-1])

delta <- 0.99 #discount factor; "streaky parameter"
#forward filtering (FF)
smt <- matrix(rep(0,p*T), nrow=p)           #save post means
sCt <- array(rep(0,p*p*T), dim = c(p,p,T))  #save post covars
spt <- rep(NaN, T)                          #save post prob success
lmlik <- rep(0,T)                           #marg lik per time int
ishot <- 0
for(t in 1:T){
  if(t %in% tshot){
    #current shot attempt index, and time
    ishot <- ishot + 1
    ti <- tshot[ishot]
    
    ft <- (F[,ishot]) %*% mt
    At <- Ct %*% F[,ishot]/delta
    qt <- (F[,ishot]) %*% At
    At <- At/as.numeric(qt)
    
    #at = Gt*mt in txtbk, but = mt here.
    #Rt = Gt*Ct[t-1]*Gt' + Wt in txtbk, but = Ct/delta here
    #f = F'at = F'mt
    #q = F'RF = F'Ct F (1/delta)
    #((lambda,theta)' | Dt-1) ~ N( (f, a), ((q, F'C/delta),(CF/delta, C)) )

    #what is mu tho?
    #???????????????????????
    #"the samp dist of Yt depends on thetat only via the single quantity mut
    #prior: (mu|Dt) ~ N(f, q)
    #Vt > 0 is scale parameter aka precision of distribution...
    #but precision of what??? what is b(Yt, Vt?)
    #Q = q + Vt
    #post:  (mu|Dt) ~ N(f*, q*)
    
    #f* = 
    #what is mu???
    #f = F'a which is
    
    #prior mean and var of linear predictor, and adaptive vector
    #compute approx prior Beta(r,s) params; update w/ numerical iterations for exact values
    eft <- exp(ft)   #crude initial values
    rt <- (1+eft)/qt
    st <- rt/eft
    rt <- max(0.5, rt)
    st <- max(0.5, st)
    
    
    

    #fts = ft* = posterior mean of ????
    #qts = qt* = posterior variance of something ???
    #iterative numerical solution
    ep <- 0.5; drt <- 1; dst <- 1; xt <- matrix(c(rt, st))
    while(max(drt, dst) < ep){
      r0t <- psigamma(rt,0); s0t <- psigamma(st,0)
      r1t <- psigamma(rt,1); s1t <- psigamma(st,1)
      fxt <- c(r0t-s0t-ft, r1t+s1t-qt)
      Axt <- matrix(c(r1t, -s1t, psigamma(rt, 2), psigamma(st, 2)), ncol=2, byrow = TRUE)
      xt <- xt - solve(Axt, fxt)
      drt <- xt[1] - rt; dst <- xt[2] - st
      rt <- xt[1]; st <- xt[2]
    }
    lmlik[t] <- lgamma(rt+st) - lgamma(rt) - lgamma(st) + 
                lgamma(rt+y[t]) + lgamma(st+1-y[t]) - lgamma(rt+st+1) + 
                lgamma(2) - lgamma(1+y[t]) - lgamma(2-y[t])
    rts <- rt + y[t]; sts <- st + 1-y[t] #posterior beta params
    #convert to mean and variance for linear predictor
    fts <- psigamma(rts,0)-psigamma(sts,0); qts <- psigamma(rts,1)+psigamma(sts,1)
    spt[t] <- rts/(sts+rts)
    
    #update state parameters
    mt <- mt+At%*%(fts-ft)
    Ct <- Ct/delta - (At%*%t(At))*as.numeric(qt-qts)
    Ct <- (Ct + t(Ct))/2
    c(t, rt, st, mt)
    
    if(any(is.nan(mt))){
      print("stop")
      break
    }
    
  }
  smt[,t] <- mt; sCt[,,t] <- Ct #saving
}

par(xpd=TRUE)
plot(smt[1,],type="l", col = "blue", ylim = c(-20, 20), xlab = "time interval", ylab = "online state mean", main = "Dynamic Params")
lines(smt[2,],type="l", col = "orange")
lines(smt[3,],type="l", col = "yellow")
legend(x=T*.8, y=30.5, legend = c("intercept", "angle", "log(distance)"), pch = c(16), col = c("blue", "orange", "yellow"))

plot(0,0,type="n",xlim = c(0,T),ylim=c(0,1), ylab = "probability", xlab = "time interval", main = "DGLM")
points(tshot, spt[tshot], pch=4, col = "blue")
points(tshot, y[tshot], pch=1, col = "red")
legend(x=T*.8, y=1.21, legend=c("probability", "outcome"), pch = c(4,1), col=c("blue", "red"))





#Backward sampling
nmc <- 1000
#save posterior means and posterior success probs
MCtheta <- array(0, c(p, T, nmc)) 
MCq <- array(0, c(T, nmc))

#begin BS at timeunit T
thetat <- rmvnorm(n=nmc, smt[,T], sCt[,,T]) #SOMETIMES sCT[,,T] IS NOT POSITIVE DEFINITE. DEPENDS ON RANDOM SEED.
MCtheta[,T,] <- t(thetat)
MCq[T,] <- 1/(1+exp(-thetat %*% F[,nshots]))

#then recurse backwards
ishot <- nshots + 1
for(t in (T-1):1){
  if(t %in% tshot){
    ht = (1-delta)*t(array(smt[,t], c(dim(smt)[1], nmc))) + delta*thetat
    #run a simulation for each row of ht and each 3rd dim of sCt
    thetat <- t(apply(ht, 1, rmvnorm, n=1, sigma = sCt[,,t]*(1-delta)))
    MCtheta[,t,] <- t(thetat)
    ishot <- ishot - 1; ti <- tshot[ishot]
    MCq[t,] <- 1/(1+exp(-thetat %*% F[,ishot]))
  }
}

#retrospective posterior summaries
#posterior of shot probabilities?
pr <- t(apply(MCq[tshot,], 1, quantile, c(.025, .25, .5, .75, .975))) #get quantiles of each row
plot(0,0, type="n", xlim = c(0,T), ylim=c(0,1), main = "Posterior Probability", ylab="hit rate", xlab="time interval") 
lines(x=tshot, y=pr[,1], col = "gray")
lines(x=tshot, y=pr[,5], col = "gray")
polygon(c(tshot, rev(tshot)), c(pr[,1], rev(pr[,5])),
        col = "gray", border = NA)
lines(x=tshot, y=pr[,2], col = "black")
lines(x=tshot, y=pr[,4], col = "black")
polygon(c(tshot, rev(tshot)), c(pr[,2], rev(pr[,4])),
        col = "black", border = NA)
lines(x=tshot, y=pr[,3], col = "red")
points(x=1:T, y=y, pch=1)

#posteriors of parameters from DGLM
posterior_labels <- c("Posterior Intercept", "Posterior Angle", "Posterior Log Distance")
for(j in 1:p){
  
  pr = t(apply(MCtheta[j,tshot,], 1, quantile, c(.025, .25, .5, .75, .975)))
  plot(0,0, type="n", xlim = c(0,T), ylim = range(pr), main = posterior_labels[j], xlab = "time interval", ylab = "state vector element") 
  lines(x=tshot, y=pr[,1], col = "gray")
  lines(x=tshot, y=pr[,5], col = "gray")
  polygon(c(tshot, rev(tshot)), c(pr[,1], rev(pr[,5])),
          col = "gray", border = NA)
  lines(x=tshot, y=pr[,2], col = "black")
  lines(x=tshot, y=pr[,4], col = "black")
  polygon(c(tshot, rev(tshot)), c(pr[,2], rev(pr[,4])),
          col = "black", border = NA)
  points(x=tshot, y=pr[,3], col = "red", pch = 4)
  
}

dev.off()
