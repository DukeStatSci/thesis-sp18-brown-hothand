#court dimensions
xm <- 50; ym <- 94            
#num time units within a game
T <- 48/0.25                 

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
plot(x, ylim=c(0,ym), xlim=c(-xm,xm))
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
plot(Z[,2], Z[,3]+za, xlab = "angle", ylab="log distance")

#generating shot success probabilities
theta <- matrix(c(-0.5, 1.5, -5.5)) #GLM parameters
p <- length(theta)
pscore <- 1/(1+exp(-Z %*% theta))
q <- rep(NaN, T)
q[shots] <- pscore

#generating shot outcomes
y <- rep(NaN, T)
y[shots] <- runif(n=nshots) <= pscore
iy <- which(y[!is.nan(y)] == 1)
par(xpd=TRUE)
plot(0,0,type="n",xlim = c(0,T),ylim=c(0,1), ylab = "probability", xlab = "time interval")
points(tshot, q[tshot], pch=4, col = "blue")
points(tshot, y[tshot], pch=1, col = "red")
legend(x=T/2, y=1.2, legend=c("probability", "outcome"), pch = c(4,1), col=c("blue", "red"))
plot(x[iy,], ylim=c(0,ym), xlim=c(-xm,xm), col = "red", pch = 3)
points(x[-iy,], col ="blue", pch = 1)
points(0,0,col="red", cex=2)

#set up DGLM and initial prior
#first, set up covariates per time interval
F <- t(Z)
p <- dim(F)[1]
mt <- rep(0,p) #prior mean vector
Ct <- diag(p) #prior covariance matrix
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
    
    #prior mean and var of linear predictor, and adaptive vector
    #compute approx prior Beta(r,s) params; update w/ numerical iterations for exact values
    
  }
}