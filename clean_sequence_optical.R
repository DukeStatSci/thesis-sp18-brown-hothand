source("C:/Users/Nathaniel Brown/Documents/GitHub/thesis-sp18-brown-hothand/sportvu_fxns.R")

# player_map <- get_player_map()

allgameids <- get_all_gameids()
allgameshots <- array(NA, c(100*length(allgameids),13))
r <- 1
for(i in 1:length(allgameids)){
  
  id <- allgameids[i]
  gameshots <- as.matrix(get_shots(id))
  
  allgameshots[r:(r+nrow(gameshots)-1),] <- gameshots
  r <- r+nrow(gameshots)+1
  print(length(allgameids) - i)
  
}
allgameshots_full <- as.data.frame(allgameshots)
allgameshots <- allgameshots_full %>% filter(complete.cases(.))
colnames(allgameshots) <- colnames(allgameshots_full) <- colnames(gameshots)
allgameshots$season <- get_season(allgameshots$gameid)
allgameshots_full$season <- get_season(allgameshots_full$gameid)

save(allgameshots, file=paste0(datafolder, "/allgameshots.RData"))
save(allgameshots_full, file=paste0(datafolder,"/allgameshots_full.RData"))
#save(game, file = paste0(datafolder,"/game", gameid, ".RData"))

kennardid <- 887661
allenid <- 842296

sub <- (allgameshots %>% filter(globalplayerid == allenid, season == 2017) %>% select(x2,y2))
sub1 <- (allgameshots %>% filter(globalplayerid == allenid, season == 2017, result==1) %>% select(x2,y2))
sub0 <- (allgameshots %>% filter(globalplayerid == allenid, season == 2017, result==0) %>% select(x2,y2))

plot(sub, type="n", xlim=c(-25,25), ylim=c(0,94))
points(sub0, col="blue")
points(sub1,col="red")

magnitude <- function(v){
  return(sqrt(sum(abs(v*v))))
}

colnames(allroadshots) <- colnames(roadshots)
plot(allroadshots[,"x2"], allroadshots[,"y2"])

pbp <- (load_pbp(gameid))
game <- load_optical(gameid)



#TODO: also investigate when players enter and exit game

#THERE SEEM TO BE ABOUT FOUR SHOTS MISSING. TIP-INS? CALLED BACK? WILL INVESTIGATE LATER.



shot_locs <- merge(shots, locs, by=c("time", "globalplayerid")) %>% filter(globalteamid == 1388)
#FOR NOW THESE ARE ONLY THE STARTERS' SHOTS!!!
#transform x and y so that basket is origin, and all shots occur on one half of court
#then transform into r and theta.
shot_locs$x2 <- shot_locs$x - 25
shot_locs$y2 <- shot_locs$y
shot_locs$y2[shot_locs$time < second_half_start] = (94 - shot_locs$y2[shot_locs$time < second_half_start])
shot_locs$y2 <- shot_locs$y2 - 4 #so the hoop is 0, not the baseline
shot_locs$r <- sqrt(shot_locs$x2^2 + shot_locs$y2^2)
shot_locs$theta <- atan(shot_locs$y2/shot_locs$x2)
shot_locs$gametime <- shot_locs$gameclock
shot_locs$gametime[shot_locs$time < second_half_start] <- 2*shot_locs$gameclock[shot_locs$time < second_half_start]
shot_locs$gametime <- 2400 - shot_locs$gametime

allgameids
allgameids[startsWith(as.character(allgameids), "2017")]
#TODO: get empirical distribution of r and theta given 2016-17 season
plot(shot_locs$gametime, shot_locs$x2)
plot(shot_locs$gametime, shot_locs$y2)

plot(shot_locs$gametime, shot_locs$r)
plot(shot_locs$gametime, shot_locs$theta)
plot(shot_locs$gametime, abs(shot_locs$theta))

plot(shot_locs$x2, shot_locs$y2, xlab="x", ylab="y", main = "Shots",
     ylim=c(0,94), xlim=c(-25,25))
abline(h=94/2)
points(0,0,col="red", cex=2)

plot(shot_locs$theta, (shot_locs$r), type="n",xlab="angle", ylab="log(distance)", main="log distance vs angles")
points(shot_locs$theta[shot_locs$result==1 & shot_locs$globalplayerid == 887661], (shot_locs$r)[shot_locs$result==1 & shot_locs$globalplayerid == 887661], col="red")
points(shot_locs$theta[shot_locs$result==0 & shot_locs$globalplayerid == 887661], (shot_locs$r)[shot_locs$result==0 & shot_locs$globalplayerid == 887661], col="blue")
mod <- glm(result ~ r + theta, data = shot_locs[shot_locs$globalplayerid == 887661,], family="binomial")
