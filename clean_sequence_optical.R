library(xml2); library(dplyr); library(reshape2)
#setwd("F:/SPORTVU2014-15")
datafolder <- "C:/Users/Nathaniel Brown/Desktop/sample"
#"F:/sample"
#gameid <- #201401250173 #FSU
#   201311150173 #vermont
gameid <- 201311150173 #florida atl
list.files(datafolder) %>% .[endsWith(., "XML")] %>% gsub(x=., pattern='[0-9]',replacement='\\1')

readoptical <- function(opticalname){
  opticalxml <- read_html(paste0(opticalname))
  opticaldf <-  xml_find_all(opticalxml, ".//moment") %>% 
                xml_attrs() %>% #c("time", "locations", "game-clock", "shot-clock", "game-event-id")) %>%
                as.data.frame(stringsAsFactors=FALSE) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
  rownames(opticaldf) <- NULL
  colnames(opticaldf) <- c("gameclock", "time", "gameeventid", "shotclock", "locations")
  return(opticaldf)
}


to_mat <- function(lst, namez){
  df <- lapply(lst, function(x){strsplit(x,",")}) %>% data.frame() %>% t()
  colnames(df) <- namez
  rownames(df) <- NULL
  return(df)
}

clean_optical <- function(optical){
  #names(optical) <- lapply(strsplit(names(optical),'...', fixed= TRUE), function(x){x[2]}) %>% as.character()
  
  name <- c("team_id", "player_id", "y", "x", "z")
  loc <- optical[,"locations"]
  loc <- strsplit(loc, ";")
  
  ball <- to_mat(lapply(loc, '[', 1), name)
  p1 <- to_mat(lapply(loc, '[', 2), name)
  p2 <- to_mat(lapply(loc, '[', 3), name) 
  p3 <- to_mat(lapply(loc, '[', 4), name) 
  p4 <- to_mat(lapply(loc, '[', 5), name)
  p5 <- to_mat(lapply(loc, '[', 6), name) 
  p6 <- to_mat(lapply(loc, '[', 7), name)
  p7 <- to_mat(lapply(loc, '[', 8), name) 
  p8 <- to_mat(lapply(loc, '[', 9), name) 
  p9 <- to_mat(lapply(loc, '[', 10), name) 
  p10 <- to_mat(lapply(loc, '[', 11), name) 
  
  optical[["ball_x"]] <- ball[,"x"] 
  optical[["ball_y"]] <- ball[,"y"] 
  optical[["ball_z"]] <- ball[,"z"] 
  
  optical[["p1_global_id"]] <- p1[,"player_id"]
  optical[["p1_team_id"]] <- p1[,"team_id"]
  optical[["p1_x"]] <- p1[,"x"] 
  optical[["p1_y"]] <- p1[,"y"] 
  
  optical[["p2_global_id"]] <- p2[,"player_id"]
  optical[["p2_team_id"]] <- p2[,"team_id"]
  optical[["p2_x"]] <- p2[,"x"] 
  optical[["p2_y"]] <- p2[,"y"] 
  
  optical[["p3_global_id"]] <- p3[,"player_id"]
  optical[["p3_team_id"]] <- p3[,"team_id"]
  optical[["p3_x"]] <- p3[,"x"] 
  optical[["p3_y"]] <- p3[,"y"] 
  
  optical[["p4_global_id"]] <- p4[,"player_id"]
  optical[["p4_team_id"]] <- p4[,"team_id"]
  optical[["p4_x"]] <- p4[,"x"] 
  optical[["p4_y"]] <- p4[,"y"] 
  
  optical[["p5_global_id"]] <- p5[,"player_id"]
  optical[["p5_team_id"]] <- p5[,"team_id"]
  optical[["p5_x"]] <- p5[,"x"] 
  optical[["p5_y"]] <- p5[,"y"] 
  
  optical[["p6_global_id"]] <- p6[,"player_id"]
  optical[["p6_team_id"]] <- p6[,"team_id"]
  optical[["p6_x"]] <- p6[,"x"] 
  optical[["p6_y"]] <- p6[,"y"] 
  
  optical[["p7_global_id"]] <- p7[,"player_id"]
  optical[["p7_team_id"]] <- p7[,"team_id"]
  optical[["p7_x"]] <- p7[,"x"] 
  optical[["p7_y"]] <- p7[,"y"] 
  
  optical[["p8_global_id"]] <- p8[,"player_id"]
  optical[["p8_team_id"]] <- p8[,"team_id"]
  optical[["p8_x"]] <- p8[,"x"] 
  optical[["p8_y"]] <- p8[,"y"] 
  
  optical[["p9_global_id"]] <- p9[,"player_id"]
  optical[["p9_team_id"]] <- p9[,"team_id"]
  optical[["p9_x"]] <- p9[,"x"] 
  optical[["p9_y"]] <- p9[,"y"] 
  
  optical[["p10_global_id"]] <- p10[,"player_id"]
  optical[["p10_team_id"]] <- p10[,"team_id"]
  optical[["p10_x"]] <- p10[,"x"]
  optical[["p10_y"]] <- p10[,"y"]
  
  optical[["locations"]] <- NULL
  rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ball, loc)
  
  optical <- apply(optical,2,as.numeric)
  optical <- as.data.frame(optical)
  return(optical)
}


#incomplete function
xml_to_df_clean <- function(opticalname){
  dat <- NULL
  for(half in opticalname){
    dat <- cbind(dat,clean_optical(readoptical(half)))
  }
  
  return(dat)
}
# dat1 <- clean_optical(readoptical(opticalname[1]))
# dat2 <- clean_optical(readoptical(opticalname[2]))
# dat1$half <- 1
# dat2$half <- 2
# game <- rbind(dat1, dat2)
# save(game, file=paste0(datafolder,"/game.RData"))
load(file=paste0(datafolder,"/game.RData"))
game$ball_global_id <- -1

readpbp <- function(pbpname){
  pbpxml <- read_html(paste0(pbpname))
  pbpdf <-  xml_find_all(pbpxml, ".//moment") %>% 
    xml_attrs() %>% #c("event-id", "game-clock", "time", "player-id", "global-player-id", "pbp-seq-number", "shot-clock",  )) %>%
    as.data.frame(stringsAsFactors=FALSE) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
  rownames(pbpdf) <- NULL
  colnames(pbpdf) <- c("eventid", "gameclock", "time", "playerid", "globalplayerid", "pbpseqnum", "shotclock")
  pbpdf <- as.data.frame(apply(pbpdf, 2, as.numeric))
  return(pbpdf)
}

readbox <- function(boxname){
  boxxml <- read_html(boxname)
  boxdf <- xml_find_all(boxxml, ".//player") %>% 
    xml_attrs() %>%
    as.data.frame(stringsAsFactors=FALSE) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
  rownames(boxdf) <- NULL
  colnames(boxdf) <- gsub("-", "", colnames(boxdf))
  return(boxdf)
}

secs_to_ms <- function(x){
  x <- as.numeric(x)
  m <- x %/% 60
  s <- round(x %% 60,2)
  paste0(m,":",s)
}


filenames <- grep(gameid, 
                  list.files(datafolder), 
                  value=TRUE)

opticalname <- paste(datafolder, grep("FINAL_SEQUENCE_OPTICAL", filenames, value=TRUE), sep="/")
pbpname <- paste(datafolder, grep("PBP", filenames, value=TRUE), sep="/")
boxname <- paste(datafolder, grep("BOXSCORE", filenames, value=TRUE), sep="/")
boxopticalname <- paste(datafolder, grep("BOX_OPTICAL", filenames, value=TRUE), sep="/")

pbp_save <- readpbp(pbpname)
box <- readbox(boxname)

pbp <- pbp_save
pbp$gameclock_ms <- secs_to_ms(as.numeric(pbp$gameclock))
pbp$ord <- 1:nrow(pbp)

#pbp %>% filter(playerid == 121033) %>% group_by(eventid) %>% summarize(count=n()) %>% as.data.frame() %>% arrange(as.numeric(eventid))

player_map <- box %>% dplyr::select(globalplayerid, first, last, globalteamid) %>% mutate(playername = paste(first, last)) %>% dplyr::select(-first, -last) %>% unique()
pbp <- merge(pbp, player_map, by="globalplayerid", all=TRUE) %>% arrange(ord)
#pbp %>% filter(eventid == 11) %>% View()

#sportvu knows locations of players but not exactly if they are on lines or not! only reports makes/misses! not 2 or 3!
eventids <- 
  c(
  "1", "FTM",
  "2", "FTm",
  "3",  "FGM",
  "4", "FGm",
  "5", "OREB",
  "6", "DREB",
  "7", "TO",
  "8",  "PF",
  "9", "",
  "10","",
  "11","Timeout",
  "12","",
  "13","",
  "14","",
  "15","EOP",
  "16","",
  "17","",
  "18","",
  "19","",
  "20","",
  "21", "dribble",
  "22", "pass",
  "23", "touch",
  "24", "BLK",
  "25", "AST")
  
eventid_map <- as.data.frame(t(matrix(eventids, nrow=2)), stringsAsFactors = FALSE)
colnames(eventid_map) <- c("eventid", "eventdesc")
eventid_map[["eventid"]] <- as.numeric(eventid_map[["eventid"]])
eventid_map <- arrange(eventid_map, eventid)

#write a function to get time-stamped shot attempts and results!
#later: also investigate when players enter and exit game

#THERE SEEM TO BE ABOUT FOUR SHOTS MISSING. TIP-INS? CALLED BACK? WILL INVESTIGATE LATER.
shots <- pbp %>% filter(eventid %in% c(3,4)) %>% mutate(result = ifelse(eventid == 3, 1, 0)) %>% select_("globalplayerid", "time", "result", "globalteamid", "gameclock") #%>% group_by(playername) %>% summarize(n=n(), k=sum(result))
non_loc_cols <- grep("ball|team|shotclock|gameeventid|gameclock",colnames(game))
second_half_start <- game$time[game$half == 2][1]
locs <- game %>% 
  filter(time %in% shots$time) %>% 
  select(-non_loc_cols) %>% 
  melt(id = c("time", "half")) %>% 
  mutate(variable2 = ifelse(grepl("id", variable), "id",
                            ifelse(grepl("x", variable), "x",
                                   ifelse(grepl("y", variable),"y",NA))),
         rowid = rep(1:(nrow(.)/3),3)) %>% 
  dcast(time + rowid ~ variable2) %>%
  rename(globalplayerid = id)

head(locs)
dim(locs)
View(locs)

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

plot(shot_locs$gametime, shot_locs$x2)
plot(shot_locs$gametime, shot_locs$y2)

plot(shot_locs$gametime, shot_locs$r)
plot(shot_locs$gametime, shot_locs$theta)
plot(shot_locs$gametime, abs(shot_locs$theta))

mod <- glm(result ~ r + theta, data = shot_locs, family="binomial")


#shotlocs <- merge(shots, locs, by=c("time", "globalplayerid")) %>% select(-rowid)

locs <- melt(select_(game, -"gameclock", "time", "gameeventid", "shotclock", "ball_x", "ball_y", "ball_z"), id = "time")

d <- data.frame(state = c("SC", "NC", "VA"),
                city1 = c("Rock Hill", "Durham", "Reston"),
                city2 = c("Myrtle Beach", "Wilmington", "Chesapeake"),
                city3 = c("Lexington", "Charlotte", "Richmond"))
d2 <- melt(d, id = "state")
