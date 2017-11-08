library(xml2); library(dplyr); library(reshape2)
#setwd("F:/SPORTVU2014-15")
datafolder <- "C:/Users/Nathaniel Brown/Desktop/DMBBall Data"
load(paste0(datafolder, "/allgameshots.RData"))
load(paste0(datafolder, "/playermap.RData"))


#let's assume that every gameid is 12 digits?
get_all_gameids <- function(){
  idlen <- 12
  allfilenames <- list.files(datafolder) %>% .[endsWith(., "XML")] #%>% gsub(x=., pattern='[0-9]',replacement='\\1')
  idstarts <- gregexpr(text=allfilenames, pattern=paste0("[0-9]{",idlen,"}")) %>% lapply('[', 1) %>% as.numeric()
  idstarts[idstarts < 0] <- NA
  allgameids <- substr(allfilenames, idstarts, idstarts+idlen-1) %>% '['(!is.na(.)) %>% as.numeric() %>% unique()
  return(allgameids)
}

get_eventids <- function(){
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
      "25", "AST",
      "28","")
  eventid_map <- as.data.frame(t(matrix(eventids, nrow=2)), stringsAsFactors = FALSE)
  colnames(eventid_map) <- c("eventid", "eventdesc")
  eventid_map[["eventid"]] <- as.numeric(eventid_map[["eventid"]])
  eventid_map <- arrange(eventid_map, eventid)
  return(eventid_map)
}

get_season <- function(gameid=NA){
  season <- as.numeric(substr(gameid,1,4))
  increment <- as.numeric(substr(gameid,5,6)) > 4
  season[increment] <- season[increment] + 1
  return(season)
}

roadgames <- get_all_gameids() %>% '['(., !endsWith(as.character(.), "0173"))
#gameid <- 201511200613


read_optical <- function(opticalname=NA){
  opticalxml <- read_html(paste0(opticalname))
  opticaldf <-  xml_find_all(opticalxml, ".//moment") %>% 
    xml_attrs() %>% #c("time", "locations", "game-clock", "shot-clock", "game-event-id")) %>%
    as.data.frame(stringsAsFactors=FALSE) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
  rownames(opticaldf) <- NULL
  colnames(opticaldf) <- c("gameclock", "time", "gameeventid", "shotclock", "locations")
  return(opticaldf)
}

to_mat <- function(lst=NA, namez=NA){
  df <- lapply(lst, function(x){strsplit(x,",")}) %>% data.frame() %>% t()
  colnames(df) <- namez
  rownames(df) <- NULL
  return(df)
}

clean_optical <- function(optical=NA){
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

load_optical <- function(gameid=NA, save=TRUE){
  
  gamefilenames <- grep(gameid, 
                        list.files(datafolder), 
                        value=TRUE)
  
  if(any(grepl(".RData", gamefilenames))){  
    
    load(file = paste0(datafolder,"/game", gameid, ".RData"))
    
  }else{
    game <- NULL
    opticalname <- paste(datafolder, grep("FINAL_SEQUENCE_OPTICAL", gamefilenames, value=TRUE), sep="/")
    for(h in 1:length(opticalname)){
      H <- opticalname[h]
      temp <- clean_optical(read_optical(H))
      temp$half <- h
      game <- rbind(game, temp)
    }
  }
  
  if(save == TRUE){
    save(game, file = paste0(datafolder,"/game", gameid, ".RData"))
  }
  return(game)
}

load_pbp <- function(gameid=NA){
  
  gamefilenames <- grep(gameid, 
                        list.files(datafolder), 
                        value=TRUE)
  
  pbpname <- paste(datafolder, grep("PBP", gamefilenames, value=TRUE), sep="/")
  
  
  pbpxml <- read_html(paste0(pbpname))
  pbpdf <-  xml_find_all(pbpxml, ".//moment") %>% 
    xml_attrs() %>% #c("event-id", "game-clock", "time", "player-id", "global-player-id", "pbp-seq-number", "shot-clock",  )) %>%
    as.data.frame(stringsAsFactors=FALSE) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
  rownames(pbpdf) <- NULL
  colnames(pbpdf) <- c("eventid", "gameclock", "time", "playerid", "globalplayerid", "pbpseqnum", "shotclock")
  pbpdf <- as.data.frame(apply(pbpdf, 2, as.numeric))
  return(pbpdf)
}

load_box <- function(gameid=NA){
  
  gamefilenames <- grep(gameid, 
                        list.files(datafolder), 
                        value=TRUE)
  boxfilename <- grep("_CG", gamefilenames, value=TRUE)
  if(length(boxfilename) == 0){
    return(NULL)
  }
  
  boxname <- paste(datafolder, boxfilename, sep="/")
  
  # print(length(list.files(datafolder)))
  # print(gamefilenames)
  # print(boxname)
  
  boxxml <- read_html(boxname)
  boxdf <- xml_find_all(boxxml, ".//player") %>% 
    xml_attrs() %>%
    as.data.frame(stringsAsFactors=FALSE) %>% t() %>% as.data.frame(stringsAsFactors=FALSE)
  rownames(boxdf) <- NULL
  colnames(boxdf) <- gsub("-", "", colnames(boxdf))
  return(boxdf)
}

get_player_map <- function(){
  
  if(any(grepl("player_map", list.files(datafolder)))){
    
    load(file = paste0(datafolder,"/player_map.RData"))
    
  }else{
    
    allgameids <- get_all_gameids()
    superbox <- box <- NULL
    
    for(gid in allgameids[-1]){
      
      box <- load_box(gid) 
      
      if(length(box) == 0){
        next
      }else{
        box <- box %>% select(teamid, globalteamid, playerid, globalplayerid, first, last)
        superbox <- rbind(superbox, box)
      }
    }
    player_map <- superbox %>% filter(globalteamid == 1388) %>% unique()
    
  }
  return(player_map)
}

get_shot_results <- function(pbp=NA){
  # pbp$gameclock_ms <- secs_to_ms(as.numeric(pbp$gameclock))
  # pbp$ord <- 1:nrow(pbp)
  
  player_map <- get_player_map()
  
  # pbp_playersF <- merge(pbp, player_map, by="globalplayerid", all=FALSE) %>% arrange(ord) %>% select(-ord)
  # pbp_playersT <- merge(pbp, player_map, by="globalplayerid", all=TRUE) %>% arrange(ord) %>% select(-ord)
  pbp_players <- pbp %>% filter(globalplayerid %in% player_map$globalplayerid)
  
  # shotsT <- pbp_playersT %>% filter(eventid %in% c(3,4)) %>% mutate(result = ifelse(eventid == 3, 1, 0)) %>% select_("globalplayerid", "time", "result", "globalteamid", "gameclock")
  # shotsF <- pbp_playersF %>% filter(eventid %in% c(3,4)) %>% mutate(result = ifelse(eventid == 3, 1, 0)) %>% select_("globalplayerid", "time", "result", "globalteamid", "gameclock")
  shots <- pbp_players%>% filter(eventid %in% c(3,4)) %>% mutate(result = ifelse(eventid == 3, 1, 0)) %>% select(globalplayerid, time, result, gameclock)
  
  return(shots)
}

get_shot_locs <- function(optical=NA){
  
  non_loc_cols <- grep("ball|team|shotclock|gameeventid|gameclock",colnames(optical))
  locs <- optical %>% 
    filter(time %in% optical$time) %>% 
    select(-non_loc_cols) %>% 
    melt(id = c("time", "half")) %>% 
    mutate(variable2 = ifelse(grepl("id", variable), "id",
                              ifelse(grepl("x", variable), "x",
                                     ifelse(grepl("y", variable),"y",NA))),
           rowid = rep(1:(nrow(.)/3),3)) %>% #rowid makes sure that id_i, x_i, and y_i are all on row i
    dcast(time + rowid + half ~ variable2) %>%
    rename(globalplayerid = id) %>% 
    arrange(rowid) %>% select(-rowid)
  return(locs)
}

get_shots <- function(gameid=NA){
  
  play <- load_pbp(gameid)
  opt <- load_optical(gameid)
  shots <- get_shot_results(play)
  locs <- get_shot_locs(opt)
  
  second_half_start <- locs$time[locs$half == 2][1]
  
  shot_locs <- merge(shots, locs, by=c("time", "globalplayerid")) #%>% filter(globalteamid == 1388)
  
  shot_locs$x2 <- shot_locs$x - 25
  shot_locs$y2 <- shot_locs$y
  shot_locs$y2[shot_locs$time < second_half_start] = (94 - shot_locs$y2[shot_locs$time < second_half_start])
  shot_locs$y2 <- shot_locs$y2 - 4 #so the hoop is 0, not the baseline
  if(mean(shot_locs$y2) > 94/2){ #if the expected distance of the shots is beyond half court, then we probably just got the sides mixed up
    shot_locs$y2 <- shot_locs$y
    shot_locs$y2[shot_locs$time >= second_half_start] = (94 - shot_locs$y2[shot_locs$time >= second_half_start])
    shot_locs$y2 <- shot_locs$y2 - 4 #so the hoop is 0, not the baseline
  }
  shot_locs$r <- sqrt(shot_locs$x2^2 + shot_locs$y2^2)
  shot_locs$theta <- atan(shot_locs$y2/shot_locs$x2)
  shot_locs$gametime <- shot_locs$gameclock
  shot_locs$gametime[shot_locs$time < second_half_start] <- 2*shot_locs$gameclock[shot_locs$time < second_half_start]
  shot_locs$gametime <- 2400 - shot_locs$gametime
  shot_locs$gameid <- gameid
  return(shot_locs)
}

secs_to_ms <- function(x){
  x <- as.numeric(x)
  m <- x %/% 60
  s <- round(x %% 60,2)
  paste0(m,":",s)
}

