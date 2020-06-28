library(rjson)
library(data.table)
library(magrittr)
library(dplyr)
library(knitr)

####Obtain Competitions####

#Read File from JSON into a list 
competitions <- fromJSON(file="C:\\Users\\ryan_\\OneDrive\\Desktop\\open-data-master\\data\\competitions.json")

#Convert List into a DataFrame
competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)

####Obtain Matches####
match.files <- list.files(path="C:\\Users\\ryan_\\OneDrive\\Desktop\\open-data-master\\data\\matches",
                          full.names = TRUE,recursive = TRUE)

matches.list <- list()
for(i in 1:length(match.files)){
  match.temp <- fromJSON(file=match.files[i]) ##Loop through each file which contains all the matches for a given competition and season and obtain the necessary match information
  
  matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
  matches.df <- rbindlist(matches,fill=TRUE) #Use rbindlist instead of do.call(rbind,) because of column mismatch
  matches.list[[i]] <- matches.df 
  
}

all.matches.df <- data.frame(rbindlist(matches.list,fill=TRUE)) ###Combines all matches from all competitions into one dataframe

###Remove columns that contain NA so the dataset is clean
columns.to.keep <- names(which(unlist(lapply(all.matches.df,function(x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep] #Selects the columns by column name 
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week)
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)

####Obtain Events####
event.files <- list.files(path="C:\\Users\\ryan_\\OneDrive\\Desktop\\open-data-master\\data\\events",
                          full.names = TRUE,recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file=event.files[i])
  
  #unique(unlist(lapply(event.temp,function(x) x$type$name))) | Let's us see the unique events that happen in a game
  
  teamids <- c() #Get the unique teamids participating in a match
  
  #Obtain index where we find the event about Starting XI
  starting.x11.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Starting XI")
  starting.x11.list <- list()
  for(s in 1:2){
    starting.x11.team1 <- data.frame(matrix(t(unlist(event.temp[[s]]$tactics$lineup)),ncol=5,byrow = TRUE),stringsAsFactors = FALSE)
    colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup))[1:5]
    starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
    starting.x11.team1$team_id <- event.temp[[s]]$team$id
    
    teamids <- c(teamids,event.temp[[s]]$team$id)
    
    starting.x11.team1$team_name <- event.temp[[s]]$team$name
    starting.x11.list[[s]] <- starting.x11.team1
  }
  
  pass.index <- which(unlist(lapply(event.temp,function(x) x$type$name))=="Pass")
  
  #Obtain the passes just for team1 (the first element in teamids)
  pass.team1 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[1])]
  
  pass.team1.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(pass.team1.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team1)){
    pass.temp <- event.temp[[pass.team1[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
    pass.team1.df <- rbind(pass.team1.df,row.toadd)
  }
  pass.team1.df <- pass.team1.df[-1,]
  pass.team1.df[,c(1:4,6:10)] <- lapply(pass.team1.df[,c(1:4,6:10)],as.numeric)
  
  pass.team1.df <- pass.team1.df %>% group_by(Possession) %>% mutate(seq = row_number())
  pass.team1.df$team_id <- teamids[1]
  
  pass.team2 <- pass.index[which(unlist(lapply(pass.index,function(x) event.temp[[x]]$team$id))==teamids[2])]
  pass.team2.df <- data.frame(matrix(NA,nrow=1,ncol=11))
  colnames(pass.team2.df) <- c("Possession","Passer","X.Pass","Y.Pass",
                               "Pass.Type","Receiver","X.Receive","Y.Receive",
                               "Pass.Length","Pass.Angle","Body.Part")
  
  for(p in 1:length(pass.team2)){
    pass.temp <- event.temp[[pass.team2[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession,passer,pass.location,pass.type,receiver,receive.location,pass.length,pass.angle,body.part)
    pass.team2.df <- rbind(pass.team2.df,row.toadd)
  }
  pass.team2.df <- pass.team2.df[-1,]
  pass.team2.df[,c(1:4,6:10)] <- lapply(pass.team2.df[,c(1:4,6:10)],as.numeric)
  pass.team2.df <- pass.team2.df %>% group_by(Possession) %>% mutate(seq = row_number())
  pass.team2.df$team_id <- teamids[2]
  
  pass.list <- list(pass.team1.df,pass.team2.df)
  
  match.id <- strsplit(basename(event.files[i]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- list(starting.x11.list,pass.list)
  
}