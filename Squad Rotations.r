#Using Premier League 2003/2004 season which is competition_id == 2 and season_id == 44

#This gets the number of matches per competition and season to check which season we have the most data for
#matches.count <- all.matches.clean %>% group_by(competition.competition_id,season.season_id) %>% summarise(count = n())

matches.epl.0304 <- all.matches.clean[which(all.matches.clean$competition.competition_id==2 & all.matches.clean$season.season_id==44),]
matches.epl.0304 <- matches.epl.0304[order(matches.epl.0304$match_week),]

epl.teams <- unique(matches.epl.0304$home_team.home_team_name) #Get the unique list of teams to loop through each team

squad.rotation.list <- list() #This list is for keeping track of the number of squad rotations per matchweek
team.starting.x11 <- list() #This list is for keeping track of the starting 11 for each match week
for(w in 1:length(epl.teams)){
  if(w==10 || w==11 || w==12 || w==13 || w==14 || w==15 || w==16) next #These teams have no rotation so step over
  squad.rotation.list[[epl.teams[w]]] <- list()
  team.starting.x11[[epl.teams[w]]] <- list()
  team.matches <- matches.epl.0304[which(matches.epl.0304$home_team.home_team_name==epl.teams[w] | 
                                           matches.epl.0304$away_team.away_team_name==epl.teams[w]),]
  team.matches$GD <- team.matches$home_score-team.matches$away_score
  
  team.events.index <- which(names(event.list) %in% team.matches$match_id)
  team.events <- event.list[team.events.index]
  team.id <- unique(matches.epl.0304[which(matches.epl.0304$home_team.home_team_name==epl.teams[w]),]$home_team.home_team_id)
  team.matches$Team.GD <- ifelse(team.matches$home_team.home_team_id==team.id,team.matches$GD,team.matches$GD*-1)
  team.matches$Result <- ifelse(team.matches$Team.GD>0,"W",
                                ifelse(team.matches$Team.GD==0,"D","L"))
  
  
  for(i in 1:length(team.events)){ #For each game of that particular team, get the starting 11 for them
    starting.x11 <- team.events[[i]][[1]]
    starting.x11.index <- which(lapply(starting.x11, function(x) unique(x$team_id))==team.id)
    
    team.11 <- starting.x11[[starting.x11.index]]
    team.starting.x11[[epl.teams[w]]][[i]] <- team.11$player.name
  }
  
  num.matches <- length(team.events)
  #All matches after the first, calculate the difference in players from matchweek X and matchweek X+1
  squad.rotation <- c(0,sapply(seq(1:(num.matches-1)),function(x) length(setdiff(team.starting.x11[[w]][[x]],team.starting.x11[[w]][[x+1]]))))
  team.matches$Rotated <- squad.rotation 
  squad.rotation.list[[w]] <- team.matches[,c("match_week","Result","Rotated")]
}

library("ggplot2")

result.colors <- c("W"="forestgreen","L"="red","D" = "yellow") #define a set of colors to use in our plot

#ggplot is where you bind the data. The aes stands for aesthetic and defines what data is bound to what part of the graph
#Team IDs:
#[1] "Arsenal"            "Middlesbrough"      "Manchester City"        
#[4] "Manchester United"  "Liverpool"          "Charlton Athletic"      
#[7] "Leeds United"       "Birmingham City"    "Bolton Wanderers"       
#[10] "Southampton"       "Aston Villa"       "Wolverhampton Wanderers"
#[13] "Chelsea"           "Blackburn Rovers"  "Tottenham Hotspur"      
#[16] "Fulham"
ggplot(data=squad.rotation.list[[5]], aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors)

all.squad.rotations <- plyr::ldply(squad.rotation.list,.id="Team") #Binds all the rows of the list elements together and adds the list element name as an additional column

ggplot(data=all.squad.rotations, aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors) + facet_grid(rows=vars(Team)) #Adds a plot for each team
