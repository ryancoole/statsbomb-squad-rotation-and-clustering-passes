#Cluster passes can help understand the passes tendencies and lanes for each team
#We will use Premier League 2003/2004 again

pass.events.index <- which(names(event.list) %in% matches.epl.0304$match_id)

passes.list <- list()
for(i in 1:length(pass.events.index)){
  match.temp <- event.list[[pass.events.index[i]]][[2]]
  
  all.passes <- do.call(rbind,match.temp)
  
  all.passes.locations <- all.passes[,c("team_id","X.Pass","Y.Pass","X.Receive","Y.Receive")]
  
  passes.list[[i]] <- all.passes.locations
  
}

full.pass.df <- do.call(rbind,passes.list)
full.pass.df <- full.pass.df[which(full.pass.df$Y.Receive<=80),] #Cleaning the data
full.pass.df$Y.Pass <- 80 - full.pass.df$Y.Pass #Changing the axis so that origin starts at the lower left corner
full.pass.df$Y.Receive <- 80 - full.pass.df$Y.Receive

library(parallel)
library(ggplot2)

#Perform k-means on the dataset (removing the 1st column because we just need to use the last 4 columns in our analysis)
mc = mclapply(c(25,50,75), function(x,centers) kmeans(x, centers, iter.max=1000), x=full.pass.df[,-1])

full.pass.df$Cluster.25 <- mc[[1]]$cluster #created clusters using 25 clusters
full.pass.df$Cluster.50 <- mc[[2]]$cluster #created clusters using 50 clusters
full.pass.df$Cluster.75 <- mc[[3]]$cluster #created clusters using 75 clusters

cluster.75.summary <- full.pass.df %>% group_by(Cluster.75) %>% summarise(X.Pass = mean(X.Pass),Y.Pass = mean(Y.Pass),
                                                                          X.Receive = mean(X.Receive), Y.Receive = mean(Y.Receive),
                                                                          count = n()) #Obtain for each cluster id, the average location of the pass

cluster.75.team.summary <- full.pass.df %>% group_by(Cluster.75,team_id) %>% summarise(count = n()) #get a count per team
liverpool.clusters <- cluster.75.team.summary %>% group_by(Cluster.75) %>% mutate(z.score = (count - mean(count))/sd(count)) %>%
  filter(team_id == 24 & z.score >= 1.5) #Identify clusters that liverpool do more than 1.5 standard deviation than the league average

source("C:\\Users\\ryan_\\OneDrive\\Desktop\\open-data-master\\Draw_Pitch.R") #Load in hori5, which contains the field

hori5 + geom_segment(data=cluster.75.summary, aes(x=X.Pass,xend=X.Receive,
                                                  y=Y.Pass,yend=Y.Receive,color=count),size=1.5,arrow=arrow(length = unit(0.03, "npc"))) +
  geom_text(data=cluster.75.summary,aes(x=X.Pass,y=Y.Pass,label=Cluster.75))

hori5 + geom_segment(data=cluster.75.summary, aes(x=X.Pass,xend=X.Receive,
                                                  y=Y.Pass,yend=Y.Receive),size=1.5,arrow=arrow(length = unit(0.03, "npc"))) +
  geom_segment(data=cluster.75.summary[which(cluster.75.summary$Cluster.75 %in% liverpool.clusters$Cluster.75),], aes(x=X.Pass,xend=X.Receive,
                                                                                                                    y=Y.Pass,yend=Y.Receive),size=1.5,color="red",arrow=arrow(length = unit(0.03, "npc")))
