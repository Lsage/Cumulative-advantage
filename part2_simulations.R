# Polya Urn world 9, graph for the 48 songs 
library(dplyr)
library(ggplot2)
library(RColorBrewer)


setwd("")

rm(list = ls())

# Select a version of the experiment: set version ==1 or 2
version<-1
if(version==1){
  load("newDfV1.saved")
}else{
  load("newDf.saved")
}


# Need a function to attribute a rank randomly for two songs which have the same number of downloads:
random_rank <- function(x) {
  rank_values <- rank(x, ties.method = "first")
  duplicates <- which(duplicated(rank_values))
  
  for (i in duplicates) {
    same_rank <- which(rank_values == rank_values[i])
    rank_values[same_rank] <- rank_values[same_rank] + runif(length(same_rank), -0.4, 0.4)
  }
  return(rank_values)
}

# Colors for the graphs:
dark2_palette <- brewer.pal(8, "Dark2")


# Parameters for simulation:
N<-48                                     # Number of colors/songs
col<-1:N                                  # Number of columns in the sim data
numberSim<-2000                           # Number of simulations 
breaks<-4                                 # Number of intervals (time)
feedback<-ifelse(version==1,0.295,0.31)   # Estimated parameter

set.seed(1457831)                         # Seed to reproduce exactly the graphs of the paper

# Stats collected in each simulation:
allSongsTs<-c()  # All songs at different time steps

data<-newDf[which(newDf$world==9),]

# Create a variable that will tell whether user is the same as the previous download
data<-data%>%
  group_by(world)%>%
  mutate(previousUser=lag(userId))

# replace NAs by a value
data$previousUser<-ifelse(is.na(data$previousUser),999999,data$previousUser)

# Select the world 9:
d<-newDf[which(newDf$world==9),]

# Get the number of downloads per week per song in the empirical data: 
d<-d[,1:48]
totals<-as.numeric(d[nrow(d),])

# Total number of downloads in the world:
totDown <- sum(totals)

# Number of iteration we will need for this world:
for(m in 1:numberSim){
  # To store stats:
  ballcount<-matrix(0, nrow = totDown, ncol = N)
  
  # Case where we simulate a fixed probability model
  # Go through each row  
  for (i in 1: (totDown)){
    # Draw one song with fixed probas assigned based on empirical values
    if (i==1){
      previousDownloads<-rep(1,N)
    }else{
      previousDownloads<-unname(ballcount[i-1,])
    }
    if (data$userId[i]!=data$previousUser[i]){
      # There is a change of users so all colors should be available again
      # All songs are assigned the empirical original probability of being drawn 
      colorsAvailable<-previousDownloads
    }
    colorSelected<-sample(col,1,replace=TRUE,prob=colorsAvailable/sum(colorsAvailable))
    previousDownloads[colorSelected]<-previousDownloads[colorSelected]+feedback
    # If the next user is the same as for this round, then the proba should be equal to 0
    colorsAvailable[colorSelected]<-0
    ballcount[i,]<-previousDownloads
  }
  # Originally all songs had 0 download to need to substract 1 everywhere:
  ballcount<-ballcount-1
  
  # convert into the actual number of download (must be an integer)
  ballcount<-ballcount/feedback
  ballcount<-as.data.frame(ballcount)
  
  # Here we impose the length of the time steps with parameter breaks:
  ballcount<-ballcount[c(cumsum(rep(floor(totDown/breaks),breaks-1)),totDown),]
  ballcount<-as.data.frame(unname(t(ballcount)))
  
  new<-ballcount
  for (i in 2:breaks) {
    # Take the difference between number of downloads in two consecutive intervals
    # (net/additional number of downloads)
    new[, i] <- ballcount[, i] - ballcount[, i-1]
  }
  ballcount<-new
  rm(new)
  
  # Rank songs:
  ballcount<-ballcount%>%
    mutate(rank1=random_rank(desc(V1)),
           rank2=random_rank(desc(V2)),
           rank3=random_rank(desc(V3)),
           rank4=random_rank(desc(V4)))
  ballcount$sim<-m
  allSongsTs<-bind_rows(allSongsTs,ballcount) 
}  

# Arrange the empirical data in the same way as the simulated data
data<-data[c(cumsum(rep(floor(nrow(data)/breaks),breaks-1)),nrow(data)),]
empi<-as.data.frame(t(unname(as.matrix(data[,1:48]))))
empi<-empi%>%
  mutate(V4=V4-V3,
         V3=V3-V2,
         V2=V2-V1)

# Arrange the simulated data (4 time steps) to prepare for the graph
tot<-c()
for (u in 1:breaks){
  
  small<-allSongsTs%>%
    group_by(!!sym(paste0("rank", u)))%>%
    summarise(downloads=mean(!!sym(paste0("V", u))),
              d_05=unname(quantile(!!sym(paste0("V", u)), probs = 0.025)),
              d_95=unname(quantile(!!sym(paste0("V", u)), probs = 0.975)))
  names(small)[1]<-"rank"
  
  small$time<-u
  
  small<-small%>%
    mutate(percentage=(downloads/unname(colSums(empi))[u])*100,
           perc_05=(d_05/unname(colSums(empi))[u])*100,
           perc_95=(d_95/unname(colSums(empi))[u])*100)
  
  tot<-bind_rows(tot,small)
}

empirical1<-sort(as.numeric((empi$V1/sum(empi$V1))*100),decreasing=TRUE)
empirical2<-sort(as.numeric((empi$V2/sum(empi$V2))*100),decreasing=TRUE)
empirical3<-sort(as.numeric((empi$V3/sum(empi$V3))*100),decreasing=TRUE)
empirical4<-sort(as.numeric((empi$V4/sum(empi$V4))*100),decreasing=TRUE)

tot$empirical<-c(empirical1,empirical2,empirical3,empirical4)

rm(empirical1,empirical2,empirical3,empirical4)

tot$t[tot$time==1]<-"Time step 1"
tot$t[tot$time==2]<-"Time step 2"
tot$t[tot$time==3]<-"Time step 3"
tot$t[tot$time==4]<-"Time step 4"


# Draw the figure
g<-
  ggplot(tot, aes(x = rank)) +
  facet_wrap(~t)+
  geom_point(aes(y = percentage, color = "Wrong model", shape = "Wrong model"), size = 2) +
  geom_errorbar(
    aes(y = percentage, ymin = perc_05, ymax = perc_95, color = "Wrong model"),
    width = 0
  ) +
  geom_point(aes(y = empirical, color = "Empirical", shape = "Empirical"), size = 2) +
  
  labs(
    x = "Song rank",
    y = "Percentage of new downloads at time step"
  ) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6,7)) +
  scale_x_continuous(breaks = c(1,12,24,36,48)) +
  
  scale_shape_manual(
    values = c("Empirical" = 19, "Wrong model" = 17),
    name = ""
  ) +
  scale_color_manual(
    values = c("Empirical" = dark2_palette[3], "Wrong model" = dark2_palette[1]),
    guide = guide_legend(title = "")
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_line(linewidth = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(color = "white", face = "bold"),
    strip.background = element_rect(fill = "grey70")  
  )

if(version==1){
  ggsave("/Images/All_songs_empi_vs_simulated_V1.jpeg",
         plot=g, device = "jpeg",
         width = 20, height = 15, units = "cm")
}else{
  ggsave("Images/All_songs_empi_vs_simulated_V2.jpeg",
         plot=g, device = "jpeg",
         width = 20, height = 15, units = "cm")
}

