# Format the musiclab data to get it download by download
# rather than week by week
# the script produces two datasets newDfV1.saved and newDf.saved

library(haven)
library(dplyr)

rm(list = ls())

# Set working directory to where the original musiclab data is stored
setwd("")



# Select a version of the muscilab experiment (set v to 1 or 2)
for (v in 1:2){
  # Load the original data:
  ml<-read_dta(file = "musiclab.dta")
  
  if(v==1){
    ml<-ml[which(ml$version==1),]
  }else{
    ml<-ml[which(ml$version==2),]
  }
  
  ml<-ml%>%
    arrange(world,time)
  
  newDf<-c()
  for (w in unique(ml$world)){
    # Select worlds one by one
    world<-ml[which(ml$world==w),]
    
    # Create a df with one column per song
    ballcount <- rep(0,48)
    ballcount<-as.data.frame(t(ballcount))
    
    for(r in 1:nrow(world)){
      # Next song to be downloaded:
      song<-as.numeric(world[r,"song"])
      previousDownloads<-as.numeric(unname(ballcount[r,1:48]))
      
      # Add one download to that song and put it in the vector:
      previousDownloads[song]<-previousDownloads[song]+1
      
      # Add it to the bottom of the dataframe:
      ballcount<-rbind(ballcount,previousDownloads)
    }
    # Delete the first row that was just full of 0s
    ballcount<-ballcount[-1,]
    ballcount<-as.data.frame(ballcount)
    ballcount$world<-w
    ballcount$userId<-world$subjectid
    newDf<-rbind(newDf,ballcount)
  }
  
  if (v==1){
    save(newDf,file="newDfV1.saved")
  }else{
    save(newDf,file="newDf.saved")
  }
}

