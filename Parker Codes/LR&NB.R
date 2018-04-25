load("~/Documents/Math 571 project/UC_FINAL_AGG.Rda")
load("~/Documents/Math 571 project/IIT_FINAL_AGG.Rda")


weatherFull$condition[i]<-('Thunderstorms')
weatherFull$severity[i]<-('High')

for (i in 1:length(IIT_FINAL_AGG$OCCURED)){
  if(is.na(IIT_FINAL_AGG$STAND_COND[i])){
    node<-IIT_FINAL_AGG$COND[i]
    if (!is.na(node)){
      if(node == 'Thunderstorms and Rain'){
        IIT_FINAL_AGG$STAND_COND[i]<-('Thunderstorms')
        IIT_FINAL_AGG$SEVERITY[i]<-('Medium')
      }
      else if (node == 'Patches of Fog'){
        IIT_FINAL_AGG$STAND_COND[i]<-('Fog')
        IIT_FINAL_AGG$SEVERITY[i]<-('Low')
      }
    }
  }
}

for (i in 1:length(UC_FINAL_AGG$OCCURED)){
  if(is.na(UC_FINAL_AGG$STAND_COND[i])){
    node<-UC_FINAL_AGG$COND[i]
    if (!is.na(node)){
      if(node == 'Thunderstorms and Rain'){
        UC_FINAL_AGG$STAND_COND[i]<-('Thunderstorms')
        UC_FINAL_AGG$SEVERITY[i]<-('Medium')
      }
      else if (node == 'Patches of Fog'){
        UC_FINAL_AGG$STAND_COND[i]<-('Fog')
        UC_FINAL_AGG$SEVERITY[i]<-('Low')
      }
      else if (node == 'Thunderstorms with Small Hail'){
        UC_FINAL_AGG$STAND_COND[i]<-('Thunderstorms')
        UC_FINAL_AGG$SEVERITY[i]<-('Low')
      }
    }
  }
}