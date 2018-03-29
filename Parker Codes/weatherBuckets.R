Crime_2014toPres_IIT_area$bucket<-NA
Crime_2014toPres_UC_area$bucket<-NA
iitCrime$bucket<-NA
Uchicago_campus_crimes_cleaned$bucket<-NA

#6 hour buckets, 0-6, 6-12, 12-18, 18-24
for (i in 1:length(iitCrime$Incident)){
  if (strftime(iitCrime$Occured[i])<6:00:00){
    iitCrime$bucket[i]<-1
  }
  else if (strftime(iitCrime$Occured[i])<12:00:00){
    iitCrime$bucket[i]<-2
  }
  else if (strftime(iitCrime$Occured[i])<18:00:00){
    iitCrime$bucket[i]<-3
  }
  else if (strftime(iitCrime$Occured[i])<24:00:00){
    iitCrime$bucket[i]<-4
  }
}