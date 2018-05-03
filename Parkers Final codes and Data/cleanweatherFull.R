#Set path and working directory for loading and saving
path<-"~/Documents/Math 571 project/Parkers Final codes and Data"
setwd(path)
load("weatherFull.Rda")

#Remove attributes with many missing values
weatherFull$precip_rate<-NULL
weatherFull$precip_total<-NULL
weatherFull$precip<-NULL
weatherFull$heat_index<-NULL
weatherFull$wind_chill<-NULL
weatherFull$wind_gust<-NULL

#Add new attributes condition and severity initailzed to NA
weatherFull[,'condition']<-NA
weatherFull[,'severity']<-NA

#Condition Buckets: Snow, rain, fog, sun, clouds, fog, thunderstorm, Clear, NA
#Severity Buckets: Low, Medium, High
#For loop to get the condition and bucket it for every record
for (i in 1:length(weatherFull$date)){
  node<-weatherFull$cond[i]
  if (node =='Blowing Snow'){
    weatherFull$condition[i]<-('Snow')
    weatherFull$severity[i]<-('High')
  }
  else if (node == 'Clear'){
    weatherFull$condition[i]<-('Clear')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Drizzle'){
    weatherFull$condition[i]<-('Rain')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Fog'){
    weatherFull$condition[i]<-('Fog')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Freezing Rain'){
    weatherFull$condition[i]<-('Ice')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Haze'){
    weatherFull$condition[i]<-('Fog')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Heavy Rain'){
    weatherFull$condition[i]<-('Rain')
    weatherFull$severity[i]<-('High')
  }
  else if (node == 'Heavy Snow'){
    weatherFull$condition[i]<-('Snow')
    weatherFull$severity[i]<-('High')
  }
  else if (node == 'Heavy Thunderstorms and Rain'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('High')
  }
  else if (node == 'Heavy Thunderstorms with Small Hail'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('High')
  }
  else if (node == 'Ice Pellets'){
    weatherFull$condition[i]<-('Ice')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Drizzle'){
    weatherFull$condition[i]<-('Rain')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Freezing Drizzle'){
    weatherFull$condition[i]<-('Ice')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Freezing Fog'){
    weatherFull$condition[i]<-('Fog')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Freezing Rain'){
    weatherFull$condition[i]<-('Ice')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Hail'){
    weatherFull$condition[i]<-('Ice')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Ice Pellets'){
    weatherFull$condition[i]<-('Ice')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Rain'){
    weatherFull$condition[i]<-('Rain')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Rain Showers'){
    weatherFull$condition[i]<-('Rain')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Snow'){
    weatherFull$condition[i]<-('Snow')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Thunderstorms and Rain'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Light Thunderstorms and Hail'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Mist'){
    weatherFull$condition[i]<-('Fog')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Mostly Cloudy'){
    weatherFull$condition[i]<-('Clouds')
    weatherFull$severity[i]<-('High')
  }
  else if (node == 'Overcast'){
    weatherFull$condition[i]<-('Clouds')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Partly Cloudy'){
    weatherFull$condition[i]<-('Clouds')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Patches of Fog'){
    weatherFull$condition[i]<('Fog')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Rain'){
    weatherFull$condition[i]<-('Rain')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Scattered Clouds'){
    weatherFull$condition[i]<-('Clouds')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Shallow Fog'){
    weatherFull$condition[i]<-('Fog')
    weatherFull$severity[i]<-('Low')
  }
  else if (node == 'Smoke'){
    weatherFull$condition[i]<-('Fog')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Snow'){
    weatherFull$condition[i]<-('Snow')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Thunderstorm'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Thunderstorm and Ice Pellets'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Thunderstorm and Rain'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('Medium')
  }
  else if (node == 'Thunderstorm and Small Hail'){
    weatherFull$condition[i]<-('Thunderstorms')
    weatherFull$severity[i]<-('Medium')
  }
}

#remove records with NAs
weatherFull<-weatherFull[complete.cases(weatherFull),]

#save the data
save(weatherFull,file = 'cleanWeatherFull.Rda')