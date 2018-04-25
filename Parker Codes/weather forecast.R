rwunderground::set_api_key('651e75c69a9131c3')
key=get_api_key()
location=set_location(territory = 'Illinois', city = 'Chicago')

tenDayForecast <- function(){
  forecast <- forecast10day(location = location, key = key)
  forecast <- addCondition(forecast)
  return(forecast)
}

threeDayForecast <- function(){
  forecast <- forecast3day(location = location, key = key)
  forecast <- addCondition(forecast)
  return(forecast)
}

addCondition <- function(forecast){
  forecast$condition<-NA
  forecast$severity<-NA
  for (i in 1:length(forecast$date)){
    node<-forecast$cond[i]
    if (node =='Blowing Snow'){
      forecast$condition[i]<-('Snow')
      forecast$severity[i]<-('High')
    }
    else if (node == 'Clear'){
      forecast$condition[i]<-('Clear')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Drizzle'){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Fog'){
      forecast$condition[i]<-('Fog')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Freezing Rain'){
      forecast$condition[i]<-('Ice')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Haze'){
      forecast$condition[i]<-('Fog')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Heavy Rain'){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('High')
    }
    else if (node == 'Heavy Snow'){
      forecast$condition[i]<-('Snow')
      forecast$severity[i]<-('High')
    }
    else if (node == 'Heavy Thunderstorms and Rain'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('High')
    }
    else if (node == 'Heavy Thunderstorms with Small Hail'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('High')
    }
    else if (node == 'Ice Pellets'){
      forecast$condition[i]<-('Ice')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Drizzle'){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Freezing Drizzle'){
      forecast$condition[i]<-('Ice')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Freezing Fog'){
      forecast$condition[i]<-('Fog')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Freezing Rain'){
      forecast$condition[i]<-('Ice')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Hail'){
      forecast$condition[i]<-('Ice')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Ice Pellets'){
      forecast$condition[i]<-('Ice')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Rain'){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Rain Showers'){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Snow'){
      forecast$condition[i]<-('Snow')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Thunderstorms and Rain'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Light Thunderstorms and Hail'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Mist'){
      forecast$condition[i]<-('Fog')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Mostly Cloudy'){
      forecast$condition[i]<-('Clouds')
      forecast$severity[i]<-('High')
    }
    else if (node == 'Overcast'){
      forecast$condition[i]<-('Clouds')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Partly Cloudy'){
      forecast$condition[i]<-('Clouds')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Patches of Fog'){
      forecast$condition[i]<('Fog')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Rain'){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Scattered Clouds'){
      forecast$condition[i]<-('Clouds')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Shallow Fog'){
      forecast$condition[i]<-('Fog')
      forecast$severity[i]<-('Low')
    }
    else if (node == 'Smoke'){
      forecast$condition[i]<-('Fog')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Snow'){
      forecast$condition[i]<-('Snow')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Thunderstorm'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Thunderstorm and Ice Pellets'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Thunderstorm and Rain'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('Medium')
    }
    else if (node == 'Thunderstorm and Small Hail'){
      forecast$condition[i]<-('Thunderstorms')
      forecast$severity[i]<-('Medium')
    }
    else if (grepl('Chance',node) & grepl('Rain',node)){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('NA')
    }
    else if (grepl('Chance',node) & grepl('Snow',node)){
      forecast$condition[i]<-('Rain')
      forecast$severity[i]<-('NA')
    }
  }
  return(forecast)
}
