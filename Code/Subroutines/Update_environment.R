update_enviro = function(i,daylight){
day_hour <<- (i%%24)+1
if(day_hour==1){day <<- day+1}
if(day==366){day <<- 1}
if(day==1&day_hour==1){year <<- year+1}
daytime <<- ifelse(day_hour >= daylight[which(daylight$dayofyear==(day%%262)+1),2]&
                   day_hour <daylight[which(daylight$dayofyear==(day%%262)+1),3],
                 "day",'night')
if(day==79){season <<- "spring"}
if(day==171){season <<- "summer"}
if(day==265){season <<- "fall"}
if(day==355){season <<- "winter"}
#if((i%%8760<2160)|(i%%8760>=8424)){season='winter'} cut out winter
# if(i%%6264>0&i%%6264<=1464){season <<- 'spring'}
# if(i%%6265>=1465&i%%6265<3266){season <<- 'summer'}
# if(i%%6265>=3266&i%%6265<6265){season <<- 'fall'}
# if(i%%6265==0){season <<- 'winter'}
}
