daylight=getSunlightTimes(date=seq.Date(from=as.Date("2018-01-01"),
                                        to=as.Date("2018-12-31"),by=1),
                          tz="EST",
                          lat=43.0481,lon = -76.1474) %>%
  dplyr::select(sunrise,sunset) %>%
  mutate(day = as.numeric(substring(.$sunrise,9,10)),
         sunrise = substring(.$sunrise,12,16),
         sunrise_hour = as.numeric(substring(.$sunrise,12,13)),
         sunrise_minute = as.numeric(substring(.$sunrise,15,16)),
         sunset = substring(.$sunset,12,16),
         sunset_hour = as.numeric(substring(.$sunset,12,13)),
         sunset_minute = as.numeric(substring(.$sunset,15,16)),
         dayofyear = 1:nrow(.)) %>%
  mutate(sunrise_hour = ifelse(.$sunrise_minute>30,.$sunrise_hour+1,.$sunrise_hour),
         sunrise_minute = ifelse(.$sunset_minute>30,.$sunset_minute+1,.$sunset_minute)) %>%
  dplyr::select(dayofyear,sunrise_hour,sunset_hour) #%>%
  #filter(dayofyear>=90&dayofyear<=351) %>%
  #mutate(dayofyear=c(1:262))
