#####
# Set up environment
#####

edgerow = c(1,gridrows)
edgecol = c(1,gridcols)

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
  dplyr::select(dayofyear,sunrise_hour,sunset_hour) %>%
  filter(dayofyear>=90&dayofyear<=351) %>%
  mutate(dayofyear=c(1:262))

year=0
day=1
daytime = "night"

#####
# Set up agents
#####

ticks = c("larvae","nymphs","adult_males","adult_females")
tot_agents = eggs + larvae + nymphs + adult_males + adult_females + mice + deer + other
agents = data.frame(agents = c(rep("mice",mice),
                               rep("deer",deer),
                               rep("other",other),
                               rep("eggs",eggs),
                               rep("larvae",larvae),
                               rep("larvae",fedlarvae),
                               rep("nymphs",nymphs),
                               rep("adult_males",adult_males),
                               rep("adult_females",adult_females)))
agents$row = round(x = runif(n = nrow(agents),
                             min = 1,
                             max = gridrows),
                   digits=0)
agents$col = round(x = runif(n = nrow(agents),
                             min = 1,
                             max = gridcols),
                   digits=0)
agents$location = paste(agents$row,",",agents$col,sep = "")
agents$id = c(1:nrow(agents))
agents$links = 0
agents$time_on_host = 0
agents$fed=ifelse(agents$agents=="larvae",1,0)
agents$mated = 0
agents$sex = sample(c('male','female'),nrow(agents),replace=T)
agents$time_since_mating = 0
mouse_agents = agents %>%
  filter(agents == "mice")
deer_agents = agents %>%
  filter(agents == "deer")
other_agents = agents %>%
  filter(agents == "other")
tick_agents = agents %>%
  filter(agents%in%ticks)
egg_agents = agents %>%
  filter(agents=="eggs")
remove(agents)

pb = txtProgressBar(min = 0, max = go_timesteps,initial=0,style = 3)
