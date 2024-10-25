library(dplyr)
library(tidyr)
library(ggplot2)
library(suncalc)
# Halsey and Miller (2018) parameters: ------------------------------------
#   Fecundity (eggs per female)
#   Egg mortality rate
#   Egg hatch to larvae
#   larvae to Nymph
#   Nymph to Adult
#   Female adult to pre-oviposition
#   Feed time:
#     Larvae
#     Nymphs
#     Adults
#   Unfed tick death rate in forest habitat:
#     Tick age < 40 weeks
#       Larvae Nymphs Adults
#     Tick age > 40 weeks
#       Larvae Nymphs Adults
#   Replete ticks death rate in forest habitat
#     Larvae Nymphs Adults
#   Probability of attaching to host
#     Larvae Nymphs Adults
#   Molting Success
#     Larvae Nymphs Adults
#   Grooming rate
#     White-footed mouse
#     White-tailed deer
#   Grooming survival

# Setup -------------------------------------------------------------------
rm(list=ls())
gridrows = 64
gridcols = 64
edgerow = c(1,gridrows)
edgecol = c(1,gridcols)
# create agents
eggs = 100
larvae = 0
fedlarvae = 100 # to start the model
nymphs = 0
adult_males = 20
adult_females = 20
mice = 12
deer = 2
ticks = c("larvae","nymphs","adult_males","adult_females")
tot_agents = eggs + larvae + nymphs + adult_males + adult_females + mice + deer
agents = data.frame(agents = c(rep("mice",mice),
                               rep("deer",deer),
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
host_agents = agents %>%
  filter(agents=="deer"|agents=="mice")
tick_agents = agents %>%
  filter(agents%in%ticks)
egg_agents = agents %>%
  filter(agents=="eggs")
remove(agents)
test=as.Date("2018-01-01 00:00:00",tz='EST')
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
# Model -------------------------------------------------------------------
go_timesteps = 1000
pb = txtProgressBar(min = 0, max = go_timesteps,initial=0,style = 3)
for(i in 1:go_timesteps){# 
# Environment setup -------------------------------------------------------
  day_hour = (i%%24)+1
  if(day_hour==1){day=day+1}
  
  daytime = ifelse(day_hour >= daylight[which(daylight$dayofyear==(day%%262)+1),2]&
                     day_hour <daylight[which(daylight$dayofyear==(day%%262)+1),3],
                   "day",'night')
  if(i%%6265==1){year=year+1}
  #if((i%%8760<2160)|(i%%8760>=8424)){season='winter'} cut out winter
  if(i%%6264>0&i%%6264<=1464){season='spring'}
  if(i%%6265>=1465&i%%6265<3266){season='summer'}
  if(i%%6265>=3266&i%%6265<6265){season='fall'}
  if(i%%6265==0){season=='winter'}

# Host movement -----------------------------------------------------------
  if(daytime=="day"){host_agents = host_agents %>%
    mutate(possibilities = case_when(row==gridrows & !col%in%edgecol ~ 2,
                                     row==1 & !col%in%edgecol ~ 3,
                                     col==gridcols & !row%in%edgerow ~ 4,
                                     col==1 & !row%in%edgerow ~ 5,
                                     row==gridrows & col==gridcols ~ 6,
                                     row==gridrows & col==1 ~ 7,
                                     row==1 & col==gridcols ~ 8,
                                     row==1 & col==1 ~ 9,
                                     TRUE ~ 1),
           movement = case_when(possibilities==2 ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
                                possibilities==3 ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
                                possibilities==4 ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
                                possibilities==5 ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
                                possibilities==6 ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
                                possibilities==7 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
                                possibilities==8 ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
                                possibilities==9 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
                                TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
           row = case_when(movement==1 ~ row-1,
                           movement==2 ~ row-1,
                           movement==3 ~ row-1,
                           movement==7 ~ row+1,
                           movement==8 ~ row+1,
                           movement==9 ~ row+1,
                           TRUE ~ row),
           col = case_when(movement==1 ~ col-1,
                           movement==3 ~ col+1,
                           movement==4 ~ col-1,
                           movement==6 ~ col+1,
                           movement==7 ~ col-1,
                           movement==9 ~ col+1,
                           TRUE ~ col),
           location = paste(row,",",col,sep = ""))}

# Tick attachment ---------------------------------------------------------
if(season!='winter'){ 
  mice_df_nest = host_agents %>%
    filter(agents=='mice') %>%
    group_by(location) %>%
    summarize(id = list(id))
  
  deer_df_nest = host_agents %>%
    filter(agents=='deer') %>%
    slice(rep(1:n(),each=9)) %>%
    mutate(newid = rep(1:9,length(unique(.$id))),
           row = case_when(newid==1|newid==2|newid==3 ~ as.numeric(row-1),
                           newid==7|newid==8|newid==9 ~ as.numeric(row+1),
                           TRUE ~ as.numeric(row)),
           col = case_when(newid==1|newid==4|newid==7 ~ as.numeric(col-1),
                           newid==3|newid==6|newid==9 ~ as.numeric(col+1),
                           TRUE ~ as.numeric(col)),
           location = paste(row,",",col,sep="")) %>%
    dplyr::select(-newid) %>%
    group_by(location) %>%
    summarize(id = list(id))
  
  safe_sample = function(x, ...) {
    if(length(x)==1) return(x)
    sample(x, ...)
  }
if(length(which(tick_agents$agents=="adult_females" |
                  tick_agents$agents=="adult_males"))==0){
  tick_agents = tick_agents %>%
    filter(agents == "larvae" | agents == "nymphs") %>%
    mutate(id2 = id) %>%
    dplyr::select(-id) %>%
    left_join(mice_df_nest,by='location') %>%
    mutate(links = case_when(links>0 | fed==1 ~ as.integer(links),
                             links==0 ~ sapply(id, \(x) if(is.null(x)) 0L else safe_sample(x, size =1)),
                             TRUE ~ as.integer(0))) %>%
    dplyr::select(-id)
  tick_agents=tick_agents[,c(1:4,11,5:10)]
  names(tick_agents)[5]="id"}
if(length(which(tick_agents$agents=="adult_females" |
                   tick_agents$agents=="adult_males"))>0){
  tick_agents = tick_agents %>%
    filter(agents == "larvae" | agents == "nymphs") %>%
    mutate(id2 = id) %>%
    dplyr::select(-id) %>%
    left_join(mice_df_nest,by='location') %>%
    mutate(links = case_when(links>0 | fed==1 ~ as.integer(links),
           links==0 ~ sapply(id, \(x) if(is.null(x)) 0L else safe_sample(x, size =1)),
           TRUE ~ as.integer(0))) %>%
    dplyr::select(-id) %>%
    bind_rows(.,
              tick_agents %>%
                filter(agents == "adult_females" | agents == "adult_males") %>%
                mutate(id2 = id) %>%
                dplyr::select(-id) %>%
                left_join(deer_df_nest,by='location') %>%
                mutate(links = case_when(links>0 | fed==1 ~ as.integer(links),
                                         links==0 ~ sapply(id, \(x) if(is.null(x)) 0L else safe_sample(x, size =1)),
                                         TRUE ~ as.integer(0))) %>%
                dplyr::select(-id))
    tick_agents=tick_agents[,c(1:4,11,5:10)]
  names(tick_agents)[5]="id"}
}
# Tick timer -----------------------------------------------------------
tick_agents$time_on_host=ifelse(tick_agents$links>0,tick_agents$time_on_host+1,
                                tick_agents$time_on_host)

# Tick movement -----------------------------------------------------------
 if(day_hour==12&day%%2==0){tick_agents = tick_agents %>%
   filter(fed==0 & mated == 0) %>%
   mutate(possibilities = case_when(row==gridrows & !col%in%edgecol ~ 2,
                                    row==1 & !col%in%edgecol ~ 3,
                                    col==gridcols & !row%in%edgerow ~ 4,
                                    col==1 & !row%in%edgerow ~ 5,
                                    row==gridrows & col==gridcols ~ 6,
                                    row==gridrows & col==1 ~ 7,
                                    row==1 & col==gridcols ~ 8,
                                    row==1 & col==1 ~ 9,
                                    TRUE ~ 1),
          movement = case_when(possibilities==2 ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
                               possibilities==3 ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
                               possibilities==4 ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
                               possibilities==5 ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
                               possibilities==6 ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
                               possibilities==7 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
                               possibilities==8 ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
                               possibilities==9 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
                               TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
          row = case_when(movement==1 ~ row-1,
                          movement==2 ~ row-1,
                          movement==3 ~ row-1,
                          movement==7 ~ row+1,
                          movement==8 ~ row+1,
                          movement==9 ~ row+1,
                          TRUE ~ row),
          col = case_when(movement==1 ~ col-1,
                          movement==3 ~ col+1,
                          movement==4 ~ col-1,
                          movement==6 ~ col+1,
                          movement==7 ~ col-1,
                          movement==9 ~ col+1,
                          TRUE ~ col),
          location = paste(row,",",col,sep = "")) %>%
   dplyr::select(-c(possibilities, movement)) %>%
   bind_rows(., tick_agents %>%
               filter(fed==1 | mated==1))}
   
 # tick_agents = tick_agents %>%
 #    left_join(host_agents %>%
 #                mutate(location2 = location) %>%
 #                dplyr::select(id,location2),by=c("links"='id')) %>%
 #    mutate(location = case_when(links>0~location2,
 #                                TRUE ~ location),
 #           row = as.numeric(gsub(",.*$","",x=location)),
 #           col = as.numeric(sub('.*\\,',"",x=location))) %>%
 #    dplyr::select(-location2)
# Tick mating -------------------------------------------------------------
if(season!="winter"){  
  male_links = tick_agents %>%
    filter(agents=="adult_males",
           links>0&mated==0) %>%
    dplyr::select(links)
  
  female_links = tick_agents %>%
    filter(agents=="adult_females",
           links>0&mated==0) %>%
    dplyr::select(links)
  
  tick_agents = tick_agents %>%
    mutate(mated = case_when(agents=="adult_females"&
                               links>0&
                               links%in%male_links$links&
                               mated==0 ~ as.numeric(1),
                             agents=="adult_males"&
                               links >0&
                               links%in%female_links$links&
                               mated==0 ~ as.numeric(1),
                             TRUE ~ as.numeric(mated)),
           time_since_mating = case_when(mated==1 ~ as.numeric(time_since_mating)+1,
                                         TRUE ~ as.numeric(time_since_mating)))}
  
  # Notes: How many females can a male mate with?
  
# Tick de-attachment ------------------------------------------------------
if(season!='winter'){
  tick_agents = tick_agents %>%
    left_join(host_agents %>%
                mutate(location2 = location) %>%
                dplyr::select(id,location2),by=c("links"='id')) %>%
    mutate(fed = case_when(agents=="larvae" & time_on_host==120 ~ as.numeric(1),
                           agents=="nymphs" & time_on_host==120 ~ as.numeric(1),
                           agents=="adult_males" & (time_on_host==240 | mated==1) ~ as.numeric(1),
                           agents=="adult_females" & time_since_mating == 168 ~ as.numeric(1),
                           agents=="adult_females" & mated==0 & time_on_host==240 ~ as.numeric(1),
                           TRUE ~ as.numeric(fed)),
           location = case_when(links>0~location2,
                                TRUE ~ location),
           row = as.numeric(gsub(",.*$","",x=location)),
           col = as.numeric(sub('.*\\,',"",x=location))) %>%
    dplyr::select(-location2) %>%
    mutate(links = case_when(fed == 1 ~ as.numeric(0),
                             TRUE ~ as.numeric(links)),
           time_on_host = case_when(fed==1 & links == 0 ~ as.numeric(0),
                                    TRUE ~ as.numeric(time_on_host)))
  
  }
# Notes: How long will a male stay on a deer in search of a mate?
  
# Lay eggs,molt, kill ticks  ----------------------------------
  
if(season=="spring"|season=="fall"){  
  egg_agents = tick_agents %>%
    filter(agents == "adult_females" & fed == 1 & mated == 1) %>%
    mutate(agents = "eggs",
           id = max(c(host_agents$id,tick_agents$id,egg_agents$id))+1,
           fed = 1,
           mated = 1,
           sex = "egg",
           time_since_mating = 0) %>%
    bind_rows(.,egg_agents)
  tick_agents = tick_agents %>%
    filter(!(agents=="adult_males" & mated==1 & fed==1)) %>%
    filter(!(agents=="adult_females" & mated==1 & fed==1))} # adults lay eggs

if(i%%6265==1465){tick_agents = egg_agents %>%
  mutate(agents='larvae',
         fed=0,
         mated=0,
         time_on_host=0) %>%
  slice(rep(1:n(),each=10)) %>%
  mutate(id = (max(c(tick_agents$id,egg_agents$id,host_agents$id))+1):
           (max(c(tick_agents$id,egg_agents$id,host_agents$id))+nrow(.)),
         sex = sample(c('male','female'),nrow(.),replace=T)) %>%
  bind_rows(.,tick_agents)  %>%
  filter(!((agents=="adult_males"|agents=="adult_females")&
             mated==0))
egg_agents = data.frame(NULL) 
tick_agents = tick_agents %>%
  mutate(kill = runif(n = nrow(.),min=0,max = 1)) %>%
  filter(agents=="larvae" & fed ==1 & kill<=0.415) %>% # Halsey 18
  dplyr::select(-kill) %>%
  bind_rows(., tick_agents %>%
               filter(!c(agents=="larvae"& fed==1))) %>%
  mutate(agents = case_when(agents=="larvae" & fed == 1 ~ as.character('nymphs'),
                            TRUE ~ as.character(agents)))}  # Molt Larvae to Nymphs hatch eggs, kill unfed adults
  
if(season=="fall"){tick_agents = tick_agents %>%
  filter(!c(agents=="larvae" & fed==0)) %>%
  filter(!c(agents=="nymphs" & fed==0)) %>%
  mutate(agents = case_when(agents=="nymphs" & fed == 1 & sex =="male" ~ as.character("adult_males"),
                            agents=="nymphs" & fed == 1 & sex=="female" ~ as.character("adult_females"),
                  TRUE ~ as.character(agents)),
         fed = case_when(agents=="adult_males"|agents=="adult_females"~as.numeric(0),
                         TRUE ~ as.numeric(fed)),
         )} # Kill unfed larvae molt nymphs to adults kill unfed nymphs
if(i%%6265==0){
  tick_agents = tick_agents %>%
    filter((agents=="adult_females"|agents=='adult_males')&
             fed==0 & mated==0) %>% 
    mutate(kill = round(runif(n = nrow(.),min=0,max = 1))) %>%
    filter(kill==0) %>%
    dplyr::select(-kill) %>%
    bind_rows(., tick_agents %>%
                filter(!((agents=="adult_females"|agents=='adult_males')&
                fed==0 & mated==0)))
} # Kill overwintering ticks

# Track population --------------------------------------------------------
if(i==1){track_counts=tick_agents %>%
  group_by(agents) %>%
  summarize(tot=n()) %>%
  mutate(time_step=i) %>%
  bind_rows(.,egg_agents %>%
              summarize(tot=n()) %>%
              mutate(time_step=i,
                     agents="eggs"))
  
  
  # track_counts=tick_agents %>%
  # group_by(agents,fed) %>%
  # summarize(tot=n()) %>%
  # mutate(time_step=i,
  #        agents = paste(ifelse(fed==1,"fed_",
  #                              "unfed_"),agents,sep="")) %>%
  # bind_rows(.,egg_agents %>%
  #             summarize(tot=n()) %>%
  #             mutate(time_step=i,
  #                    agents="eggs"))
  }
if(i>1){track_counts=rbind(track_counts,
                           tick_agents %>%
                             group_by(agents) %>%
                             summarize(tot=n()) %>%
                             mutate(time_step=i) %>%
                             bind_rows(.,egg_agents %>%
                                         summarize(tot=n()) %>%
                                         mutate(time_step=i,
                                                agents="eggs")))
  
  
  # track_counts=rbind(track_counts,
  #                          tick_agents %>%
  #                            group_by(agents,fed) %>%
  #                            summarize(tot=n()) %>%
  #                            mutate(time_step=i,
  #                                   agents = paste(ifelse(fed==1,"fed_",
  #                                                         "unfed_"),agents,sep="")) %>%
  #                            bind_rows(.,egg_agents %>%
  #                                        summarize(tot=n()) %>%
  #                                        mutate(time_step=i,
  #                                               agents="eggs")))
}
  
  
setTxtProgressBar(pb,i)
}

# Visualization -----------------------------------------------------------
seasonsdf = data.frame(time_step=c(0,2160,3127,5497,8424,2160+8760,
                                   3127+8760,5497+8760,8424+8760,2*8760),
                       tot=rep(max(track_counts$tot,na.rm=T)+10,10))

track_counts2=track_counts %>%
  left_join(track_counts %>%
  expand(agents,time_step),.) %>%
  mutate

ggplot(data=track_counts2,aes(x=time_step,y=tot,group=agents))+
  geom_line(aes(col=agents))+
  # geom_segment(data=seasonsdf %>% filter(time_step==0),
  #              aes(x=time_step,y=tot,xend=0,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='blue',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==2160),
  #              aes(x=time_step,y=tot,xend=2160,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='green',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==3127),
  #              aes(x=time_step,y=tot,xend=3127,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='red',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==5497),
  #              aes(x=time_step,y=tot,xend=5497,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='orange',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==8424),
  #              aes(x=time_step,y=tot,xend=8424,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='blue',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==2160+8760),
  #              aes(x=time_step,y=tot,xend=2160+8760,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='green',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==3127+8760),
  #              aes(x=time_step,y=tot,xend=3127+8760,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='red',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==5497+8760),
  #              aes(x=time_step,y=tot,xend=5497+8760,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='orange',
  #              lwd=2,
  #              inherit.aes = F)+
  # geom_segment(data=seasonsdf %>% filter(time_step==8424+8760),
  #              aes(x=time_step,y=tot,xend=8424+8760,yend=max(track_counts$tot,na.rm=T)+20),
  #              col='blue',
  #              lwd=2,
  #              inherit.aes = F)+
  theme_classic()



