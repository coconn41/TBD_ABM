library(dplyr)
library(raster)
library(ggplot2)
# create grid parameters (10 x 10)
rm(list=ls())

go_timesteps = 100

gridrows = 15
gridcols = 15
edgerow = c(1,gridrows)
edgecol = c(1,gridcols)

# create agents
######
eggs = 5
larvae = 100
nymphs = 25
adult_males = 50
adult_females = 50
mice = 100
deer = 1
ticks = c("larvae","nymphs","adult_males","adult_females")
tot_agents = eggs + larvae + nymphs + adult_males + adult_females + mice + deer
agents = data.frame(agents = c(rep("mice",mice),
                               rep("deer",deer),
                               rep("eggs",eggs),
                               rep("larvae",larvae),
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
agents$fed=0
agents$mated = 0
agents$sex = sample(c('male','female'),nrow(agents),replace=T)

host_agents = agents %>%
  filter(agents=="deer"|agents=="mice")
tick_agents = agents %>%
  filter(agents%in%ticks)
egg_agents = agents %>%
  filter(agents=="eggs")
hatched_eggs=NULL
# host_viewgrid = matrix(nrow = gridrows,
#                   ncol = gridcols,
#                   0)
# tick_viewgrid = matrix(nrow = gridrows,
#                        ncol = gridcols,
#                        0)
# for(i in 1:nrow(host_agents)){
#   host_viewgrid[host_agents[i,2],host_agents[i,3]]=host_viewgrid[host_agents[i,2],host_agents[i,3]]+1}
# for(i in 1:nrow(tick_agents)){
#   tick_viewgrid[tick_agents[i,2],tick_agents[i,3]]=tick_viewgrid[tick_agents[i,2],tick_agents[i,3]]+1}
pb = txtProgressBar(min = 0, max = go_timesteps,initial=0,style = 3)
for(i in 1:go_timesteps){
  
  
  
  egg_agents$time_on_host=egg_agents$time_on_host+1
  tick_agents$time_on_host=ifelse(tick_agents$links!=0,tick_agents$time_on_host+1,tick_agents$time_on_host)
  egg_agents$mated=ifelse(egg_agents$time_on_host>20,1,egg_agents$mated)
  hatched_eggs = egg_agents %>% 
    filter(mated==1) %>%
    mutate(agents='larvae',
           mated=0,
           time_on_host=0) %>%
    slice(rep(1:n(),each=10))
  if(nrow(hatched_eggs)>0){hatched_eggs=hatched_eggs%>%
    mutate(id = (max(c(tick_agents$id,egg_agents$id,host_agents$id))+1):
             (max(c(tick_agents$id,egg_agents$id,host_agents$id))+nrow(.)),
           sex = sample(c('male','female'),nrow(.),replace=T))}
  egg_agents = egg_agents %>% 
    filter(mated!=1)
  tick_agents = bind_rows(tick_agents,hatched_eggs)
  
  tick_agents$attachment_prob = runif(nrow(tick_agents),min=0,max=1)
  tick_agents$location = ifelse(tick_agents$attachment_prob>0.8&tick_agents$links>0,
                                host_agents[which(host_agents$links%in%tick_agents$links),4],
                                tick_agents$location)
  tick_agents$links = ifelse(tick_agents$location%in%host_agents$location,
                             host_agents[which(host_agents$)],tick_agents$links)
  
  # for(j in 1:nrow(tick_agents)){
  #   if(tick_agents[j,4] %in% host_agents[,4]){
  #     attachmentprob = runif(n=1,min=0,max=1) # needs parameters
  #     if(attachmentprob > 0.8){tick_agents[j,6] = host_agents %>%
  #                                           filter(location==tick_agents[j,4]) %>%
  #                                           dplyr::select(id) %>%
  #                                           sample_n(1)
  #     }
  #  }
  #} # Tick attachment
  
  
  
  attached_males = tick_agents %>% filter(agents=="adult_males"&links==0)
  
  
  
  for(k in 1:nrow(host_agents)){
    #possible moves
    possibilities = c(0:8)
    if(host_agents[k,2]==gridrows & !host_agents[k,3]%in%edgecol){possibilities = c(0:5)} # can't move down
    if(host_agents[k,2]==1 & !host_agents[k,3]%in%edgecol){possibilities = c(0,4:8)} # can't move up
    if(host_agents[k,3]==gridcols & !host_agents[k,2]%in%edgerow){possibilities = c(0,1,2,4,6,7)} # can't move right
    if(host_agents[k,3]==1 & !host_agents[k,2]%in%edgerow){possibilities = c(0,2,3,5,7,8)} # can't move left
    if(host_agents[k,2]==gridrows & host_agents[k,3]==gridcols){possibilities = c(0,1,2,4)}# can't move down or right
    if(host_agents[k,2]==gridrows & host_agents[k,3]==1){possibilities = c(0,2,3,5)}# can't move down or left
    if(host_agents[k,2]==1 & host_agents[k,3]==gridcols){possibilities = c(0,4,6,7)} # can't move up or right
    if(host_agents[k,2]==1 & host_agents[k,3]==1){possibilities = c(0,5,7,8)} # can't move up or left

    movement_ref = round(runif(n=1,
                         min = 0.5,
                         max = length(possibilities)+0.5))

    movement = as.numeric(possibilities[movement_ref])
    
  if(movement == 1){host_agents[k,2]=host_agents[k,2]-1  # move row up one
                   host_agents[k,3]=host_agents[k,3]-1} # move col left one
  if(movement == 2){host_agents[k,2]=host_agents[k,2]-1} # move row up one
  if(movement == 3){host_agents[k,2]=host_agents[k,2]-1  # move row up one
                   host_agents[k,3]=host_agents[k,3]+1} # move col right one
  if(movement == 4){host_agents[k,3]=host_agents[k,3]-1} # move col left one
  if(movement == 5){host_agents[k,3]=host_agents[k,3]+1} # move col right one
  if(movement == 6){host_agents[k,2]=host_agents[k,2]+1  # move row down one
                   host_agents[k,3]=host_agents[k,3]-1} # move col left one
  if(movement == 7){host_agents[k,2]=host_agents[k,2]+1} # move row down one
  if(movement == 8){host_agents[k,2]=host_agents[k,2]+1  # move row down one
                   host_agents[k,3]=host_agents[k,3]+1} # move col right one
  if(is.na(host_agents[k,6])==T){host_agents[k,4] = paste(host_agents[k,2],",",host_agents[k,3],sep = "")}
    #host_viewgrid[host_agents[k,2],host_agents[k,3]]=host_viewgrid[host_agents[k,2],host_agents[k,3]]+1
    } # Host movement
  
  
  
  locs2drop=host_agents[which(host_agents$id%in%
                                tick_agents[which(tick_agents$agents=="nymphs"&
                                                    tick_agents$links>0&
                                                    tick_agents$time_on_host==5),6]),4]
  locs2drop=c(locs2drop,
              host_agents[which(host_agents$id%in%
                                tick_agents[which(tick_agents$agents=="larvae"&
                                                    tick_agents$links>0&
                                                    tick_agents$time_on_host==5),6]),4])
  locs2drop=c(locs2drop,
              host_agents[which(host_agents$id%in%
                                  tick_agents[which(tick_agents$agents=="adult_females"&
                                                      tick_agents$links>0&
                                                      tick_agents$time_on_host==7),6]),4])
  
  moltingticks = tick_agents %>% filter((time_on_host==7&agents=="adult_females")|
                                          (time_on_host==5&agents=="nymphs")|
                                          (time_on_host==5&agents=="larvae")|
                                          (time_on_host==10&agents=="adult_males"),
                                        links>0)
  dying_adults = tick_agents %>% filter((agents=="adult_males"&time_on_host==11)|
                                        (agents=="adult_females"&time_on_host==8))
  l2did=0
  
  
  
  for(m in unique(moltingticks$id)){
    l2did=l2did+1
    tick_agents$location=ifelse(tick_agents$id==m,locs2drop[l2did],tick_agents$location)}
  
tick_agents = tick_agents %>%
     mutate(links = case_when((agents=="larvae"|agents=="nymphs")&time_on_host==5 ~ 0,
                              TRUE ~ links),
            fed = case_when((agents=="larvae"|agents=="nymphs")&time_on_host==5 ~ 1,
                           TRUE ~ fed)) %>%
    mutate(time_on_host = case_when(fed==1~0,
                                    TRUE~time_on_host),
           agents = case_when(agents=="larvae"&fed==1 ~ "nymphs",
                              agents=="nymphs"&fed==1&sex=="male" ~ "adult_males",
                              agents=="nymphs"&fed==1&sex=="female" ~ "adult_females",
                              TRUE ~ agents)) %>%
    mutate(fed = case_when((agents=="nymphs"|agents=="adult_males"|
                              agents=="adult_females")&fed==1 ~ 0,
                           TRUE ~ fed))
           # ^ might be able to remove adult portion if they are deleted
  
  # Links aren't resetting
  male_links = tick_agents %>% filter(agents=="adult_males"&links>0)
  female_links = tick_agents %>% filter(agents=="adult_females"&links>0)
  linked = merge(male_links,female_links,by='links') 
  linkedids=c(linked[,6],linked[,14])
  linked = linked %>% dplyr::select(links)
  tick_agents = tick_agents %>%
    filter(!id%in%dying_adults$id)
  
  
  for(l in unique(linked$links)){
    dropegg = which(host_agents[,5]==l)
    new_egg = data.frame(agents="eggs",
                         row=host_agents[dropegg,2],
                         col=host_agents[dropegg,3],
                         location=host_agents[dropegg,4],
                         id = max(c(tick_agents$id,egg_agents$id,host_agents$id))+1,
                         links = 0,
                         time_on_host=0,
                         fed=0,
                         mated=0,
                         sex="male")
    egg_agents = rbind(egg_agents,new_egg)}
  
  if(i==1){track_counts=tick_agents %>%
    group_by(agents) %>%
    summarize(tot=n()) %>%
    mutate(time_step=i) %>%
    bind_rows(.,egg_agents %>%
                summarize(tot=n()) %>%
                mutate(time_step=i,
                       agents="eggs"))}
  if(i>1){track_counts=rbind(track_counts,
                             tick_agents %>%
                               group_by(agents) %>%
                               summarize(tot=n()) %>%
                               mutate(time_step=i) %>%
                               bind_rows(.,egg_agents %>%
                                           summarize(tot=n()) %>%
                                           mutate(time_step=i,
                                                  agents="eggs")))}
  setTxtProgressBar(pb,i)
  }

ggplot(data=track_counts,aes(x=time_step,y=tot,group=agents))+
  geom_line(aes(col=agents))+
  theme_classic()

