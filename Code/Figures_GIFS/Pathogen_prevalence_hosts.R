# Debugging load:
load(paste0(getwd(),'/Debugging/Network_6/net_6_timestep_55000.RData'))
# Burn in load:
load(paste0(getwd(),'/Simulations/Network_6/timestep_61320_attach_25_path_trans_100.RData'))
library(tidyverse)
library(tmap)
library(gifski)
library(tmaptools)
library(sf)
#####
# Rename at factor:
#####
deer_data2 = deer_data2 %>%
  filter(season!="")
mouse_data2 = mouse_data2 %>%
  filter(season!="")
#####
# Calculate Ha percentage in mice:
#####
ggplot(data = mouse_data2 %>% 
         group_by(timestep) %>% 
         summarize(ha_perc = (sum(tot_ha_infected)/sum(tot_mice))*100),
       aes(x=timestep,y=ha_perc))+
  geom_line()+
  geom_vline(xintercept = (8760*5))+
  theme_bw()+
  xlab("Timesteps (hours)")+
  ylab("Ap-ha (%)")
#####
# Calculate V1 percentage in deer:
#####
ggplot(data = deer_data2 %>%
         group_by(timestep) %>% 
         summarize(v1_perc = (sum(tot_v1_infected)/sum(tot_deer))*100),
       aes(x=timestep,y=v1_perc))+
  geom_line()+
  geom_vline(xintercept = (8760*5))+
  theme_bw()+
  xlab("Timesteps (hours)")+
  ylab("Ap-v1 (%)")
