ta = tick_agents %>% group_by(Lifestage,network_ID,Infection_status) %>% summarise(tot = n())

na = nymph_agents %>% group_by(Lifestage,network_ID,Infection_status) %>% summarise(tot = n())

pathogens_distribution = rbind(ta,na) %>%
  pivot_wider(names_from = Infection_status,values_from = tot) %>%
  rbind(.,data.frame(Lifestage = c("Nymph","Nymph"),
                     network_ID = c(1,3),
                     None = c(NA,NA),
                     v1 = c(NA,NA),
                     ha = c(NA,NA))) %>%
  mutate(total = sum(None,v1,ha,na.rm=T),
         ticks_needed = ifelse(total==0,3422,0),
         v1_perc = v1/total*100,
         ha_perc = ha/total*100,
         v1_needed = ifelse(ticks_needed>0,round(.01 * ticks_needed),
                            ifelse(is.na(v1_perc)==T,round(.01 * total),0)),
         ha_needed = ifelse(ticks_needed>0,round(.01 * ticks_needed),
                            ifelse(is.na(ha_perc)==T,round(.01 * total),0)))


tick_agents = rbind(tick_agents,nymph_agents)
for(i in 1:nrow(pathogens_distribution)){
  if(pathogens_distribution[i,]$ticks_needed>0){
    layer_opts = aspatial_network %>% 
      filter(network_ID == pathogens_distribution[i,]$network_ID)
    layerz = unique(c(unique(layer_opts$origin_ID,layer_opts$destination_ID)))
    newtix_layerz = data.frame(layer = sample(layerz,pathogens_distribution[i,]$ticks_needed,replace=T),
                               Agent_type = rep(pathogens_distribution[i,]$Lifestage,
                                                pathogens_distribution[i,]$ticks_needed),
                               network_ID = rep(pathogens_distribution[i,]$network_ID,
                                                pathogens_distribution[i,]$ticks_needed),
                               Infection_status = rep("None",
                                                      pathogens_distribution[i,]$ticks_needed),
                               links = rep(0,pathogens_distribution[i,]$ticks_needed),
                               time_on_host = rep(0,pathogens_distribution[i,]$ticks_needed),
                               fed = rep(0,pathogens_distribution[i,]$ticks_needed),
                               mated = rep(0,pathogens_distribution[i,]$ticks_needed),
                               sex = sample(c("male","female"),
                                            pathogens_distribution[i,]$ticks_needed,
                                            replace = T),
                               time_since_mating = rep(0,pathogens_distribution[i,]$ticks_needed),
                               tick_age_wks = ifelse(pathogens_distribution[i,]$Lifestage=="Nymph",
                                                     rep(39,pathogens_distribution[i,]$ticks_needed),
                                                     rep(65,pathogens_distribution[i,]$ticks_needed)),
                               die = rep(0,pathogens_distribution[i,]$ticks_needed),
                               linked_type = rep("N",pathogens_distribution[i,]$ticks_needed),
                               num_ticks = rep(1,pathogens_distribution[i,]$ticks_needed),
                               dropped = rep(0,pathogens_distribution[i,]$ticks_needed),
                               time_since_fed = rep(0,pathogens_distribution[i,]$ticks_needed),
                               attempted_pathogen_transfer = rep(0,pathogens_distribution[i,]$ticks_needed),
                               Agent_ID = (max(tick_agents$Agent_ID)+1):(max(tick_agents$Agent_ID)+pathogens_distribution[i,]$ticks_needed))
    
    net_compare = deer_agents %>%
      filter(network_ID == pathogens_distribution[i,]$network_ID) %>%
      dplyr::select(County,Site,layer,metric,gridrows,gridcols) %>%
      group_by(layer,County,Site,metric,gridrows,gridcols) %>%
      summarise(tot = n()) %>%
      dplyr::select(-tot)
    
    newtix = left_join(newtix_layerz,net_compare,join_by(layer),multiple = "first") %>%
      filter(is.na(gridrows)==F) %>%
      mutate(row = round(runif(1,gridrows,n=nrow(.))),
             col = round(runif(1,gridcols,n=nrow(.))),
             Lifestage = rep(pathogens_distribution[i,]$Lifestage,
                             nrow(.)))
    
    tick_agents = tick_agents %>%
      bind_rows(.,newtix)
  }    
if(pathogens_distribution[i,]$v1_needed>0){
  df = tick_agents %>%
    filter(network_ID == pathogens_distribution[i,]$network_ID & 
             Lifestage == pathogens_distribution[i,]$Lifestage & 
             Infection_status == "None") 
  sampled_rows = c(sample(x = 1:nrow(df),size = pathogens_distribution[i,]$v1_needed))
  df[sampled_rows, "Infection_status"] <- "v1"
tick_agents = tick_agents %>%
    filter(!c(Agent_ID %in% df$Agent_ID)) %>%
    bind_rows(.,df)
  
}
if(pathogens_distribution[i,]$ha_needed>0){
df = tick_agents %>%
  filter(network_ID == pathogens_distribution[i,]$network_ID & 
           Lifestage == pathogens_distribution[i,]$Lifestage & 
           Infection_status == "None") 
sampled_rows = c(sample(x = 1:nrow(df),size = pathogens_distribution[i,]$ha_needed))
df[sampled_rows, "Infection_status"] <- "ha"
tick_agents = tick_agents %>%
  filter(!c(Agent_ID %in% df$Agent_ID)) %>%
  bind_rows(.,df)
}
  
  
}


# ta2 = tick_agents %>% group_by(Lifestage,network_ID,Infection_status) %>% summarise(tot = n())
# 
# pathogens_distribution2 = ta2 %>%
#   pivot_wider(names_from = Infection_status,values_from = tot) %>%
#   mutate(total = sum(None,v1,ha,na.rm=T),
#          ticks_needed = ifelse(total==0,3422,0),
#          v1_perc = v1/total*100,
#          ha_perc = ha/total*100,
#          v1_needed = ifelse(ticks_needed>0,round(.01 * ticks_needed),
#                             ifelse(is.na(v1_perc)==T,round(.01 * total),0)),
#          ha_needed = ifelse(ticks_needed>0,round(.01 * ticks_needed),
#                             ifelse(is.na(ha_perc)==T,round(.01 * total),0)))
