#####
# Load libraries
#####
library(readr)
library(tidyverse)
library(sf)
library(lme4)
library(mgcv)
#####
# Load data
#####
Tick_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_ticks.csv") %>%
  mutate(network_ID = 6) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_ticks.csv") %>%
              mutate(network_ID = 1)) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_ticks.csv") %>%
              mutate(network_ID = 7))

Deer_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_deer.csv") %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_deer.csv")) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_deer.csv"))

Mouse_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_mice.csv") %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_mice.csv")) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_mice.csv"))

starting_tick_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/starting_tick_data.csv")

#####
# Summary statistics:
#####

# Mean IP and total ticks at start:
starting_tick_data = starting_tick_data %>%
  mutate(Lifestage = case_when(Lifestage=="Nymph" ~ "Nymphs",
                               Lifestage=="Adult" ~ "Adults",
                               TRUE ~ Lifestage),
         Lifestage = factor(Lifestage,
                            levels = c("Eggs","Larvae","Nymphs","Adults")))

# Mean IP and total ticks at end:
ending_tick_data = Tick_data %>%
  filter(Lifestage!="Eggs") %>%
  group_by(network_ID,Lifestage,cohort) %>%
  slice_min(simulation_day) %>%
  filter((Lifestage=="Adults"  & cohort == 5)|
           (Lifestage=="Nymphs" & cohort == 4)) %>%
  mutate(ha_perc = (tot_ha/total_ticks)*100,
         v1_perc = (tot_v1/total_ticks)*100) %>%
  select(network_ID,Lifestage,total_ticks,tot_ha,tot_v1,ha_perc,v1_perc,cohort) %>%
  rename(end_ha_pos = "tot_ha",
         end_v1_pos = "tot_v1",
         end_tot = "total_ticks",
         end_ha_perc = "ha_perc",
         end_v1_perc = "v1_perc");ending_tick_data

# Combined:
start_p_end = left_join(ending_tick_data,starting_tick_data,by=c('network_ID','Lifestage')) %>%
  mutate(ha_change = end_ha_perc - ha_perc,
         v1_change = end_v1_perc - v1_perc,
         tick_change = ((end_tot - tot)/tot)*100) %>%
  filter(Lifestage!="Larvae");View(start_p_end %>%
                                     select(network_ID,Lifestage,ha_change,v1_change,tick_change,cohort))
write.csv(start_p_end,paste0(getwd(),'/Post_sim_analysis/Cleaned_data/Individual_networks/comp_start_end.csv'))

#####
# Large model load data:
#####
# Network connectivity table:
net_conn = read.csv(paste0(getwd(),'/Cached_data/Tick_agents.csv'))[,-1] %>%
  filter(network_ID %in% c(1,6,7)) %>%
  select(Site,layer,metric) %>% 
  distinct() %>%
  mutate(network_ID = case_when(Site == "Hanging Bog" ~ 1,
                                Site == "Veterans Memorial Park" ~ 6,
                                Site == "Pine Barrens Trails" ~ 7,
                                TRUE ~ 0)) %>%
  filter(network_ID != 0) %>%
  rename(network_metric = "metric")


fin_all_patch = read_sf(paste0(getwd(),'/Cached_data/fin_all_patch.shp'))

network1 = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
  rename(lcp_distance = "lcp_dst",
         origin_ID = "orgn_ID",
         destination_ID = "dstn_ID",
         distance = "distanc",
         inverse_sinuousity = "invrs_s",
         network_ID = "ntwr_ID")
iind = 0
for(i in unique(net_conn$network_ID)){ 
iind = iind + 1
  ncdf = net_conn %>%
    filter(network_ID==i)
  network_df = network1 %>%
    filter(network_ID==i) %>%
    st_drop_geometry() %>%
    dplyr::select(-c(distance,inverse_sinuousity,network_ID)) %>%
    group_by(origin_ID,destination_ID) %>%
    summarize(lcp_distance = sum(lcp_distance),.groups="drop") %>%
    left_join(.,network1 %>%
                filter(network_ID==i) %>%
                st_drop_geometry() %>%
                dplyr::select(-c(inverse_sinuousity,network_ID)) %>%
                group_by(origin_ID,destination_ID) %>%
                summarize(distance = sum(distance),.groups="drop"))
  
  graph_vs = as.numeric(V(network_graph_df)$name)
  
  start_point = which(graph_vs==ncdf$layer)
  aind = 0
  for(a in 1:length(V(network_graph_df))){
    aind = aind+1
    list_i = shortest_paths(network_graph_df,
                            from = V(network_graph_df)[[start_point]],
                            to = V(network_graph_df)[[a]])
    order_away = data.frame(network_ID = i,
                            layer = as.numeric(V(network_graph_df)[[a]]$name),
                            order = length(list_i$vpath[[1]]$name)-1)
    if(aind==1){order_away2 = order_away}
    if(aind>1){order_away2 = rbind(order_away2,order_away)}
  }
if(iind==1){order_away3 = order_away2}
if(iind>1){order_away3 = rbind(order_away3,order_away2)}
}

order_away = order_away3
rm(order_away3,order_away2)

net_conn = net_conn %>% 
  select(-layer)

All_ticks <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_ticks_all.csv") %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_ticks_all.csv")) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_ticks_all.csv")) %>%
  filter(Lifestage!="Eggs",
         year != 0,
         !c(Lifestage=="Adults" & year == 1),
         !c(Lifestage=="Nymphs" & year == 1),
         !c(Lifestage=="Adults" & year == 2 & day_of_year <= 200)) %>%
  select(-c(`...1`,Agent)) %>%
  left_join(.,fin_all_patch %>%
              select(layer,metric,hectare) %>%
              st_drop_geometry() %>%
              distinct()) %>%
  left_join(.,net_conn,by="network_ID") %>%
  left_join(.,order_away)

#####
# Large model creation:
#####

model_1 = mgcv::gam(formula = ha_perc ~ Lifestage + 
                      total_ticks + 
                      metric + 
                      network_metric + 
                      as.factor(order) +
                      hectare + 
                      as.factor(order)*network_metric + 
                      total_ticks*Lifestage + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_ticks)
summary(model_1)

model_2 = mgcv::gam(formula = v1_perc ~ Lifestage + 
                      total_ticks + 
                      metric + 
                      network_metric + 
                      as.factor(order) +
                      hectare + 
                      as.factor(order)*network_metric + 
                      total_ticks*Lifestage +
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_ticks)
summary(model_2)
