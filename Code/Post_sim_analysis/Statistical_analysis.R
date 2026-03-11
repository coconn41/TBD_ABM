#####
# Load libraries
#####
library(readr)
library(tidyverse)
library(sf)
library(igraph)
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
  filter(Lifestage!="Larvae")
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

jump_probability_df <- read_csv("Cached_data/jump_probability_df.csv") %>%
  filter(network_ID %in% unique(net_conn$network_ID),
         origin_ID %in% unique(net_conn$layer)) %>%
  select(network_ID,destination_ID,probability) %>%
  rename(layer = "destination_ID")

# network1 = sf::read_sf(paste0(getwd(),'/Cached_data/Reduced_network.shp')) %>%
#   rename(lcp_distance = "lcp_dst",
#          origin_ID = "orgn_ID",
#          destination_ID = "dstn_ID",
#          distance = "distanc",
#          inverse_sinuousity = "invrs_s",
#          network_ID = "ntwr_ID")
# iind = 0
# for(i in unique(net_conn$network_ID)){ 
# iind = iind + 1
#   ncdf = net_conn %>%
#     filter(network_ID==i)
#   network_df = network1 %>%
#     filter(network_ID==i) %>%
#     st_drop_geometry() %>%
#     dplyr::select(-c(distance,inverse_sinuousity,network_ID)) %>%
#     group_by(origin_ID,destination_ID) %>%
#     summarize(lcp_distance = sum(lcp_distance),.groups="drop") %>%
#     left_join(.,network1 %>%
#                 filter(network_ID==i) %>%
#                 st_drop_geometry() %>%
#                 dplyr::select(-c(inverse_sinuousity,network_ID)) %>%
#                 group_by(origin_ID,destination_ID) %>%
#                 summarize(distance = sum(distance),.groups="drop"))
#   network_graph_df = igraph::graph_from_data_frame(network_df,directed=TRUE,vertices = NULL)
#   E(network_graph_df)$distance = (network_df$lcp_distance)
#   graph_vs = as.numeric(V(network_graph_df)$name)
#   
#   start_point = which(graph_vs==ncdf$layer)
#   aind = 0
#   for(a in 1:length(V(network_graph_df))){
#     aind = aind+1
#     list_i = shortest_paths(network_graph_df,
#                             from = V(network_graph_df)[[start_point]],
#                             to = V(network_graph_df)[[a]])
#     print(paste0("Network ",i,", layers: ",c(list_i$vpath[[1]]$name)))
#     order_away = data.frame(network_ID = i,
#                             layer = as.numeric(V(network_graph_df)[[a]]$name),
#                             order = length(list_i$vpath[[1]]$name)-1)
#     if(aind==1){order_away2 = order_away}
#     if(aind>1){order_away2 = rbind(order_away2,order_away)}
#   }
# if(iind==1){order_away3 = order_away2}
# if(iind>1){order_away3 = rbind(order_away3,order_away2)}
# }
# 
# order_away = order_away3
# rm(order_away3,order_away2)

# net_conn = net_conn %>% 
#   select(-layer)

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
  select(-layer.y) %>%
  rename(layer = 'layer.x') %>%
  left_join(.,jump_probability_df,by=c('network_ID','layer')) %>%
  mutate(tick_density = total_ticks/hectare)

unq_layerz = All_ticks %>%
  group_by(network_ID,layer) %>%
  summarize(tot = n()) %>%
  select(-tot) %>%
  left_join(.,fin_all_patch %>%
              select(layer,hectare) %>%
              st_drop_geometry()) %>%
  ungroup() %>%
  group_by(network_ID) %>%
  summarize(hectares = sum(hectare))

#####
# Pull in deer and mouse data:
#####

All_data = All_ticks %>%
  left_join(.,Deer_data %>%
              select(network_ID,layer,tot_deer,timestep),
              by=c('network_ID','layer','timestep')) %>%
  left_join(.,Mouse_data %>%
              select(network_ID,layer,tot_mice,timestep),by=c('network_ID','layer','timestep')) %>%
  mutate(deer_density = tot_deer/hectare,
         mouse_density = tot_mice/hectare,
         Ap_ha_ERI = (ha_perc/100)*(tick_density),
         Ap_v1_ERI = (v1_perc/100)*(tick_density))

#####
# Large model creation:
#####
dens_mod1 = mgcv::gam(formula = tick_density ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer,bs='re'),
                      data = All_data %>%
                        filter(Lifestage=="Larvae"))
summary(dens_mod1)
dens_mod2 = mgcv::gam(formula = tick_density ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer,bs='re'),
                      data = All_data %>%
                        filter(Lifestage=="Nymphs"))
summary(dens_mod2)
dens_mod3 = mgcv::gam(formula = tick_density ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer,bs='re'),
                      data = All_data %>%
                        filter(Lifestage=="Adults"))
summary(dens_mod3)

model_1 = mgcv::gam(formula = ha_perc ~ v1_perc + 
                      tick_density + 
                      deer_density + 
                      mouse_density +
                      metric + 
                      network_metric + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Larvae"))
summary(model_1)

model_2 = mgcv::gam(formula = ha_perc ~  v1_perc + 
                      tick_density +
                      deer_density + 
                      mouse_density +
                      metric + 
                      network_metric + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Nymphs"))
summary(model_2)

model_3 = mgcv::gam(formula = ha_perc ~ v1_perc +
                      tick_density + 
                      deer_density + 
                      mouse_density +
                      metric + 
                      network_metric + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Adults"))
summary(model_3)

model_4 = mgcv::gam(formula = v1_perc ~ ha_perc +
                      tick_density + 
                      deer_density + 
                      mouse_density +
                      metric + 
                      network_metric + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Larvae"))
summary(model_4)

model_5 = mgcv::gam(formula = v1_perc ~ ha_perc + 
                      tick_density + 
                      deer_density + 
                      mouse_density +
                      metric + 
                      network_metric + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Nymphs"))
summary(model_5)

model_6 = mgcv::gam(formula = v1_perc ~ ha_perc + 
                      tick_density + 
                      deer_density + 
                      mouse_density +
                      metric + 
                      network_metric + 
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Adults"))
summary(model_6)

ERI_mod_1 = mgcv::gam(formula = log(Ap_ha_ERI+0.0001) ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer, bs = "re"),
                      data = All_data %>%
                        filter(Lifestage=="Larvae"))
summary(ERI_mod_1)
ERI_mod_2 = mgcv::gam(formula = log(Ap_ha_ERI+0.0001) ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer, bs = "re"),
                      data = All_data %>%
                        filter(Lifestage=="Nymphs"))
summary(ERI_mod_2)
ERI_mod_3 = mgcv::gam(formula = log(Ap_ha_ERI+0.0001) ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer, bs = "re"),
                      data = All_data %>%
                        filter(Lifestage=="Adults"))
summary(ERI_mod_3)
ERI_mod_4 = mgcv::gam(formula = log(Ap_v1_ERI+0.0001) ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer, bs = "re"),
                      data = All_data %>%
                        filter(Lifestage=="Larvae"))
summary(ERI_mod_4)
ERI_mod_5 = mgcv::gam(formula = log(Ap_v1_ERI+0.0001) ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer, bs = "re"),
                      data = All_data %>%
                        filter(Lifestage=="Nymphs"))
summary(ERI_mod_5)
ERI_mod_6 = mgcv::gam(formula = log(Ap_v1_ERI+0.0001) ~ deer_density + 
                        mouse_density + 
                        metric + 
                        network_metric + 
                        s(timestep) + 
                        s(layer, bs = "re"),
                      data = All_data %>%
                        filter(Lifestage=="Adults"))
summary(ERI_mod_6)

# all_results = data.frame(Pathogen = c(rep("Ap-ha",3),rep("Ap-v1",3)),
#                          Lifestage = rep(c("Larvae","Nymph","Adult"),2),
#                          total_ticks = c(model_1$coefficients[2],model_2$coefficients[2],
#                                          model_3$coefficients[2],model_4$coefficients[2],
#                                          model_5$coefficients[2],model_6$coefficients[2]),
#                          hectare = c(model_1$coefficients[3],model_2$coefficients[3],
#                                      model_3$coefficients[3],model_4$coefficients[3],
#                                      model_5$coefficients[3],model_6$coefficients[3]),
#                          tick_density = c(model_1$coefficients[4],model_2$coefficients[4],
#                                           model_3$coefficients[4],model_4$coefficients[4],
#                                           model_5$coefficients[4],model_6$coefficients[4]),
#                          metric = c(model_1$coefficients[5],model_2$coefficients[5],
#                                     model_3$coefficients[5],model_4$coefficients[5],
#                                     model_5$coefficients[5],model_6$coefficients[5]),
#                          network_metric = c(model_1$coefficients[6],model_2$coefficients[6],
#                                             model_3$coefficients[6],model_4$coefficients[6],
#                                             model_5$coefficients[6],model_6$coefficients[6]),
#                          probability = c(model_1$coefficients[7],model_2$coefficients[7],
#                                          model_3$coefficients[7],model_4$coefficients[7],
#                                          model_5$coefficients[7],model_6$coefficients[7]),
#                          deer_density = c(model_1$coefficients[8],model_2$coefficients[8],
#                                           model_3$coefficients[8],model_4$coefficients[8],
#                                           model_5$coefficients[8],model_6$coefficients[8]),
#                          mouse_density = c(model_1$coefficients[9],model_2$coefficients[9],
#                                            model_3$coefficients[9],model_4$coefficients[9],
#                                            model_5$coefficients[9],model_6$coefficients[9]))


#investigate plotting 3d relationship?

model_7 = mgcv::gam(formula = ha_perc ~ s(tick_density,mouse_density,deer_density,bs='tp'),# +
                     # s(timestep) + 
                     # s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Adults"))
summary(model_7)

model_8 = mgcv::gam(formula = ha_perc ~ s(mouse_density,deer_density,bs='tp') +
                     s(timestep) + 
                     s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Larvae"))
summary(model_8)

vis.gam(model_8,theta=40,view = c('deer_density','mouse_density'),phi=20)

model_9 = mgcv::gam(formula = ha_perc ~ s(mouse_density,deer_density,bs='tp') +
                      s(timestep) + 
                    s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Nymphs"))
summary(model_9)

model_10 = mgcv::gam(formula = ha_perc ~ s(mouse_density,deer_density,bs='tp') +
                      s(timestep)+ 
                    s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Adults"))
summary(model_10)

model_11 = mgcv::gam(formula = v1_perc ~ s(mouse_density,deer_density,bs='tp') +
                      s(timestep) + 
                      s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Larvae"))
summary(model_11)

vis.gam(model_11,theta=40,view = c('deer_density','mouse_density'),phi=20)

model_12 = mgcv::gam(formula = v1_perc ~ s(mouse_density,deer_density,bs='tp') +
                      s(timestep) + 
                    s(layer,bs='re'),
                    data = All_data %>%
                      filter(Lifestage=="Nymphs"))
summary(model_12)

model_13 = mgcv::gam(formula = v1_perc ~ s(mouse_density,deer_density,bs='tp') +
                       s(timestep)+ 
                     s(layer,bs='re'),
                     data = All_data %>%
                       filter(Lifestage=="Adults"))
summary(model_13)
jpeg(filename = paste0(getwd(),'/Figures/Figures/GAM_fig.jpeg'),
     res = 300,
     width = 2000,
     height = 1400)
par(mfrow=c(2,3),mar = c(.25,.25,2,.25))
vis.gam(model_8,
        theta=40,
        view = c('deer_density','mouse_density'),
        phi=20,
        xlab = "Deer density",
        ylab = "Mouse density",
        color = 'gray')
title(main = paste0("A.) Ap-ha in larval ticks"))
vis.gam(model_9,
        theta=40,
        view = c('deer_density','mouse_density'),
        phi=20,
        xlab = "Deer density",
        ylab = "Mouse density",
        color = 'gray')
title(main = paste0("B.) Ap-ha in nymphal ticks"))
vis.gam(model_10,
        theta=40,
        view = c('deer_density','mouse_density'),
        phi=20,
        xlab = "Deer density",
        ylab = "Mouse density",
        color = 'gray')
title(main = paste0("C.) Ap-ha in adult ticks"))
vis.gam(model_11,
        theta=40,
        view = c('deer_density','mouse_density'),
        phi=20,
        color = 'gray')
title(main = paste0("D.) Ap-v1 in larval ticks"))
vis.gam(model_12,
        theta=40,
        view = c('deer_density','mouse_density'),
        phi=20,
        xlab = "Deer density",
        ylab = "Mouse density",
        color = 'gray')
title(main = paste0("E.) Ap-v1 in nymphal ticks"))
vis.gam(model_13,
        theta=40,
        view = c('deer_density','mouse_density'),
        phi=20,
        xlab = "Deer density",
        ylab = "Mouse density",
        color = 'gray')
title(main = paste0("F.) Ap-v1 in adult ticks"))
dev.off()
