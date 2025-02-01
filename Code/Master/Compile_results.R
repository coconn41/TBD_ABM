options(dplyr.summarise.inform = FALSE)
track_data = function(i,deer_agents,mouse_agents,tick_agents){
  #if(i%%24==0){
    deer_data <<- deer_agents %>%
      group_by(network_ID,layer) %>%
      summarise(V1_perc = sum(V1_infected)/n()) %>%
      mutate(day_of_year = day,
             season = season,
             timestep = i,
             year = year,
             day_of_year = day,
             network = net_select,
             Agent = "Deer")
    mouse_data<<- mouse_agents %>%
      group_by(network_ID,layer) %>%
      summarise(Ha_perc = sum(Ha_infected)/n()) %>%
      mutate(day_of_year = day,
             timestep = i,
             season = season,
             year = year,
             day_of_year = day,
             network = net_select,
             Agent = "Mice")
    tick_data <<- tick_agents %>%
      group_by(Lifestage,network_ID,layer) %>%
      summarise(Ha_perc = (length(which(Infection_status == "ha"))/n())*100,
                v1_perc = (length(which(Infection_status == "v1"))/n())*100,
                total_ticks = sum(num_ticks),
                Agent = "Tick",
                year = year,
                timestep = i,
                day_of_year = day,
                network = net_select,
                season = season)
 # }
  if(i==1){#24){
    deer_data2 <<- deer_data
    mouse_data2 <<- mouse_data
    tick_data2 <<- tick_data
  }
  if(i>1){#24){
    deer_data2 <<- rbind(deer_data,deer_data2)
    mouse_data2 <<- rbind(mouse_data,mouse_data2)
    tick_data2 <<- rbind(tick_data,tick_data2)
  }
}
complete_viz_dataset_fn = function(tick_data2,aspatial_network){#deer_data2,mouse_data2,
tick_viz_data <<- expand_grid(layer = unique(c(aspatial_network$origin_ID,
                                                aspatial_network$destination_ID)),
                               Lifestage = c("Eggs","Larvae","Nymph","Adult"),
                               year = unique(tick_data2$year),
                               timestep = unique(tick_data2$timestep)) %>%
  mutate(network_ID = rep(net_select,nrow(.)),
         Ha_perc = rep(NA_real_,nrow(.)),
         v1_perc = rep(NA_real_,nrow(.)),
         total_ticks = rep(0,nrow(.)),
         Agent = rep("Tick",nrow(.)),
         day_of_year = rep(day,nrow(.)),
         network = rep(net_select,nrow(.)),
         season = rep(season,nrow(.))) %>%
  rbind(.,tick_data2 %>%
          ungroup()) %>%
  group_by(Lifestage,year,timestep,layer) %>%
  slice_max(order_by = total_ticks,n=1) %>%
  ungroup()

# expanded_deer_df = expand_grid(layer = unique(c(aspatial_network$origin_ID,
#                                                 aspatial_network$destination_ID)),
#                                year = unique(deer_data2$year),
#                                timestep = unique(deer_data2$timestep)) %>%
#   mutate(network_ID = rep(net_select,nrow(.)),
#          Ha_perc = rep(NA_real_,nrow(.)),
#          v1_perc = rep(NA_real_,nrow(.)),
#          total_ticks = rep(0,nrow(.)),
#          Agent = rep("Deer",nrow(.)),
#          day_of_year = rep(day,nrow(.)),
#          network = rep(net_select,nrow(.)),
#          season = rep(season,nrow(.))) %>%
#   rbind(.,deer_data2 %>%
#           ungroup()) %>%
#   group_by(year,timestep,layer) %>%
#   slice_max(order_by = total_ticks,n=1) %>%
#   ungroup()
# 
# expanded_mouse_df = expand_grid(layer = unique(c(aspatial_network$origin_ID,
#                                                  aspatial_network$destination_ID)),
#                                 year = unique(mouse_data2$year),
#                                 timestep = unique(mouse_data2$timestep)) %>%
#   mutate(network_ID = rep(net_select,nrow(.)),
#          Ha_perc = rep(NA_real_,nrow(.)),
#          v1_perc = rep(NA_real_,nrow(.)),
#          total_ticks = rep(0,nrow(.)),
#          Agent = rep("Mice",nrow(.)),
#          day_of_year = rep(day,nrow(.)),
#          network = rep(net_select,nrow(.)),
#          season = rep(season,nrow(.)))
}
