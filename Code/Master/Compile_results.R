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

