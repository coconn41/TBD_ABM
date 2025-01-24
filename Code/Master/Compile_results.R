options(dplyr.summarise.inform = FALSE)
track_data = function(i,deer_agents,mouse_agents,tick_agents){
  if(i%%24==0){
    deer_data <<- deer_agents %>%
      group_by(network_ID) %>%
      summarise(V1_perc = sum(V1_infected)/n()) %>%
      mutate(day_of_year = day,
             season = season,
             year = year,
             Agent = "Deer")
    mouse_data<<- mouse_agents %>%
      group_by(network_ID) %>%
      summarise(Ha_perc = sum(Ha_infected)/n()) %>%
      mutate(day_of_year = day,
             season = season,
             year = year,
             Agent = "Mice")
    tick_data <<- tick_agents %>%
      group_by(Lifestage,network_ID) %>%
      summarise(Ha_perc = (length(which(Infection_status == "ha"))/n())*100,
                v1_perc = (length(which(Infection_status == "v1"))/n())*100,
                total_ticks = sum(num_ticks),
                Agent = "Tick",
                year = year,
                day_of_year = day,
                season = season)
  }
  if(i==24){
    deer_data2 <<- deer_data
    mouse_data2 <<- mouse_data
    tick_data2 <<- tick_data
  }
  if(i>24){
    deer_data2 <<- rbind(deer_data,deer_data2)
    mouse_data2 <<- rbind(mouse_data,mouse_data2)
    tick_data2 <<- rbind(tick_data,tick_data2)
  }
}

