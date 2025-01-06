tick_timer = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(tick_age_wks = tick_age_wks+(1/168),
           time_since_mating = ifelse(mated==1,time_since_mating+1,0)) # hours per week
}