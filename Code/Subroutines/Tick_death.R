tick_death = function(tick_agents){
  tick_agents <- tick_agents %>%
    mutate(replete_death = case_when(
      Lifestage == "Larvae" & fed == 1 & dropped == 1 ~ rbinom(n(), size = 1, prob = L_rep_DR),
      Lifestage == "Nymph" & fed == 1 & dropped == 1 ~ rbinom(n(), size = 1, prob = N_rep_DR),
      Lifestage == "Adult" & fed == 1 & dropped == 1 ~ rbinom(n(), size = 1, prob = A_rep_DR),
      TRUE ~ 0)) %>%
    mutate(un_replete_death = case_when(
      Lifestage == "Larvae" & fed == 0 & dropped == 0 & (day/7) >= 40 ~ rbinom(n(), size = 1, prob = L_unfed_DR_o40),
      Lifestage == "Larvae" & fed == 0 & dropped == 0 & (day/7) < 40  ~ rbinom(n(), size = 1, prob = L_unfed_DR_lt40),
      Lifestage == "Nymph" & fed == 0 & dropped == 0 & (day/7) >= 40  ~ rbinom(n(), size = 1, prob = N_unfed_DR_o40),
      Lifestage == "Nymph" & fed == 0 & dropped == 0 & (day/7) < 40   ~ rbinom(n(), size = 1, prob = N_unfed_DR_lt40),
      Lifestage == "Adult" & fed == 0 & dropped == 0 & (day/7) >= 40  ~ rbinom(n(), size = 1, prob = A_unfed_DR_o40),
      Lifestage == "Adult" & fed == 0 & dropped == 0 & (day/7) < 40   ~ rbinom(n(), size = 1, prob = A_unfed_DR_lt40),
      TRUE ~ 0)) %>%
    mutate(die = ifelse(replete_death+un_replete_death>=1,1,0)) %>%
    mutate(die = ifelse(Lifestage == "Larvae" & 
                          fed == 0 &
                          season == "fall",1,die)) %>%
    mutate(die = ifelse(Lifestage == "Nymph" & 
                          fed == 0 &
                          season == "fall" &
                          molt_death_immune == 0,1,die)) %>%
    mutate(die = ifelse(Lifestage == "Adult" & 
                          mated == 0 &
                          season == "summer" &
                          molt_death_immune == 0,1,die))
  
  die_list <- tick_agents %>% filter(die==1)
  die_list = die_list$Agent_ID
  
  deer_agents <<- deer_agents %>% 
    mutate(tick_links = ifelse(tick_links %in% die_list,0,tick_links))
  
  mouse_agents <<- mouse_agents %>%
    mutate(tick_links = ifelse(tick_links %in% die_list,0,tick_links))
  
  tick_agents <<- tick_agents %>%
    filter(num_ticks >= 0,
           die == 0) %>%
    dplyr::select(-c(replete_death,un_replete_death))
}
