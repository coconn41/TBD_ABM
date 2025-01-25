tick_death = function(tick_agents){
  tick_agents <<- tick_agents %>%
    mutate(replete_death = ifelse(Lifestage=="Larvae" & fed == 1 & dropped == 1,
                                  rbinom(n = 1, size = 1, prob = L_rep_DR),
                                  ifelse(Lifestage == "Nymph" & fed == 1 & dropped == 1,
                                         rbinom(n=1, size=1, prob = N_rep_DR),
                                         ifelse(Lifestage=="Adult" & fed == 1 & dropped == 1,
                                                rbinom(n=1,size=1,A_rep_DR),
                                                0)))) %>%
    mutate(un_replete_death = ifelse(Lifestage == "Larvae" & fed == 0 & dropped == 0 & (day/7) >=40,
                                     rbinom(n = 1, size = 1, prob = L_unfed_DR_o40),
                                     ifelse(Lifestage == "Larvae" & fed == 0 & dropped == 0 & (day/7) <40,
                                            rbinom(n = 1, size = 1, prob = L_unfed_DR_lt40),
                                            ifelse(Lifestage == "Nymph" & fed == 0 & dropped == 0 & (day/7) >=40,
                                                   rbinom(n = 1, size = 1 ,prob = N_unfed_DR_o40),
                                                   ifelse(Lifestage == "Nymph" & fed == 0 & dropped == 0 & (day/7) <40,
                                                          rbinom(n = 1, size = 1, N_unfed_DR_lt40),
                                                          ifelse(Lifestage == "Adult" & fed == 0 & dropped == 0 & (day/7) >=40,
                                                                 rbinom(n = 1, size = 1, prob = A_unfed_DR_o40),
                                                                 ifelse(Lifestage == "Adult" & fed == 0 & dropped == 0 & (day/7) <40,
                                                                        rbinom(n = 1, size = 1, prob = A_unfed_DR_lt40),0))))))) %>%
    mutate(die = ifelse(sum(replete_death,un_replete_death)>=1,1,0)) %>%
    mutate(die = ifelse(Lifestage == "Larvae" & 
                          fed == 0 &
                          season == "fall",1,die)) %>%
    mutate(die = ifelse(Lifestage == "Nymph" & 
                          fed == 0 &
                          season == "fall",1,die)) %>%
    mutate(die = ifelse(Lifestage == "Adult" & 
                          mated == 0 &
                          season == "summer",1,die)) %>%
    filter(num_ticks >= 0,
           die == 0) %>%
    dplyr::select(-c(replete_death,un_replete_death))
}
