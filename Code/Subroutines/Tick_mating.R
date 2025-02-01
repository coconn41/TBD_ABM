mating_fn = function(tick_agents){
  multiple_tick = tick_agents %>%
    filter(links > 0,
           Lifestage == "Adult",
           mated != 1) %>%
    group_by(links) %>%
    summarise(tot = n()) %>%
    ungroup() %>%
    filter(tot > 1)
  
  multiple_deer = deer_agents %>%
    filter(Agent_ID %in% multiple_tick$links)
  
  multiple_tick2 = tick_agents %>%
    filter(mated!=1,
           Lifestage=="Adult",
           links %in% multiple_deer$Agent_ID)
unq_ind = 0
  if(length(multiple_tick2$links)>0){
    for(unq in unique(multiple_tick2$links)){
    unq_ind = unq_ind+1
    multiple_tick3 = multiple_tick2 %>% 
      filter(links==unq)
    females = multiple_tick3 %>%
      filter(sex=="female")
    males = multiple_tick3 %>%
      filter(sex=="male")
    if(nrow(females)!=0&nrow(males)!=0){
    if(nrow(females)==nrow(males)){
      mated_females = females %>%
        mutate(mated=1)
      dead_males = males}
    if(nrow(females)>nrow(males)){
    mated_females = females
    mated_females[sample(x = nrow(females), size = nrow(males)),]$mated = 1
    mated_females = mated_females %>%
      filter(mated==1)
    dead_males = males}
    if(nrow(females)<nrow(males)){
      mated_females = females %>%
        mutate(mated=1)
      dead_males = males
      dead_males[sample(x = nrow(males), size = nrow(females)),]$mated = 1}}
  if(nrow(females)==0|nrow(males)==0){
    mated_females = NULL
    dead_males = NULL
  }
  if(unq_ind==1){mated_females2 = mated_females
                 dead_males2 = dead_males}
  if(unq_ind>1){mated_females2 = rbind(mated_females,mated_females2)
                dead_males2 = rbind(dead_males,dead_males2)}
      # tick_agents <<- tick_agents %>%
      #   filter(!c(Agent_ID %in% mated_females$Agent_ID)) %>%
      #   filter(!c(Agent_ID %in% dead_males$Agent_ID)) %>%
      #   bind_rows(.,mated_females)
      
  }
tick_agents <<- tick_agents %>%
  filter(!c(Agent_ID %in% mated_females2$Agent_ID)) %>%
  filter(!c(Agent_ID %in% dead_males2$Agent_ID)) %>%
  bind_rows(.,mated_females2)
deer_agents <<- deer_agents %>%
  mutate(tick_links = ifelse(tick_links %in% dead_males2$Agent_ID,0,tick_links))
mouse_agents <<- mouse_agents %>%
  mutate(tick_links = ifelse(tick_links %in% dead_males2$Agent_ID,0,tick_links))
}
}
