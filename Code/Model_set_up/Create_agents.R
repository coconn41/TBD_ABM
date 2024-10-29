deer_agents = fin_poly %>%
  filter(is.na(deer_agents)==F) %>%
  uncount(deer_agents) %>%
  select(layer)
