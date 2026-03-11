#####
# Load libraries
#####
library(readr)
library(tidyverse)

#####
# Load data:
#####

Deer_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_deer.csv") %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_deer.csv")) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_deer.csv")) %>%
  # group_by(network_ID,layer,timestep,Agent) %>%
  # summarize(total = sum(tot_deer),
  #           pathogen_total = sum(tot_v1_infected)) %>%
  rename(Population = "tot_deer",
         Pathogen_total = "tot_v1_infected") %>%
  mutate(Pathogen = "Ap-v1",
         Pathogen_prevalence = (Pathogen_total/Population)*100) %>%
  select(network_ID,layer,timestep,Agent,Population,Pathogen_total,Pathogen,Pathogen_prevalence) %>%
  filter(timestep >= 5064) %>%
  mutate(year = as.numeric(substring(timestep/8760,1,1))+1)

Mouse_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_mice.csv") %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_mice.csv")) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_mice.csv")) %>%
 # group_by(network_ID,layer,timestep,Agent) %>%
  # summarize(total = sum(tot_mice),
  #           pathogen_total = sum(tot_ha_infected)) %>%
  rename(Population = "tot_mice",
         Pathogen_total = "tot_ha_infected") %>%
  mutate(Pathogen = "Ap-ha",
         Pathogen_prevalence = (Pathogen_total/Population)*100) %>%
  select(network_ID,layer,timestep,Agent,Population,Pathogen_total,Pathogen,Pathogen_prevalence)%>%
  filter(timestep >= 5064) %>%
  mutate(year = as.numeric(substring(timestep/8760,1,1))+1)

Host_data = bind_rows(Deer_data,Mouse_data) %>%
  mutate(network_ID = case_when(network_ID == 6 ~ 2,
                                network_ID == 7 ~ 3,
                                TRUE ~ network_ID),
         Agent = case_when(Agent == "Deer" ~ "Deer (Ap-v1)",
                           TRUE ~ "Mice (Ap-ha)"))

Grouped_host_data = Host_data %>%
  group_by(network_ID,Agent,Pathogen,timestep,year) %>%
  summarize(Pathogen_prevalence = (sum(Pathogen_total)/sum(Population))*100)

min_slices = Grouped_host_data %>%
  group_by(year) %>%
  slice_min(timestep) %>%
  ungroup() %>%
  select(timestep) %>%
  distinct() %>%
  as.vector() %>%
  unlist() %>%
  as.numeric()

p1 = ggplot(data = Host_data,
       aes(x = timestep, y = Pathogen_prevalence, group = layer)) + 
  geom_line(alpha=.1)+
  geom_smooth(data = Grouped_host_data,
              aes(x = timestep, y = Pathogen_prevalence, color = as.factor(network_ID), group = as.factor(network_ID)))+
  facet_grid(. ~ Agent)+
  theme_bw()+
  theme(text = element_text(size=20))+
  scale_color_manual("Network",values = tmaptools::get_brewer_pal(n=3,"Set1"))+
  scale_x_continuous(breaks = min_slices[-1],
                     labels = 1:7)+
  ylab("Prevalence (%)")+
  xlab("Year");p1

ggsave(p1,
       filename = paste0(getwd(),'/Figures/Figures/Host_pathogen_prevalence.jpeg'),
       dpi = 300)
