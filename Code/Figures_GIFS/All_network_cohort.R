library(readr)
comp_start_end <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/comp_start_end.csv") %>%
  select(network_ID,Lifestage,ha_perc,v1_perc) %>%
  pivot_longer(c(v1_perc,ha_perc)) %>%
  mutate(network_ID = case_when(network_ID == 6 ~ paste0("Network ",2),
                                             network_ID == 7 ~ paste0("Network ",3),
                                             TRUE ~ paste0("Network ",network_ID)),
         cohort = "ID",
         name = ifelse(name=="ha_perc","Ap-ha (%)","Ap-v1 (%)")) %>%
  bind_rows(.,data.frame(network_ID = c("Network 1","Network 1",
                                        "Network 2","Network 2",
                                        "Network 2","Network 2",
                                        "Network 3","Network 3"),
                         Lifestage = c(rep("Adults",4),rep("Nymphs",4)),
                         name = rep(c("Ap-v1 (%)","Ap-ha (%)"),4),
                         value = rep(NA,8),
                         cohort = rep("",8)))

Tick_data <- read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_6_ticks.csv") %>%
  mutate(network_ID = 6) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_1_ticks.csv") %>%
              mutate(network_ID = 1)) %>%
  bind_rows(.,read_csv("Post_sim_analysis/Cleaned_data/Individual_networks/Network_7_ticks.csv") %>%
              mutate(network_ID = 7)) %>%
  filter(Lifestage%in%c("Nymphs","Adults")) %>%
  group_by(Lifestage,cohort,network_ID) %>%
  slice_min(simulation_day) %>%
  mutate(ha_perc = (tot_ha/total_ticks)*100,
         v1_perc = (tot_v1/total_ticks)*100) %>%
  pivot_longer(c(ha_perc,v1_perc)) %>%
  mutate(name = ifelse(name=="ha_perc","Ap-ha (%)","Ap-v1 (%)"),
         network_ID = case_when(network_ID == 6 ~ paste0("Network ",2),
                                network_ID == 7 ~ paste0("Network ",3),
                                TRUE ~ paste0("Network ",network_ID)),
         cohort = as.character(cohort)) %>%
  select(network_ID,Lifestage,name,value,cohort) %>%
  bind_rows(.,comp_start_end) %>%
  mutate(cohort = factor(cohort,levels=c("ID","","1","2","3","4","5")))



p1=ggplot(data = Tick_data,
       aes(x = cohort, y = value, fill = name))+
  geom_bar(stat = 'identity',position = 'dodge',color='black')+
  facet_grid(Lifestage~network_ID,scales = 'free_y')+
  geom_vline(xintercept = 2)+
  scale_fill_manual("Pathogen",values=c('#E41A1C','#377EB8'))+
  ylab("Pathogen prevalence (%)")+
  xlab("Cohort")+
  theme_bw()+
  theme();p1

ggsave(plot = p1,
       filename = paste0(getwd(),'/Figures/Figures/Pathogen_prevalence.jpeg'),
       dpi = 300)
