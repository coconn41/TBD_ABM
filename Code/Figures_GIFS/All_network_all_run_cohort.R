rm(list=ls())
library(tidyverse)
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


for(i in c(1,6,7)){
  #combine all
  # all_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
  #                                 i,"_all_combined.csv"))
  # mouse_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
  #                                   i,"_mouse_combined.csv"))
  # deer_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
  #                                  i,"_deer_combined.csv"))
  ticks_combined <- read_csv(paste0("Post_sim_analysis/Cleaned_data/Individual_networks/Cleaned_with_repeats/Combined/Network_",
                                    i,"_ticks_combined.csv")) %>%
    mutate(network_ID = i)
  if(i == 1){ticks_combined1 = ticks_combined
  # mouse_combined1 = mouse_combined
  # deer_combined1 = deer_combined
  # all_combined1 = all_combined
  }
  if(i > 1){ticks_combined1 = rbind(ticks_combined,ticks_combined1)
  # mouse_combined1 = rbind(mouse_combined,mouse_combined1)
  # deer_combined1 = rbind(deer_combined,deer_combined1)
  # all_combined1 = rbind(all_combined,all_combined1)
  }
}

Tick_data <- ticks_combined1 %>%
  filter(Lifestage%in%c("Nymphs","Adults")) %>%
  group_by(Lifestage,cohort,network_ID,run_number) %>%
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
  mutate(cohort = factor(cohort,levels=c("ID","","1","2","3","4","5"))) %>%
  filter(cohort != "ID")

Tick_data_mean <- ticks_combined1 %>%
  filter(Lifestage%in%c("Nymphs","Adults")) %>%
  group_by(Lifestage,cohort,network_ID) %>%
  slice_min(simulation_day) %>%
  summarize(ha_perc = mean((tot_ha/total_ticks)*100),
         v1_perc = mean((tot_v1/total_ticks)*100)) %>%
  ungroup() %>%
  pivot_longer(c(ha_perc,v1_perc)) %>%
  mutate(name = ifelse(name=="ha_perc","Ap-ha (%)","Ap-v1 (%)"),
         network_ID = case_when(network_ID == 6 ~ paste0("Network ",2),
                                network_ID == 7 ~ paste0("Network ",3),
                                TRUE ~ paste0("Network ",network_ID)),
         cohort = as.character(cohort)) %>%
  select(network_ID,Lifestage,name,value,cohort) %>%
  bind_rows(.,comp_start_end) %>%
  mutate(cohort = factor(cohort,levels=c("ID","","1","2","3","4","5")))



p1=ggplot(data = Tick_data_mean %>% filter(cohort=="ID"),
          aes(x = cohort, y = value, fill = name))+
  geom_point(#position='dodge',
             aes(group=factor(name)),
                 position=position_dodge(1),size=3)+
  geom_point(data = Tick_data_mean %>% filter(cohort=="ID"),
             position=position_dodge(1),aes(color=name,
                                  group = factor(name)))+
  #geom_bar(stat = 'identity',position = 'dodge',color='black')+
  geom_point(data = Tick_data,
             aes(x = cohort, y = value,
                 group = factor(name)),
             size=3,
             position = position_dodge(1))+
  geom_point(data = Tick_data,
             aes(x = cohort, y = value, color = name,
                 group = factor(name)),
             position = position_dodge(1))+
  # geom_line(data = Tick_data,
  #            aes(x = cohort, y = value, color = name,
  #                group = interaction(factor(run_number),factor(name))),
  #            position = position_dodge(1))+
  geom_smooth(data = Tick_data,
              aes(x = cohort, y = value,
                  group = factor(name)),
              color = 'black',
              se = F,
              linewidth=2,
              position = position_dodge(1))+
  geom_smooth(data = Tick_data,
             aes(x = cohort, y = value, color = name,
                 group = factor(name)),
             position = position_dodge(1))+
  facet_grid(Lifestage~network_ID,scales = 'free_y')+
  geom_vline(xintercept = 2)+
  scale_fill_manual("Pathogen",values=c('#E41A1C','#377EB8'))+
  scale_color_manual("Pathogen",values=c('#E41A1C','#377EB8'))+
  ylab("Pathogen prevalence (%)")+
  xlab("Cohort")+
  theme_bw()+
  theme(text = element_text(size=20));p1

ggsave(plot = p1,
       filename = paste0(getwd(),'/Figures/Figures/Pathogen_prevalence.jpeg'),
       dpi = 300)
