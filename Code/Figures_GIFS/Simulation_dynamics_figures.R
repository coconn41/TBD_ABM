#####
# Load libraries:
#####
library(tidyverse)
library(sf)
library(tmap)
library(cols4all)
rm(list=ls())
#####
# Load data:
#####
patches <-read_sf(paste0(getwd(),'/Cached_data/fin_all_patch.shp')) %>%
  st_drop_geometry() %>%
  select(layer,metric,hectare) %>%
  distinct()

df = read_csv(paste0(getwd(),'/Simulations/Full_sim_df.csv'))
df = df[,-1]
patch_df <- left_join(df,patches,by = 'layer') %>%
  mutate(density = total_ticks/hectare)
#####
# Get simulations by result:
#####
sim_outcomes <- df %>%
  distinct(network_ID,sim_number,outcome) %>%
  group_by(network_ID,outcome) %>%
  summarize(tot = n()) %>%
  ungroup() %>%
  tidyr::complete(network_ID,outcome) %>%
  mutate(tot = ifelse(is.na(tot)==T,0,tot),
         percent = tot/10*100,
         network_ID = ifelse(network_ID>3,network_ID-1,network_ID)) 
# This is better as a table:
# Saved at paste0(getwd(),'/Figures/Tables/Simulation_result_table.xlsx')
#####
# Plot simulation outcomes:
#####
#Figure_3 <-
ggplot(data = sim_outcomes,
       aes(x = network_ID,
           y = percent,
           color = outcome,
           group = outcome))+
  geom_point()+
  geom_smooth(method = 'lm',se=F)+
  #scale_fill_manual("Outcome",values = c4a('brewer.set2',n=3))+
  scale_x_continuous(breaks=c(1:7))+
  theme_bw()
#####
# Group by simulation averages:
#####
average_data <- patch_df %>%
  group_by(network_ID,Lifestage,layer,metric) %>%
  summarize(mean_ticks = mean(total_ticks,na.rm = T),
            mean_density = sum(total_ticks, na.rm = T)/sum(hectare,na.rm = T),
            hectare = mean(hectare, na.rm = T)) %>%
  mutate(Lifestage = factor(Lifestage,levels = c("Eggs","Larvae","Nymph","Adult")),
         network_ID = paste0("Network ",ifelse(network_ID>3,network_ID-1,network_ID)))

#####
# Create plots:
#####
p1 = ggplot(data = average_data %>% filter(Lifestage=="Adult"),
            aes(x = metric,
                y = log(hectare)))+
  geom_point(position = position_jitter())+
  geom_smooth(data = average_data,
              aes(x = metric,
                  y = log(hectare)),
              method = 'lm',
              inherit.aes = F,
              color = 'black')+
  ylab(expression(log[10](hectare)))+
  xlab("SCR connectivity")+
  facet_grid(.~network_ID)+
  theme_bw();p1

p2=ggplot(data = average_data,
          aes(x = metric,
              y = log(mean_ticks),
              color = Lifestage))+
  geom_point(position = position_jitter())+
  geom_smooth(data = average_data,
              aes(x = metric,
                  y = log(mean_ticks)),
              method = 'lm',
              inherit.aes = F,
              color = 'black')+
  scale_color_manual("Lifestage", values = c4a("brewer.set2",n=4))+
  ylab(expression(log[10]("Mean ticks")))+
  xlab("SCR connectivity")+
  facet_grid(.~network_ID)+
  theme_bw();p2

p3=ggplot(data = average_data,
          aes(x = metric,
              y = log(mean_density),
              color = Lifestage))+
  geom_point(position = position_jitter())+
  geom_smooth(data = average_data,
              aes(x = metric,
                  y = log(mean_density)),
              method = 'lm',
              inherit.aes = F,
              color = 'black')+
  scale_color_manual("Lifestage", values = c4a("brewer.set2",n=4))+
  ylab(expression(log[10]("Mean tick density")))+
  xlab("SCR connectivity")+
  facet_grid(.~network_ID)+
  theme_bw();p3

Figure_4 <-ggarrange(p1,p2,p3,ncol=1)

full_dat <- average_data %>%
  rbind(.,average_data %>% 
          filter(Lifestage=="Adult") %>%
          mutate(Lifestage="Patches")) %>%
  mutate(Lifestage = factor(case_when(Lifestage == "Nymph" ~ "Nymphs",
                               Lifestage == "Adult" ~ "Adults",
                               .default = as.character(Lifestage)),
                            levels = c("Patches","Eggs","Larvae","Nymphs","Adults"))) %>%
  pivot_longer(cols = c("hectare","mean_ticks","mean_density")) %>%
  mutate(remover = ifelse(name == "mean_density" & Lifestage == "Patches",1,
                          ifelse(name == "mean_ticks" & Lifestage == "Patches",1,0)),
         name = factor(name,levels=c("hectare","mean_ticks","mean_density"))) %>%
  filter(remover!=1)
  
  
Figure_4 <-
ggplot(data = full_dat,
       aes(x = metric,
           y = log(value),
           color = Lifestage))+
  geom_point() +
  geom_smooth(method = 'lm',col='red')+
  facet_grid(name ~ network_ID,
             scales = "free_y",
             labeller = labeller(name = as_labeller(
               c(hectare = "log(Hectares)",
                 `mean_ticks` = "log(Mean~ticks)",
                 `mean_density` = "log(Mean~tick~density)"),
               label_parsed)))+
  scale_color_manual(" ",values = c("black",c4a("brewer.set2",n=4)))+
  xlab("SCR Connectivity")+
  ylab("Value")+
  theme_bw()+
  theme(text = element_text(size = 15))

ggsave(plot = Figure_4,
       filename = paste0(getwd(),'/Figures/Figures/Connectivity_density_plots.jpeg'),
       dpi = 300,
       width = 12,
       height = 9)

