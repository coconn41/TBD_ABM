# If on running via a HPCC slurm script, set wd
setwd("/user/collinoc/Cluster_TBD_ABM/")

# Clear model environment:
rm(list=ls())

# Load libraries:
source(paste0(getwd(),'/Code/Model_set_up/Load_libraries.R'))

# Set random number state:
set.seed(1)

# Set number of cores:
cores = 8
if(detectCores()>10){computer = "Cluster"
large_cores = 24}
if(detectCores()==10){computer = "Personal"
large_cores = 8}

#####
# Calculate data or load data:
#####
calculate_data = FALSE
if(calculate_data==TRUE){source(paste0(getwd(),'/Code/Model_set_up/Calc_mod_setup.R'))}

#####
# Load data:
#####
# Agents and patches:
source(paste0(getwd(),'/Code/Model_set_up/Load_model_environment.R'))
# Sunlight times (for daily activity):
source(paste0(getwd(),'/Code/Model_set_up/Sunlight_times.R'))

#####
# Load parameter values
#####
source(paste0(getwd(),"/Code/Parameters/Parameter_script.R"))

#####
# Load model subroutines:
#####
source(paste0(getwd(),'/Code/Subroutines/Update_environment.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Mouse_movement.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Deer_movement.R')) # Good
#source(paste0(getwd(),'/Code/Subroutines/Other_movement.R')) # Not updated yet
source(paste0(getwd(),'/Code/Subroutines/Create_deer_paths.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_attachment.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Groom_attached_ticks.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_mating.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_timer.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_drop_off.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Tick_molting.R'))
source(paste0(getwd(),'/Code/Subroutines/lay_eggs.R')) # Good
source(paste0(getwd(),'/Code/Subroutines/Transfer_pathogens.R'))
source(paste0(getwd(),'/Code/Subroutines/Tick_death.R'))
source(paste0(getwd(),'/Code/Subroutines/Host_timer.R'))
source(paste0(getwd(),'/Code/Subroutines/Kill_hosts.R'))
source(paste0(getwd(),'/Code/Master/Compile_results.R'))

# Model starting timing:
year=0
day=265 
daytime = "night"
season = "fall"

# Number of hourly timesteps
go_timesteps = (8760*5)

# Select network: either "all" or the network number
net_select = 1

# Parameter modification:
deer_attach_prob = .275
mouse_attach_prob = .275
deer_infect_tick_v1 = 1# 0.0094
mouse_infect_tick_ha =1# 0.0665
deer_trans_param = deer_infect_tick_v1
mouse_trans_param = mouse_infect_tick_ha


if(net_select!="all"){
  deer_agents = deer_agents %>% 
    filter(network_ID == net_select)
  mouse_agents = mouse_agents %>%
    filter(network_ID == net_select)
  tick_agents = tick_agents %>%
    filter(network_ID == net_select)
  nymph_agents = nymph_agents %>%
    filter(network_ID == net_select)
  jump_probability_df = jump_probability_df %>%
    filter(network_ID == net_select)
  aspatial_network = aspatial_network %>%
    filter(network_ID == net_select)
  network1 = network1 %>%
    filter(network_ID == net_select)
  reduced_patches = reduced_patches %>%
    filter(network_ID == net_select)
  spat_network = spat_network %>%
    filter(network_ID == net_select)}


#pb = txtProgressBar(min = 1, max = go_timesteps, initial = 1) 
#start_time = Sys.time()

#####
# Clear useless variables:
#####
deer_agents = deer_agents %>% 
  select(-c(County,Site,metric,Ha_infected,Ha_infection_timer))
mouse_agents = mouse_agents %>%
  select(-c(County,Site,metric,V1_infected,V1_infection_timer))
tick_agents = tick_agents %>%
  select(-c(County,Site,metric))

#####
# Preallocate compiler data frames:
#####
tdflength = length(unique(mouse_agents$layer))*4*go_timesteps
hdflength = length(unique(mouse_agents$layer))*go_timesteps
tick_data2 = data.frame(Lifestage = character(tdflength),
                        network_ID = numeric(tdflength),
                        layer = numeric(tdflength),
                        ha_perc = numeric(tdflength),
                        v1_perc = numeric(tdflength),
                        tot_v1 = numeric(tdflength),
                        tot_ha = numeric(tdflength),
                        total_ticks = numeric(tdflength),
                        Agent = character(tdflength),
                        year = numeric(tdflength),
                        timestep = numeric(tdflength),
                        day_of_year = numeric(tdflength),
                        network = numeric(tdflength),
                        season = character(tdflength),
                        simulation_day = numeric(tdflength),
                        simulation_week = numeric(tdflength))
deer_data2 = data.frame(network_ID = numeric(hdflength),
                        layer = numeric(hdflength),
                        tot_v1_infected = numeric(hdflength),
                        tot_deer = numeric(hdflength),
                        v1_perc = numeric(hdflength),
                        day_of_year = numeric(hdflength),
                        season = character(hdflength),
                        timestep = numeric(hdflength),
                        year = numeric(hdflength),
                        network = numeric(hdflength),
                        Agent = character(hdflength))
mouse_data2 = data.frame(network_ID = numeric(hdflength),
                         layer = numeric(hdflength),
                         tot_ha_infected = numeric(hdflength),
                         tot_mice = numeric(hdflength),
                         ha_perc = numeric(hdflength),
                         day_of_year = numeric(hdflength),
                         season = character(hdflength),
                         timestep = numeric(hdflength),
                         year = numeric(hdflength),
                         network = numeric(hdflength),
                         Agent = character(hdflength))

for(i in 1:go_timesteps){
  
  
  #####
  # Update environment
  #####
  day_hour <- (i%%24)+1
  if(day_hour==1){day <- day+1}
  if(day==366){day <- 1}
  if(day==1&day_hour==1){year <- year+1}
  daytime <- ifelse(day_hour >= daylight[which(daylight$dayofyear==(day%%262)+1),2]&
                      day_hour <daylight[which(daylight$dayofyear==(day%%262)+1),3],
                    "day",'night')
  if(day==79){season <- "spring"}
  if(day==171){season <- "summer"}
  if(day==265){season <- "fall"}
  if(day==355){season <- "winter"}
  
  #####
  # Add nymphs in during summer of Year 1
  #####
  if(year==1&day==171 & exists("nymph_agents")){
    tick_agents = rbind(tick_agents,nymph_agents %>%
                          mutate(att_prob = NA) %>%
                          select(-c(County,Site,metric)))
    remove(nymph_agents)
  }
  
  #####
  # Move mice
  #####
  if(daytime=="day"){mouse_agents <- mouse_agents %>%
    mutate(movement = case_when(row==gridrows & !col%in%c(1,gridcols) ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
                                row==1 & !col%in%c(1,gridcols) ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
                                col==gridcols & !row%in%c(1,gridrows) ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
                                col==1 & !row%in%c(1,gridrows) ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
                                row==gridrows & col==gridcols ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
                                row==gridrows & col==1 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
                                row==1 & col==gridcols ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
                                row==1 & col==1 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
                                TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
           # movement = case_when(possibilities==2 ~ as.numeric(sample(c(1:6),nrow(.),replace=T)),
           #                      possibilities==3 ~ as.numeric(sample(c(4:9),nrow(.),replace=T)),
           #                      possibilities==4 ~ as.numeric(sample(c(1,2,4,5,7,8),nrow(.),replace=T)),
           #                      possibilities==5 ~ as.numeric(sample(c(2,3,5,6,8,9),nrow(.),replace=T)),
           #                      possibilities==6 ~ as.numeric(sample(c(1,2,4,5),nrow(.),replace=T)),
           #                      possibilities==7 ~ as.numeric(sample(c(2,3,5,6),nrow(.),replace=T)),
           #                      possibilities==8 ~ as.numeric(sample(c(4,5,7,8),nrow(.),replace=T)),
           #                      possibilities==9 ~ as.numeric(sample(c(5,6,8,9),nrow(.),replace=T)),
           #                      TRUE ~ as.numeric(sample(c(1:9),nrow(.),replace=T))),
           row = case_when(movement==1 ~ row-1,
                           movement==2 ~ row-1,
                           movement==3 ~ row-1,
                           movement==7 ~ row+1,
                           movement==8 ~ row+1,
                           movement==9 ~ row+1,
                           TRUE ~ row),
           col = case_when(movement==1 ~ col-1,
                           movement==3 ~ col+1,
                           movement==4 ~ col-1,
                           movement==6 ~ col+1,
                           movement==7 ~ col-1,
                           movement==9 ~ col+1,
                           TRUE ~ col),
           locs = paste0(row,",",col,",",network_ID)) %>%
    mutate(prob = runif(n = nrow(.),min = 0,max=1)) %>%
    arrange(desc(prob))}
  
  # Move others
  #other_movement(other_agents,daytime)
  
  #####
  # Move deer
  #####
  if(daytime=="day"){deer_agents <- deer_agents %>%
    mutate(possibility_row_min = row-100,
           possibility_row_max = row+100,
           possibility_col_min = col-100,
           possiblitiy_col_max = col+100) %>%
    group_by(Agent_ID) %>%
    mutate(new_row = sample(possibility_row_min:possibility_row_max,size = 1),
           new_col = sample(possibility_col_min:possiblitiy_col_max,size = 1)) %>%
    mutate(jump_patch = ifelse(new_row<=0|new_col<=0,1,0)) %>%
    mutate(new_patch = ifelse(jump_patch == 1,
                              sample(subset(jump_probability_df,
                                            origin_ID==layer&
                                              network_ID==network_ID)$destination_ID,
                                     size = 1,
                                     prob = subset(jump_probability_df,
                                                   origin_ID==layer,
                                                   network_ID==network_ID)$probability),
                              layer)) %>%
    mutate(new_row = ifelse(new_patch!=layer,round(runif(n=1,
                                                         min = 0,
                                                         max = subset(reduced_patches,# %>% st_drop_geometry(),
                                                                      layer==layer)$gridrows)),
                            new_row),
           new_col = ifelse(new_patch!=layer,round(runif(n=1,
                                                         min = 0,
                                                         max = subset(reduced_patches,# %>% st_drop_geometry(),
                                                                      layer==layer)$gridcols)),
                            new_col)) %>%
    mutate(old_row = row,
           old_col = col,
           row = new_row,
           col = new_col,
           layer = new_patch,
           locs = paste0(row,",",col,",",network_ID),
           jump_patch = 0) }
  #####
  # Create deer paths
  #####
  if(day>=lay_egg){
    if(daytime=="day"){deer_paths <- deer_agents %>%
      filter(jump_patch==0) %>%
      select(Agent_ID,network_ID,layer,old_row,old_col,new_row,new_col) %>%
      rowwise() %>%
      mutate(cells = list(bresenham_line(old_col, old_row, new_col, new_row))) %>%
      ungroup() %>%
      unnest(cells) %>%
      mutate(row = cells[,1],
             col = cells[,2]) %>%
      select(Agent_ID,network_ID,layer,row,col) %>%
      distinct() %>%
      group_by(network_ID,layer,row,col) %>%
      mutate(locs = paste0(row,",",
                           col,",",
                           layer,",",
                           network_ID)) %>%
      ungroup() %>%
      group_by(Agent_ID) %>%
      mutate(prob = runif(min=0,max=1,n=1)) %>%
      arrange(prob)}
    
    #####    
    # Attach ticks
    #####  
    
    if(daytime=="day"){T_matches1 <- tick_agents %>% 
      filter(Lifestage!="Eggs") %>%
      filter(links==0&dropped==0)
    
    # This includes only ticks that are not linked to a host
    # and also have not already succeeded in a bloodmeal and
    # checks to see if deer have crossed their path
    
    T_matches2 <- T_matches1 %>%
      mutate(deer_links = match(paste0(.$row,",",
                                       .$col,",",
                                       .$layer,",",
                                       .$network_ID),
                                deer_paths$locs,NA),
             att_prob = rbinom(n = n(),
                               size = 1,
                               prob = deer_attach_prob)) %>%
      mutate(deer_links = case_when(deer_links * att_prob == 0 ~ NA_real_,
                                    TRUE ~ deer_links)) 
    
    # This includes only ticks that are not linked to a host
    # and also have not already succeeded in a bloodmeal and
    # also includes only newly attached ticks, it then assigns them
    # the agent ID of the deer that picked them up  
    T_matches3 <- T_matches2 %>%
      mutate(deer_links = deer_paths[deer_links,]$Agent_ID)
    
    
    T_matches4 = T_matches3 %>%
      mutate(mouse_links = match(paste0(.$row,",",
                                        .$col,",",
                                        .$layer,",",
                                        .$network_ID),
                                 paste0(mouse_agents$row,",",
                                        mouse_agents$col,",",
                                        mouse_agents$layer,",",
                                        mouse_agents$network_ID),NA),
             att_prob = rbinom(n = n(),
                               size = 1,
                               prob = mouse_attach_prob)) %>%
      mutate(mouse_links = case_when(mouse_links * att_prob == 0 ~ NA_real_,
                                     TRUE ~ mouse_links)) 
    
    T_matches5 = T_matches4 %>%
      mutate(mouse_links = mouse_agents[.$mouse_links,]$Agent_ID)
    #%>% # Below is miscoded
    # mutate(mouse_links = ifelse(Lifestage == "Larvae",ifelse(rbinom(n=1,
    #                                                                size=1,
    #                                                                prob=LA_probability)==1,
    #                                                         mouse_links,0),
    #                            ifelse(Lifestage == "Nymph",ifelse(rbinom(n=1,
    #                                                                      size=1,
    #                                                                      prob=NA_probability)==1,
    #                                                               mouse_links,0),
    #                                   ifelse(Lifestage == "Adult",ifelse(rbinom(n=1,
    #                                                                             size=1,
    #                                                                             prob=AA_probability)==1,
    #                                                                      mouse_links,0),0)))) 
    
    tick_agents <- T_matches5 %>%
      mutate(selection = case_when(deer_links>0&mouse_links>0&Lifestage=="Nymph" ~ rbinom(n=n(),size = 1,prob = N_prob),
                                   deer_links>0&mouse_links>0&Lifestage=="Larvae" ~ rbinom(n=n(),size=1,prob = L_prob),
                                   deer_links>0&mouse_links>0&Lifestage=="Adult" ~ 0,
                                   is.na(deer_links)==T & mouse_links>0 ~ 1,
                                   deer_links > 0 & is.na(mouse_links)==T ~ 0,
                                   TRUE ~ -1),
             linked_type = case_when(selection == 1 ~ "Mouse",
                                     selection == 0 ~ "Deer",
                                     selection == -1 ~ "N",
                                     TRUE ~ "N"),
             links = case_when(selection == 1 ~ mouse_links,
                               selection == 0 ~ deer_links,
                               TRUE ~ NA_real_)) %>%
      dplyr::select(-c(deer_links,mouse_links,selection)) %>%
      rbind(.,tick_agents %>% filter(links>0|dropped>0|dropped==-1|Lifestage=="Eggs"))  # This combines back with already linked ticks
    
    # non_links = tick_agents %>% 
    #   filter(is.na(links)==F)
    
    deer_agents <- tick_agents %>%
      filter(is.na(links)==F) %>%
      group_by(links) %>%
      summarise(Agent_ID = list(Agent_ID), .groups = "drop") %>%
      rename(tick_links = "Agent_ID",
             Agent_ID = "links") %>%
      right_join(deer_agents, #%>% 
                 #  mutate(tick_links = ifelse(tick_links==0,NA_real_,tick_links)),
                 by = "Agent_ID") %>%
      mutate(combined_list = mapply(function(a,b) {
        unique(c(a,b))
      }, tick_links.x, tick_links.y, SIMPLIFY = FALSE)
      ) %>%
      select(-c(tick_links.x,tick_links.y)) %>%
      rename(tick_links = "combined_list")
    
    # deer_agents <- deer_agents %>%
    #   mutate(tick_links = non_links[match(Agent_ID,non_links$links),]$Agent_ID) %>%
    #   mutate(tick_links = ifelse(is.na(tick_links)==T,0,tick_links))
    
    mouse_agents <- tick_agents %>%
      filter(is.na(links)==F) %>%
      group_by(links) %>%
      summarise(Agent_ID = list(Agent_ID), .groups = "drop") %>%
      rename(tick_links = "Agent_ID",
             Agent_ID = "links") %>%
      right_join(mouse_agents,#%>% 
                 # mutate(tick_links = ifelse(tick_links==0,NA_real_,tick_links)),
                 by = "Agent_ID") %>%
      mutate(combined_list = mapply(function(a,b) {
        unique(c(a,b))
      }, tick_links.x, tick_links.y, SIMPLIFY = FALSE)
      ) %>%
      select(-c(tick_links.x,tick_links.y)) %>%
      rename(tick_links = "combined_list")
    
    # mouse_agents <- mouse_agents %>%
    #   mutate(tick_links = non_links[match(Agent_ID,non_links$mouse_links),]$Agent_ID) %>%
    #   mutate(tick_links = ifelse(is.na(tick_links)==T,0,tick_links))
    
    tick_agents <- tick_agents %>%
      mutate(links = ifelse(is.na(links)==T,0,links))
    }
    
    #####
    # Transfer pathogens
    #####
    if(exists('deer_paths')==F){tick_agents$transfer_type = "None"}
    if(exists('deer_paths')==T){
      tick_agents <- tick_agents %>%
        mutate(transfer_type = case_when(links>0 & 
                                           time_on_host >=24 &
                                           ((sex == "female" & Lifestage == "Adult") |
                                              (Lifestage == "Larvae") |
                                              (Lifestage == "Nymph")) &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Deer" &
                                           Infection_status == "v1" &
                                           deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "t2dv1",
                                         links>0 & 
                                           ((sex == "female" & Lifestage == "Adult") |
                                              (Lifestage == "Larvae") |
                                              (Lifestage == "Nymph")) &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Deer" &
                                           Infection_status == "None" &
                                           deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==1 ~ "d2tv1",
                                         # links>0 & 
                                         #   time_on_host >=24 &
                                         #   sex == "female" &
                                         #   Lifestage != "Eggs" &
                                         #   attempted_pathogen_transfer == 0 &
                                         #   linked_type == "Deer" &
                                         #   Infection_status == "None" &
                                         #   deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "None",
                                         links>0 & 
                                           time_on_host >=24 &
                                           ((sex == "female" & Lifestage == "Adult") |
                                              (Lifestage == "Larvae") |
                                              (Lifestage == "Nymph")) &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Mouse" &
                                           Infection_status == "ha" &
                                           mouse_agents[match(links,mouse_agents$Agent_ID),]$Ha_infected==0 ~ "t2mha",
                                         links>0 & 
                                           ((sex == "female" & Lifestage == "Adult") |
                                              (Lifestage == "Larvae") |
                                              (Lifestage == "Nymph")) &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Mouse" &
                                           Infection_status == "None" &
                                           mouse_agents[match(links,mouse_agents$Agent_ID),]$Ha_infected==1 ~ "m2tha",
                                         TRUE ~ "None")) %>%
        mutate(Infection_status = case_when(transfer_type == "d2tv1" & Lifestage != "Larvae" ~ transfer_outcomes_v1[rbinom(n = n(), size = 1, prob = deer_infect_tick_v1)+1],
                                            transfer_type == "m2tha" & Lifestage != "Larvae" ~ transfer_outcomes_ha[rbinom(n = n(), size = 1, prob = mouse_infect_tick_ha)+1],
                                            transfer_type == "d2tv1" & Lifestage == "Larvae" ~ "v1",
                                            transfer_type == "m2tha" & Lifestage == "Larvae" ~ "ha",
                                            transfer_type == "None" ~ Infection_status,
                                            TRUE ~ Infection_status))
      
      d_matches1 <- deer_agents %>%
        filter(map_lgl(tick_links, ~ identical(.,0)) | V1_infected == 1)
      
      t2dv1_ids <- tick_agents$Agent_ID[tick_agents$transfer_type == "t2dv1"]
      
      deer_agents <- deer_agents %>%
        filter(map_lgl(tick_links, ~ !identical(.,0)) & V1_infected == 0)
      
      deer_agents$tick_links <- map(deer_agents$tick_links, ~ .x[!is.na(.x)])
      deer_agents$transfer_count <- vapply(deer_agents$tick_links, function(x) sum(x %in% t2dv1_ids), integer(1))
      new_infections <- rbinom(n = nrow(deer_agents), size = deer_agents$transfer_count, prob = tick_infect_deer_v1) > 0
      deer_agents$V1_infected <- ifelse(deer_agents$V1_infected == 0 & new_infections, 1, deer_agents$V1_infected)
      deer_agents = subset(deer_agents, select = -c(transfer_count))
      deer_agents <- deer_agents %>%
        rbind(.,d_matches1)
      
      m_matches1 <- mouse_agents %>%
        filter(map_lgl(tick_links, ~ identical(.,0)) | Ha_infected == 1)
      
      t2mha_ids <- tick_agents$Agent_ID[tick_agents$transfer_type == "t2mha"]
      
      mouse_agents <- mouse_agents %>%
        filter(map_lgl(tick_links, ~ !identical(.,0)) & Ha_infected == 0)
      
      mouse_agents$tick_links <- map(mouse_agents$tick_links, ~ .x[!is.na(.x)])
      mouse_agents$transfer_count <- vapply(mouse_agents$tick_links, function(x) sum(x %in% t2mha_ids), integer(1))
      new_infections <- rbinom(n = nrow(mouse_agents), size = mouse_agents$transfer_count, prob = tick_infect_mouse_ha) > 0
      mouse_agents$Ha_infected <- ifelse(mouse_agents$Ha_infected == 0 & new_infections, 1, mouse_agents$Ha_infected)
      mouse_agents = subset(mouse_agents, select = -c(transfer_count))
      mouse_agents <- mouse_agents %>%
        rbind(.,m_matches1)
      
      tick_agents <- tick_agents %>%
        mutate(attempted_pathogen_transfer = ifelse(links>0 & 
                                                      Infection_status != "None" &
                                                      transfer_type != "None" & 
                                                      attempted_pathogen_transfer == 0,1,attempted_pathogen_transfer))
    }
    #####  
    # Groom ticks
    #####  
    if(daytime=="day"){groomed_ticks_deer <- c()
    
    deer_agents <- deer_agents %>%
      mutate(num_linked = lengths(tick_links)-1) %>%
      mutate(tick_links = map2(tick_links, num_linked, ~ {
        if (rbinom(1, 1, deer_GR) == 1) {  
          if (length(.x) > 0) {
            idx <- sample(length(.x), 1)  # Choose a random index
            groomed_ticks_deer <<- c(groomed_ticks_deer, .x[idx])  # Store removed element
            result <- .x[-idx]  # Remove the element
            if (length(result) == 0) NA else result  # Replace empty list with NA
          } else {
            .x
          }
        } else {
          .x  
        }
      })) %>%
      select(-num_linked)
    
    groomed_ticks_mice <- c() 
    
    mouse_agents <- mouse_agents %>%
      mutate(num_linked = lengths(tick_links)-1) %>%
      mutate(tick_links = map2(tick_links, num_linked, ~ {
        if (rbinom(1, 1, mouse_GR) == 1) {  
          if (length(.x) > 0) {
            idx <- sample(length(.x), 1)  # Choose a random index
            groomed_ticks_mice <<- c(groomed_ticks_mice, .x[idx])  # Store removed element
            result <- .x[-idx]  # Remove the element
            if (length(result) == 0) NA else result  # Replace empty list with NA
          } else {
            .x
          }
        } else {
          .x  
        }
      })) %>%
      select(-num_linked)
    
    tick_agents <- tick_agents %>%
      filter(!c(Agent_ID%in%groomed_ticks_deer),
             !c(Agent_ID%in%groomed_ticks_mice))
    }
    #####  
    # Mate ticks
    #####
    if(daytime=="day"){
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
        tick_agents <- tick_agents %>%
          filter(!c(Agent_ID %in% mated_females2$Agent_ID)) %>%
          filter(!c(Agent_ID %in% dead_males2$Agent_ID)) %>%
          bind_rows(.,mated_females2)
        
        # Below must filter out based on condition, use the same as grooming
        dm2_ids = unique(dead_males2$Agent_ID)
        
        deer_agents$tick_links = map(deer_agents$tick_links, ~ .x[!(.x %in% dm2_ids)])
        mouse_agents$tick_links = map(mouse_agents$tick_links, ~ .x[!(.x %in% dm2_ids)])
        
        # deer_agents <<- deer_agents %>%
        #   mutate(tick_links = map())
        #   map(deer_agents$links, ~ .x[!(.x %in% df2_set)])
        #   mutate(tick_links = map(tick_links, ~ .x[!(.x %in% dead_males2$Agent_ID)]))
        #mutate(tick_links = map(tick_links, ~ discard(.x, ~ .x %in% dead_males2$Agent_ID)))
        #mutate(tick_links = map(tick_links, ~ .x[!(.x %in% dead_males2$Agent_ID)])) # Update to use list
        #mutate(tick_links = ifelse(tick_links %in% dead_males2$Agent_ID,0,tick_links))
        # mouse_agents <<- mouse_agents %>%
        #   mutate(tick_links = map(tick_links, ~ .x[!(.x %in% dead_males2$Agent_ID)]))
        #mutate(tick_links = map(tick_links, ~ discard(.x, ~ .x %in% dead_males2$Agent_ID)))
        #mutate(tick_links = map(tick_links, ~ .x[!(.x %in% dead_males2$Agent_ID)]))
        #mutate(tick_links = ifelse(tick_links %in% dead_males2$Agent_ID,0,tick_links)) # Update to use list
      }
    }
  }  
  #####  
  # Tick timer
  #####
  tick_agents <- tick_agents %>%
    mutate(tick_age_wks = ifelse(Lifestage!="Eggs",tick_age_wks+(1/168),0), # hours per week
           time_since_mating = ifelse(mated==1,time_since_mating+1,0),
           time_since_fed = ifelse(fed==1,time_since_fed + 1,0),
           time_on_host = ifelse(links>0&dropped==0,time_on_host+1,
                                 ifelse(dropped==1,time_on_host,0)),
           fed = case_when(Lifestage == "Larvae" & time_on_host >= (3*24) & dropped == 0 ~ 1,
                           Lifestage == "Nymph" & time_on_host >=(5*24) & dropped == 0 ~ 1,
                           Lifestage == "Adult" & time_on_host >=(10*24) & dropped == 0 ~ 1,
                           TRUE ~ fed),
           molt_death_immune = case_when(Lifestage == "Nymph" &
                                           season == "summer" &
                                           molt_death_immune == 1 ~ 0,
                                         Lifestage == "Adult" &
                                           mated == 0 &
                                           season == "fall" &
                                           molt_death_immune == 1 ~ 0,
                                         TRUE ~ molt_death_immune)) 
  
  #####  
  # Tick drop off
  #####
  L_ticks = tick_agents %>%
    filter(Lifestage == "Larvae",
           fed == 1,
           dropped == 0) %>%
    mutate(dropped = sum(rbinom(n = n(), size = num_ticks, prob = 1/(12-time_since_fed))),
           num_ticks = num_ticks - dropped) 
  donext = TRUE
  if(nrow(L_ticks)!=0 & sum(L_ticks$dropped)>0){ L_ticks2 = L_ticks %>%
    uncount(dropped) %>%
    mutate(dropped = -1,
           Infection_status = case_when(Infection_status == "v1" ~ transfer_outcomes_v1[rbinom(n = n(), size = 1, prob = deer_infect_tick_v1)+1],
                                        Infection_status == "ha" ~ transfer_outcomes_ha[rbinom(n = n(), size = 1, prob = mouse_infect_tick_ha)+1],
                                        TRUE ~ "None"),
           Agent_ID = (max(tick_agents$Agent_ID)+1):((max(tick_agents$Agent_ID))+nrow(.)),
           num_ticks = 1)
  
  L_ticks = L_ticks %>%
    mutate(dropped = 0)
  tick_agents <- tick_agents %>%
    mutate(dropped = ifelse(fed == 1 & Lifestage == "Adult",-1,
                            ifelse(fed == 1 & Lifestage == "Nymph",-1,dropped))) %>%
    filter(!c(Agent_ID %in% L_ticks$Agent_ID)) %>%
    bind_rows(.,L_ticks) %>%
    bind_rows(.,L_ticks2)
  
  dropped_IDs = tick_agents %>% filter(dropped==-1)
  dropped_IDs = dropped_IDs$Agent_ID
  
  deer_agents$tick_links = map(deer_agents$tick_links, ~ .x[!(.x %in% dropped_IDs)])
  mouse_agents$tick_links = map(mouse_agents$tick_links, ~ .x[!(.x %in% dropped_IDs)])
  
  dr <- tick_agents %>%
    mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
    #mutate(dropped = 1) %>% # get rid of this in one second
    filter(links>0&dropped==1&linked_type=="Deer") %>%
    select(-c(row,col,layer)) %>%
    left_join(.,deer_agents %>% select(Agent_ID,row,col,layer),
              join_by(links==Agent_ID)) %>%
    rbind(.,tick_agents %>%
            mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
            # mutate(dropped = 1) %>% # get rid of this in one second
            filter(links>0&dropped==1&linked_type=="Mouse") %>%
            select(-c(row,col,layer)) %>%
            left_join(.,mouse_agents %>% select(Agent_ID,row,col,layer),
                      join_by(links==Agent_ID)))
  tick_agents <- tick_agents %>% 
    filter(!c(Agent_ID%in%dr$Agent_ID)) %>%
    mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
    rbind(.,dr)%>%
    # mouse_agents[which(mouse_agents$Agent_ID==links),]$layer,layer))) #%>%
    mutate(links = ifelse(links>0&dropped==1,0,links))
  donext = FALSE
  }
  if(donext==TRUE){
    if(nrow(L_ticks)==0 | sum(L_ticks$dropped)==0){tick_agents <- tick_agents %>%
      mutate(dropped = ifelse(fed == 1 & Lifestage == "Adult",-1,
                              ifelse(fed == 1 & Lifestage == "Nymph",-1,dropped))) %>%
      filter(!c(Agent_ID %in% L_ticks$Agent_ID)) %>%
      bind_rows(.,L_ticks)
    
    dropped_IDs = tick_agents %>% filter(dropped==-1)
    dropped_IDs = dropped_IDs$Agent_ID
    
    deer_agents$tick_links = map(deer_agents$tick_links, ~ .x[!(.x %in% dropped_IDs)])
    mouse_agents$tick_links = map(mouse_agents$tick_links, ~ .x[!(.x %in% dropped_IDs)])
    
    # deer_agents <<- deer_agents %>%
    #   mutate(tick_links = ifelse(tick_links%in%dropped_IDs,0,tick_links))
    # mouse_agents <<- mouse_agents %>%
    #   mutate(tick_links = ifelse(tick_links%in%dropped_IDs,0,tick_links))
    
    dr <- tick_agents %>%
      mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
      #mutate(dropped = 1) %>% # get rid of this in one second
      filter(links>0&dropped==1&linked_type=="Deer") %>%
      select(-c(row,col,layer)) %>%
      left_join(.,deer_agents %>% select(Agent_ID,row,col,layer),
                join_by(links==Agent_ID)) %>%
      rbind(.,tick_agents %>%
              mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
              # mutate(dropped = 1) %>% # get rid of this in one second
              filter(links>0&dropped==1&linked_type=="Mouse") %>%
              select(-c(row,col,layer)) %>%
              left_join(.,mouse_agents %>% select(Agent_ID,row,col,layer),
                        join_by(links==Agent_ID)))
    
    tick_agents <- tick_agents %>% 
      filter(!c(Agent_ID%in%dr$Agent_ID)) %>%
      mutate(dropped = ifelse(dropped==-1,1,dropped)) %>%
      rbind(.,dr) %>% 
      # mouse_agents[which(mouse_agents$Agent_ID==links),]$layer,layer))) #%>%
      mutate(links = ifelse(links>0&dropped==1,0,links))
    }
  }
  
  if(day>=lay_egg){ 
    ##### 
    # Lay eggs
    #####
    tick_agents <- tick_agents %>%
      mutate(tick_age_wks = ifelse(Lifestage=="Adult" & # This is a dummy to separate new eggs from old
                                     sex == "female" & 
                                     mated == 1 & 
                                     dropped == 1 &
                                     season!="fall" &
                                     season!="winter" &
                                     day >= lay_egg,# find tick egg laying timing)
                                   -1,tick_age_wks),
             Lifestage = ifelse(Lifestage=="Adult" & 
                                  sex == "female" & 
                                  season != "fall" &
                                  season!="winter" &
                                  mated == 1 & 
                                  dropped == 1 &
                                  day >= lay_egg,# find tick egg laying timing
                                "Eggs",Lifestage))
    
    num_new_eggs = length(which(tick_agents$tick_age_wks==-1))
    
    tick_agents <- tick_agents %>%
      mutate(Infection_status = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                       "None",Infection_status),
             num_ticks = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                eggs_per_female,num_ticks),#Double check the 1000 eggs estimate
             transfer_type = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                    "None",transfer_type),
             attempted_pathogen_transfer = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                                  0,attempted_pathogen_transfer),
             time_since_fed = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                     0,time_since_fed),
             dropped = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                              0,dropped),
             linked_type = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                  "None",linked_type),
             time_since_mating = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                        0,time_since_mating),
             links = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                            0,links),
             sex = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                          "none",sex),
             mated = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                            0,mated),
             Agent_ID = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                               (max(.$Agent_ID)+1):(max(.$Agent_ID)+num_new_eggs),Agent_ID)) %>%
      mutate(tick_age_wks = ifelse(Lifestage=="Eggs" & tick_age_wks == -1,
                                   0,tick_age_wks))
    
    #####  
    # Tick molting
    #####
    tick_agents <- tick_agents %>%
      mutate(molt = case_when(Lifestage == "Eggs" & 
                                season != "fall" &
                                season != "winter" &
                                day > egg_to_larvae ~ 1, #find age to molt, dummy age in for now
                              Lifestage == "Larvae" & 
                                dropped == 1 & 
                                day >= larvae_to_nymph_min &
                                day <= larvae_to_nymph_max ~ 1,# find age to molt, dummy age in for now
                              Lifestage == "Nymph" & 
                                dropped == 1 & 
                                season == "fall" ~ 1,
                              # day >= nymph_to_adult_min &
                              #day <= nymph_to_adult_max ~ 1,# find age to molt, dummy age in for now
                              TRUE ~ 0)) %>%
      mutate(die = case_when(molt==1 ~ case_when(Lifestage == "Nymph" & rbinom(n(),size = 1, prob = N_molt_success) == 0 ~ 1,
                                                 Lifestage == "Larvae" & rbinom(n(),size = 1, prob = L_molt_success) == 0 ~ 1,
                                                 TRUE ~ 0),
                             TRUE ~ 0)) %>%
      mutate(fed = ifelse(molt==1,0,fed),
             time_since_fed = ifelse(molt==1,0,fed),
             dropped = ifelse(molt==1,0,dropped),
             die = ifelse(molt==1,0,die),
             time_since_mating = ifelse(molt==1,0,time_since_mating),
             mated = ifelse(molt==1,0,mated),
             links = ifelse(molt==1,0,links),
             time_on_host = ifelse(molt==1,0,time_on_host),
             molt_death_immune = ifelse(molt==1,1,0),
             Lifestage = case_when(molt == 1 & Lifestage == "Eggs" ~ "Larvae",
                                   molt == 1 & Lifestage == "Larvae" ~ "Nymph",
                                   molt == 1 & Lifestage == "Nymph" ~ "Adult",
                                   TRUE ~ Lifestage),
             attempted_pathogen_transfer = ifelse(molt == 1,0,attempted_pathogen_transfer)) %>%
      mutate(sex2 = ifelse(sex=="none" & Lifestage == "Adult",
                           rbinom(n=n(),size=1,prob=.5),-1),
             sex = ifelse(sex2==1,"male",
                          ifelse(sex2==0,"female",sex))) %>%
      dplyr::select(-sex2) %>%
      mutate(molt = 0)
  }
  
  #####
  # Tick death
  #####
  tick_agents <- tick_agents %>%
    mutate(replete_death = case_when(
      Lifestage == "Larvae" & fed == 1 & dropped == 1 ~ rbinom(n(), size = 1, prob = L_rep_DR),
      Lifestage == "Nymph" & fed == 1 & dropped == 1 ~ rbinom(n(), size = 1, prob = N_rep_DR),
      Lifestage == "Adult" & fed == 1 & dropped == 1 ~ rbinom(n(), size = 1, prob = A_rep_DR),
      Lifestage == "Eggs" ~ rbinom(n(), size = num_ticks, prob = egg_mort_rate),
      TRUE ~ 0)) %>%
    mutate(un_replete_death = case_when(
      Lifestage == "Larvae" & fed == 0 & dropped == 0 & links==0 & (day/7) >= 40 ~ rbinom(n(), size = num_ticks, prob = L_unfed_DR_o40),
      Lifestage == "Larvae" & fed == 0 & dropped == 0 & links==0 & (day/7) < 40  ~ rbinom(n(), size = num_ticks, prob = L_unfed_DR_lt40),
      Lifestage == "Nymph" & fed == 0 & dropped == 0 & links==0 & (day/7) >= 40  ~ rbinom(n(), size = num_ticks, prob = N_unfed_DR_o40),
      Lifestage == "Nymph" & fed == 0 & dropped == 0 & links==0 & (day/7) < 40   ~ rbinom(n(), size = num_ticks, prob = N_unfed_DR_lt40),
      Lifestage == "Adult" & fed == 0 & dropped == 0 & links==0 & (day/7) >= 40  ~ rbinom(n(), size = num_ticks, prob = A_unfed_DR_o40),
      Lifestage == "Adult" & fed == 0 & dropped == 0 & links==0 &(day/7) < 40   ~ rbinom(n(), size = num_ticks, prob = A_unfed_DR_lt40),
      TRUE ~ 0)) %>%
    mutate(num_ticks = case_when(Lifestage=="Eggs" ~ num_ticks - replete_death,
                                 TRUE ~ num_ticks)) %>%
    mutate(die = ifelse(replete_death+un_replete_death>=1,1,0),
           die = case_when(Lifestage == "Eggs" & die == 1 ~ 0,
                           Lifestage == "Eggs" & num_ticks == 0 ~ 1,
                           Lifestage == "Larvae" & fed == 0 & dropped == 0 & season == "fall" ~ 1,
                           Lifestage == "Nymph" & fed == 0 & season == "fall" & molt_death_immune == 0 ~ 1,
                           Lifestage == "Adult" & mated == 0 & season == "summer" & molt_death_immune == 0 ~ 1,
                           Lifestage == "Adult" & mated == 1 & sex == "male" ~ 1, 
                           TRUE ~ die))
  
  die_list <- tick_agents %>% filter(die==1)
  die_list = die_list$Agent_ID
  
  deer_agents <- deer_agents %>% 
    mutate(tick_links = map(tick_links, ~ .x[!(.x %in% die_list)]))
  # mutate(tick_links = ifelse(tick_links %in% die_list,0,tick_links))
  
  mouse_agents <- mouse_agents %>%
    mutate(tick_links = map(tick_links, ~ .x[!(.x %in% die_list)]))
  #mutate(tick_links = ifelse(tick_links %in% die_list,0,tick_links))
  
  tick_agents <- tick_agents %>%
    mutate(die = ifelse(Lifestage=="Larvae" & un_replete_death > 0,0,die),
           num_ticks = ifelse(Lifestage=="Larvae" & un_replete_death > 0,
                              num_ticks-un_replete_death,num_ticks)) %>%
    filter(num_ticks >= 0,
           die == 0) %>%
    dplyr::select(-c(replete_death,un_replete_death))
  
  #####  
  # Host timer
  #####
  mouse_agents <- mouse_agents %>%
    mutate(Age = Age+1,
           Ha_infection_timer = case_when(Ha_infected == 1 & Ha_infection_timer==0 ~ 1,
                                          Ha_infected == 1 & Ha_infection_timer>0 ~ Ha_infection_timer + 1,
                                          TRUE ~ 0),
           Ha_infected = case_when(Ha_infection_timer >= (55*24) ~ 0,
                                   TRUE ~ Ha_infected))
  # Ha_infection_timer = ifelse(Ha_infected==1 & Ha_infection_timer==0,1,
  #                             ifelse(Ha_infected==1 & Ha_infection_timer>0,
  #                                    Ha_infection_timer + 1,0)),
  # V1_infection_timer = ifelse(V1_infected==1 & V1_infection_timer==0,1,
  #                             ifelse(V1_infected==1 & V1_infection_timer>0,
  #                                    V1_infection_timer+1,0)),
  # groom_timer = case_when(groom_timer > 1 ~ 0,
  #                         is.na(tick_links) == F ~ groom_timer + mouse_GR,
  #                         is.na(tick_links) == T ~ 0,
  #                         TRUE ~ groom_timer))
  deer_agents <- deer_agents %>%
    mutate(Age = Age+1,
           V1_infection_timer = case_when(V1_infected==1 & V1_infection_timer==0 ~ 1,
                                          V1_infected==1 & V1_infection_timer>0 ~ V1_infection_timer+1,
                                          TRUE ~ 0),
           V1_infected = case_when(V1_infection_timer >= (28*24) ~ 0,
                                   TRUE ~ V1_infected))
  
  #####
  # "Kill" hosts
  #####
  deer_agents <- deer_agents %>%
    mutate(Kill = case_when(Age==(11*24*365) ~ 1,
                            TRUE ~ as.numeric(rbinom(n = n(),
                                                     size = 1,
                                                     prob = 1/(11*24*365))))) %>% # Maximum lifespan equal 11 years
    mutate(Age = ifelse(Kill==1,0,Age),
           # Ha_infected = ifelse(Kill==1,0,Ha_infected),
           V1_infected = ifelse(Kill==1,0,V1_infected),
           Kill = 0)
  
  mouse_agents <- mouse_agents %>%
    mutate(Kill = case_when(Age==(2*24*365)~1,
                            TRUE ~ as.numeric(rbinom(n = n(),
                                                     size=1,
                                                     prob = 1/(2*24*365))))) %>% # Maximum lifespan equal 2 years
    mutate(Age = ifelse(Kill==1,0,Age),
           Ha_infected = ifelse(Kill==1,0,Ha_infected),
           # V1_infected = ifelse(Kill==1,0,V1_infected),
           Kill = 0)
  
  #####
  # Compile results
  #####
  deer_data <- deer_agents %>%
    group_by(network_ID,layer) %>%
    summarise(tot_v1_infected = sum(V1_infected,na.rm=T),
              tot_deer = n(),
              V1_perc = sum(V1_infected,na.rm=T)/n()) %>%
    mutate(day_of_year = day,
           season = season,
           timestep = i,
           year = year,
           day_of_year = day,
           network = net_select,
           Agent = "Deer")
  mouse_data<- mouse_agents %>%
    group_by(network_ID,layer) %>%
    summarise(tot_ha_infected = sum(Ha_infected,na.rm=T),
              tot_mice = n(),
              ha_perc = sum(Ha_infected)/n()) %>%
    mutate(day_of_year = day,
           season = season,
           timestep = i,
           year = year,
           day_of_year = day,
           network = net_select,
           Agent = "Mice")
  tick_data <- tick_agents %>%
    group_by(Lifestage,network_ID,layer) %>%
    summarise(ha_perc = (length(which(Infection_status == "ha"))/n())*100,
              v1_perc = (length(which(Infection_status == "v1"))/n())*100,
              tot_v1 = length(which(Infection_status == "v1")),
              tot_ha = length(which(Infection_status == "ha")),
              #total_attached = length(which(links>0)),
              #total_fed = sum(fed),
              # total_dropped = sum(dropped),
              #total_molted = sum(molt),
              #total_mated = sum(mated),
              total_ticks = sum(num_ticks),
              Agent = "Tick",
              year = year,
              timestep = i,
              day_of_year = day,
              network = net_select,
              season = season)
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  if(i==1){
    deer_data2[i:(i+(nrow(deer_data)-1)),] = deer_data
    mouse_data2[i:(i+(nrow(mouse_data)-1)),] = mouse_data
    tick_data2[i:(i+(nrow(tick_data)-1)),] = tick_data
  }
  if(i>1){
    deer_data2[ldj:(ldj+(nrow(deer_data)-1)),] = deer_data
    mouse_data2[lmj:(lmj+(nrow(mouse_data)-1)),] = mouse_data
    tick_data2[ltj:(ltj+(nrow(tick_data)-1)),] = tick_data
  }
  options(warn = oldw)
  if(i==1){
    ltj = nrow(tick_data)+1
    ldj = nrow(deer_data)+1
    lmj = nrow(mouse_data)+1
  }
  if(i>1){
    ltj = ltj+nrow(tick_data)
    ldj = ldj+nrow(deer_data)
    lmj = lmj+nrow(mouse_data)
  }
  # }
  #  if(i==1){#24){
  #    deer_data2 <- deer_data
  #    mouse_data2 <- mouse_data
  #    tick_data2 <- tick_data
  #  }
  #  if(i>1){#24){
  #    deer_data2 <- rbind(deer_data,deer_data2)
  #    mouse_data2 <- rbind(mouse_data,mouse_data2)
  #    tick_data2 <- rbind(tick_data,tick_data2)
  #  }
  
  if(i%%100==0){print(paste0("timestep ", i, ", day ",day,", year ", year," in network ",net_select))
    #save.image(file = paste0(getwd(),"/Debugging/net_6_timestep_",i,".RData"))
  }
  if(i%%5000==0){
    save.image(file = paste0(getwd(),"/Debugging/Network_",net_select,
                             "/net_",net_select,"_timestep_",i,".RData"))
    # write.csv(unnest_wider(deer_agents,tick_links,names_sep="_"),paste0(getwd(),"/Debugging/Network_",net_select,"/deer_debug_df_",
    #                                                      i,"_.csv"))
    # write.csv(unnest_wider(mouse_agents,tick_links,names_sep="_"),paste0(getwd(),"/Debugging/Network_",net_select,"/mouse_debug_df_",
    #                                                       i,"_.csv"))
    # write.csv(tick_agents,paste0(getwd(),"/Debugging/Network_",net_select,"/tick_debug_df_",
    #                              i,"_.csv"))
    # write.csv(deer_data2,paste0(getwd(),"/Debugging/Network_",net_select,"/Deer_results_debug_",
    #                             net_select,"_",Sys.Date(),"_",substring(Sys.time(),12,16),
    #                             "_.csv"))
    # write.csv(mouse_data2,paste0(getwd(),"/Debugging/Network_",net_select,"/Mouse_results_debug_",
    #                             net_select,"_",Sys.Date(),"_",substring(Sys.time(),12,16),
    #                             "_.csv"))
    # write.csv(tick_data2,paste0(getwd(),"/Debugging/Network_",net_select,"/Tick_results_debug_",
    #                             net_select,"_",Sys.Date(),"_",substring(Sys.time(),12,16),
    #                             "_.csv"))
  }
}
# end_time = Sys.time()
# end_time - start_time

# Save results:
if(i==(8760*5)){
  if(deer_infect_tick_v1<.1){pathogen_label="apriori"}
  if(deer_infect_tick_v1>=.1){pathogen_label=deer_infect_tick_v1*100}
  save.image(file = paste0(getwd(),"/Simulations/Network_",net_select,"/BI_attach_",deer_attach_prob*100,
                           "_path_trans_",substring(pathogen_label,1,3),".RData"))}
if(i>(8760*5)){
  if(deer_infect_tick_v1<.1){pathogen_label="apriori"}
  if(deer_infect_tick_v1>=.1){pathogen_label=deer_infect_tick_v1*100}
  save.image(file = paste0(getwd(),"/Simulations/Network_",net_select,"/timestep_",i,"_attach_",deer_attach_prob*100,
                           "_path_trans_",substring(pathogen_label,1,3),".RData"))}