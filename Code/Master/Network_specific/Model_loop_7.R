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
for(i in 1:go_timesteps){
  # Update environment
  
  
  update_enviro(i,daylight)
  
  # Add nymphs in during summer of Year 1
  if(year==1&day==171 & exists("nymph_agents")){
    tick_agents = rbind(tick_agents,nymph_agents %>%
                          mutate(att_prob = NA))
    remove(nymph_agents)
  }
  
  # Move mice
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
  
  # Move deer
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
  if(day>=lay_egg){  
    # Create deer paths
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
                                           sex == "female" &
                                           Lifestage != "Eggs" &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Deer" &
                                           Infection_status == "v1" &
                                           deer_agents[match(links,deer_agents$Agent_ID),]$V1_infected==0 ~ "t2dv1",
                                         links>0 & 
                                           sex == "female" &
                                           Lifestage != "Eggs" &
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
                                           sex == "female" &
                                           Lifestage != "Eggs" &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Mouse" &
                                           Infection_status == "ha" &
                                           mouse_agents[match(links,mouse_agents$Agent_ID),]$Ha_infected==0 ~ "t2mha",
                                         links>0 & 
                                           sex == "female" &
                                           Lifestage != "Eggs" &
                                           attempted_pathogen_transfer == 0 &
                                           linked_type == "Mouse" &
                                           Infection_status == "None" &
                                           mouse_agents[match(links,mouse_agents$Agent_ID),]$Ha_infected==1 ~ "m2tha",
                                         TRUE ~ "None")) %>%
        mutate(Infection_status = case_when(transfer_type == "d2tv1" ~ transfer_outcomes_v1[rbinom(n = n(), size = 1, prob = deer_infect_tick_v1)+1],
                                            transfer_type == "m2tha" ~ transfer_outcomes_ha[rbinom(n = n(), size = 1, prob = mouse_infect_tick_ha)+1],
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
  tick_timer(tick_agents = tick_agents)
  
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
    lay_eggs(tick_agents = tick_agents)
    
    #####  
    # Tick molting
    #####
    tick_molting(tick_agents = tick_agents)
  }
  
  #####
  # Tick death
  #####
  tick_death(tick_agents = tick_agents)
  
  #####  
  # Host timer
  #####
  host_timer_fn(deer_agents = deer_agents,
                mouse_agents = mouse_agents)
  
  #####
  # "Kill" hosts
  #####
  kill_hosts_fn(deer_agents = deer_agents,
                mouse_agents = mouse_agents)
  
  #####
  # Compile results
  #####
  track_data(i = i,
             tick_agents = tick_agents,
             deer_agents = deer_agents,
             mouse_agents = mouse_agents)
  
  if(i%%100==0){print(paste0("timestep ", i, ", day ",day,", year ", year," in network ",net_select))
    # save.image(file = paste0(getwd(),"/Debugging/net_6_timestep_",i,".RData"))
  }
}
# end_time = Sys.time()
# end_time - start_time






