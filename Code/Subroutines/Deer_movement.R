# Coding deer movement:

deer_movement = function(deer_agents,daytime,network){if(daytime=="day"){
  deer_agents <<- deer_agents %>%
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
                                 max = subset(reduced_patches %>% st_drop_geometry(),
                                              layer==layer)$gridrows)),
                            new_row),
           new_col = ifelse(new_patch!=layer,round(runif(n=1,
                                 min = 0,
                                 max = subset(reduced_patches %>% st_drop_geometry(),
                                              layer==layer)$gridcols)),
                            new_col)) %>%
    mutate(old_row = row,
           old_col = col,
           row = new_row,
           col = new_col,
           layer = new_patch,
           locs = paste0(row,",",col,",",network_ID)) %>%
    select(-c(jump_patch))
    
}
}
  