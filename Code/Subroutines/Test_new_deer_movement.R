deer_agents <- deer_agents %>%
  mutate(possibility_row_min = row-100,
         possibility_row_max = row+100,
         possibility_col_min = col-100,
         possiblitiy_col_max = col+100) %>%
  group_by(Agent_ID) %>%
  mutate(new_row = sample(possibility_row_min:possibility_row_max,size = 1),
         new_col = sample(possibility_col_min:possiblitiy_col_max,size = 1)) %>%
  mutate(jump_patch = ifelse(new_row<=0|new_col<=0,1,0))


deer_paths1 <- deer_agents %>%
  filter(lengths(tick_links)<100) %>% # This is new
  select(Agent_ID,network_ID,layer,old_row,old_col,new_row,new_col) %>%
  rowwise() %>%
  mutate(cells = list(bresenham_line(old_col, old_row, new_col, new_row))) %>%
  ungroup() %>%
  unnest(cells) %>%
  mutate(row = cells[,1],
         col = cells[,2]) %>%
  filter(row >= 1 & row <= gridrow & col >= 1 & col <= gridcol) %>%
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
  arrange(prob)

deer_agents <- deer_agents %>%
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
                                                       min = 0, # Should this be 1???
                                                       max = subset(reduced_patches,# %>% st_drop_geometry(),
                                                                    layer==layer)$gridrows)),
                          new_row),
         new_col = ifelse(new_patch!=layer,round(runif(n=1,
                                                       min = 0, # Should this be 1???
                                                       max = subset(reduced_patches,# %>% st_drop_geometry(),
                                                                    layer==layer)$gridcols)),
                          new_col)) %>%
  mutate(gridrows = ifelse(new_patch!=layer,new_patch$gridrows,gridrows),
         gridcols = ifelse(new_patch!=layer,new_patch$gridcols,gridcols)) %>%
  mutate(old_row = row,
         old_col = col,
         row = new_row,
         col = new_col,
         layer = new_patch,
         locs = paste0(row,",",col,",",network_ID))

deer_paths <- deer_agents %>%
  filter(jump_patch==1) %>%
  filter(lengths(tick_links)<100) %>% # This is new
  select(Agent_ID,network_ID,layer,old_row,old_col,new_row,new_col) %>%
  rowwise() %>%
  mutate(cells = list(bresenham_line(runif(n(),min = .5,max = gridcols+.5), round(runif(n(),min=0.5,max=gridrows+.5)), new_col, new_row))) %>%
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
  arrange(prob) %>%
  bind_rows(.,deer_paths1)
remove(deer_paths1)