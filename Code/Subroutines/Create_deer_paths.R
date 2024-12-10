bresenham_line <- function(x1, y1, x2, y2) {
  # Initialize variables
  cells <- list()
  dx <- abs(x2 - x1)
  dy <- abs(y2 - y1)
  sx <- ifelse(x1 < x2, 1, -1)
  sy <- ifelse(y1 < y2, 1, -1)
  err <- dx - dy
  
  # Generate cells
  while (TRUE) {
    cells <- append(cells, list(c(x1, y1)))  # Add current cell
    if (x1 == x2 && y1 == y2) break          # End condition
    e2 <- 2 * err
    if (e2 > -dy) {
      err <- err - dy
      x1 <- x1 + sx
    }
    if (e2 < dx) {
      err <- err + dx
      y1 <- y1 + sy
    }
  }
  do.call(rbind, cells)[, c(2, 1)]
}
create_deer_paths = function(deer_agents){
  deer_paths <<- deer_agents %>%
    filter(jump_patch==0) %>%
    select(Agent_ID,network_ID,old_row,old_col,new_row,new_col) %>%
    rowwise() %>%
    mutate(cells = list(bresenham_line(old_row, old_col, new_row, new_col))) %>%
    ungroup() %>%
    unnest(cells) %>%
    mutate(row = cells[,1],
           col = cells[,2]) %>%
    select(Agent_ID,network_ID,row,col) %>%
    distinct() %>%
    group_by(network_ID,row,col) %>%
    mutate(prob = runif(min=0,max=1,n=1)) %>%
    arrange(prob)
}
