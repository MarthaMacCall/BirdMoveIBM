explore_foraging_area <- function(group, last_visited_cells, world, dim.x){
  
  coords_prev <- xyFromCell(world, group[,cell])
  
  n_adults <- nrow(group)
  offsets <- c(-dim.x-1, -dim.x, -dim.x+1, -1, 1, dim.x-1, dim.x, dim.x+1)
  n_neighbors <- length(offsets)
  
  # Repeat adult IDs and current cells to compute neighbors
  adultIDs_rep <- rep(group[,adultID], each = n_neighbors)
  current_cells_rep <- rep(group[, cell], each = n_neighbors)
  neighbors <- current_cells_rep + rep(offsets, times = n_adults)
  
  # Filter neighbors outside the grid
  valid <- neighbors >= 1 & neighbors <= max(world$cellID[])
  neighbors <- neighbors[valid]
  adultIDs_rep <- adultIDs_rep[valid]
  
  # Probability and cellUse_num
  probUse <- world$probUse[neighbors]
  cellUse_num <- world$cellUse_num[neighbors]
  
  # Remove zero-probability cells
  keep <- probUse > 0
  neighbors <- neighbors[keep]
  adultIDs_rep <- adultIDs_rep[keep]
  probUse <- probUse[keep]
  cellUse_num <- cellUse_num[keep]
  
  # Remove cells in OWF for adults avoiding them
  avoids <- setNames(group[, avoids_owf], group[, adultID])
  keep <- !(cellUse_num == 3 & avoids[as.character(adultIDs_rep)] == 1)
  neighbors <- neighbors[keep]
  adultIDs_rep <- adultIDs_rep[keep]
  probUse <- probUse[keep]
  
  # Create data.table of neighbors
  dt <- data.table(adultID = adultIDs_rep, neighbor = neighbors, probUse = probUse)
  
  # Fix inLastFive using join with which=TRUE
  dt[, inLastFive := FALSE]  # initialize
  dt[last_visited_cells, on = .(adultID, neighbor = cell), inLastFive := TRUE]
  
  # Keep only neighbors not in last 5 (unless all are in last 5)
  dt <- dt[, {
    if(all(inLastFive)) .SD else .SD[!inLastFive]
  }, by = adultID]
  
  # Vectorized weighted sampling
  dt[, cumProb := cumsum(probUse)/sum(probUse), by = adultID]
  rand <- runif(n_adults)
  sampled_cells <- dt[, .(neighbor = neighbor[findInterval(rand[.GRP], cumProb)+1]), by = adultID]
  
  # Update group cells
  setkey(group, adultID)
  group[sampled_cells, cell := i.neighbor]
  
  if(runTests){
    coords_new  <- xyFromCell(world, group$cell)
    dist <- sqrt((coords_new[,1] - coords_prev[,1])^2 +
                   (coords_new[,2] - coords_prev[,2])^2)
    offenders <- group$adultID[dist > sqrt(2)]
    if (length(offenders) > 0) {
      warning(paste("Abnormal movements detected:",
                    paste(offenders, collapse = ", ")))
    }
  }
}
