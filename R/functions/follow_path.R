follow_path <- function(group, current_paths, world){
  
  ids <- group[, adultID]
  coords_prev <- xyFromCell(world, group$cell)
  
  # Update cell variable: take the first cell of each path
  group[, cell := vapply(current_paths[ids], `[`, numeric(1), 1)]
  
  if(runTests){
    coords_new  <- xyFromCell(world, group$cell)
    dist <- sqrt((coords_new[,1] - coords_prev[,1])^2 +
                   (coords_new[,2] - coords_prev[,2])^2)
    offenders <- group[dist > sqrt(2), adultID]
    if (length(offenders) > 0) {
      warning(paste("Abnormal movements detected:",
                    paste(offenders, collapse = ", ")))
    }
  }
  
  # Check whether they have arrived at their destination
  group[cell == destination, reached_target := "yes"]
  
  # Remove from current_paths the cells that the adults have just moved to
  current_paths[ids] <- lapply(current_paths[ids], `[`, -1)
  
  return(current_paths)
}