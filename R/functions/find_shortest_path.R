find_shortest_path <- function(current_paths, group, world, tr_avoidYes, tr_avoidNo) {
  
  # Precompute coordinates for all individuals as lists
  group_coords <- group[, .(
    origin = list(xyFromCell(world, cell)),
    goal   = list(xyFromCell(world, destination))
  ), by = adultID]
  
  # Create named vectors for fast lookup
  origins <- setNames(group_coords$origin, group_coords$adultID)
  goals   <- setNames(group_coords$goal, group_coords$adultID)
  
  # Separate individuals based on OWF avoidance behavior
  avoidOWF <- group[avoids_owf == 1, adultID]
  doNotAvoidOWF <- group[avoids_owf == 0, adultID]
  
  # Set up parallelisation plan
  plan(multisession)
  
  # Function to compute shortest path for a single individual
  compute_path <- function(id, transition) {
    path <- gdistance::shortestPath(
      transition,
      origin = origins[[as.character(id)]],
      goal   = goals[[as.character(id)]],
      output = "SpatialLines"
    )
    # Extract cells from the path
    raster::cellFromXY(world, as.data.frame(raster::coordinates(path)))[-1]  # Remove departure cell
  }

 # Compute paths in parallel with reproducible random numbers
  paths1 <- lapply(avoidOWF, compute_path, transition = tr_avoidYes)
  names(paths1) <- avoidOWF
  paths2 <- lapply(doNotAvoidOWF, compute_path, transition = tr_avoidNo)
  names(paths2) <- doNotAvoidOWF

  # Merge results into current_paths
  current_paths[names(paths1)] <- paths1
  current_paths[names(paths2)] <- paths2
 
  return(current_paths)
}
