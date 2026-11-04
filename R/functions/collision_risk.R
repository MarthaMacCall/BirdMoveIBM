collision_risk <- function(adults, dead_adults, daily_adult_data, world){
  
  # Consider only adults that can collide
  exposed <- adults$behav %in% c("flying", "foraging")
  
  if(any(exposed)){
    
    # Vectorized collision probability
    colRisk <- world$colRisk[adults$cell[exposed]]
    collision <- rbinom(sum(exposed), 1, colRisk)
    
    # Mark dead adults
    dead_ids <- which(exposed)[collision == 1]
    
    if(length(dead_ids) > 0){
      
      adults$alive_dead[dead_ids] <- "dead_collision"
      
      daily_adult_data <- rbindlist(list(
        daily_adult_data,
        adults[dead_ids, .(day = day, adultID, total_catch, egain, eexpend, weight, alive_dead)]
      ), use.names = TRUE)
      
      # Move dead adults to dead_adults
      dead_adults <- rbindlist(list(dead_adults, adults[dead_ids]), use.names = TRUE)
      # Keep only alive adults
      adults <- adults[alive_dead == "alive"]
    }
  }
  return(list(adults, dead_adults, daily_adult_data))
}