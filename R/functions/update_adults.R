update_adults <- function(adults, daily_adult_data, dead_adults,
                          assim, fish_energy_density, energy_tissue,
                          lethal_weight){
  
  # Calculate energy gained during that day, based on the amount of food digested
  adults[, egain := assim * fish_energy_density * digested_food]
  
  # Update weight based on energy gains and expenditure
  adults[, weight := weight + ((egain - eexpend) / energy_tissue)]
  
  # Check alive/dead status based on the lethal weight threshold
  adults[weight < lethal_weight, alive_dead := "dead_starvation"]
  
  # Record adult parameters at the end of that day
  daily_adult_data <- rbindlist(list(
    daily_adult_data,
    adults[, .(day = day, adultID, total_catch, egain, eexpend, weight, alive_dead)]
  ), use.names = TRUE)
  
  if (any(adults$alive_dead == "dead_starvation")) {
    dead_adults <- rbindlist(
      list(dead_adults, adults[alive_dead == "dead_starvation"]),
      use.names = TRUE)
    adults <- adults[alive_dead == "alive"]
  }
  
  if(runTests){
    testthat::expect_equal(nrow(adults) + nrow(dead_adults), numPairs*2)
  }
  
  # Reset some parameters
  adults[, `:=`(
    egain = 0,
    eexpend = 0,
    total_catch = 0,
    digested_food = 0
  )]
  
  return(list(adults, daily_adult_data, dead_adults))
}