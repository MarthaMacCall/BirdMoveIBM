update_chicks <- function(chicks, daily_chick_data, dead_chicks,
                          chick_params, day, assim, fish_energy_density,
                          prod_efficiency, lethal_weight){
  
  #Today's parameters
  cp <- chick_params[today == day]
  
  # Calculate energy gained during the day (kJ) based on the amount of food received
  # On the first day, each chick receives the maximum amount of food by default
  if(day == 1){
    chicks[, egain := chick_food_max * assim * fish_energy_density]
  } else {
    chicks[, egain := chick_food * assim * fish_energy_density]
  }
  
  # Update chicks' weight based on energy gains and expenditure
  chicks[, weight := weight + ((egain - cp$eexpend_chicks) * prod_efficiency / cp$energy_tissue_chicks)]
  
  # Check alive/dead status based on a lethal weight threshold
  lethal_thr <- if (day < 91) cp$chick_lethal_weight else lethal_weight
  chicks[weight < lethal_thr, alive_dead := "dead"]
  
  daily_chick_data <- rbindlist(list(
    daily_chick_data,
    chicks[, .(day = day, chickID, chick_food, weight, alive_dead, unattended_time)]
  ), use.names = TRUE)
  
  if (any(chicks$alive_dead == "dead")) {
    dead_chicks <- rbindlist(list(dead_chicks, chicks[alive_dead == "dead"]), use.names = TRUE)
    chicks <- chicks[alive_dead == "alive"]
  }
  
  if(runTests){
    testthat::expect_equal(nrow(chicks) + nrow(dead_chicks), numPairs)
  }
  
  # Reset chick_food and unattended_time
  chicks[, `:=`(chick_food = 0,
                unattended_time = 0)]
  
  return(list(chicks, daily_chick_data, dead_chicks))
}