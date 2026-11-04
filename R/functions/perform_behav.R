perform_behav <- function(adults, chicks, trip_data, current_paths, fish_to_digest,
                          last_visited_cells, day, tot_minute, timeStep,
                          fishing_efficiency, fish_mass_mean, fish_mass_sd,
                          full_threshold, time_to_digest){
  
  adultsAtNest <- adults[behav == "nest"]
  adultsFlying <- adults[behav == "flying"]
  adultsForaging <- adults[behav == "foraging"]
  adultsResting <- adults[behav == "resting"]
  
  if(runTests){
    testthat::expect_equal(nrow(adultsAtNest) + nrow(adultsFlying) + nrow(adultsForaging) + nrow(adultsResting), nrow(adults))
  }
  
  if(nrow(adultsAtNest) > 0){
      
    # Chick feeding
    adultsFeeding <- adultsAtNest[time_at_nest == 0 &
                                    stomach_content > 0 &
                                    weight >= weight_limit]
    
    chicksToFeed <- chicks[chick_food < chick_food_max]
    
    adultsFeeding <- adultsFeeding[pair %in% chicksToFeed$pair]
    
    if(nrow(adultsFeeding) > 0){
      feeding <- chicksToFeed[adultsFeeding, on = "pair"]
      feeding[, diff := chick_food_max - chick_food]
      feeding[, chick_meal_size := pmin(diff, stomach_content)]
      
      # Update chicks
      chicks[feeding, on = "chickID", `:=`(
        chick_food = chick_food + i.chick_meal_size,
        dayfed = day
        )]
      
      # Update adults at nest
      adultsAtNest[feeding, on = "adultID", `:=`(
        stomach_content = stomach_content - i.chick_meal_size,
        food_given_to_chick = food_given_to_chick + i.chick_meal_size
      )]
    }
    
    # Update trip_data for individuals that just arrived to the nest
    trip_data <- rbindlist(
      list(trip_data, adultsAtNest[time_at_nest == 0,
                                   .(adultID, tripID, day, gave_up_trip, 
                                   time_flying, time_foraging, time_resting,
                                   time_at_sea, fish_counter, stomach_content,
                                   food_given_to_chick, eexpend_trip)]
           ), use.names = TRUE)
    
    # Reset counters for individuals that just arrived to the nest
    adultsAtNest[time_at_nest == 0, `:=`(
      time_flying = 0,
      time_foraging = 0,
      time_resting = 0,
      time_at_sea = 0,
      fish_counter = 0,
      food_given_to_chick = 0,
      eexpend_trip = 0
    )]
    
    # Update time at nest of all adults at nest
    adultsAtNest[, time_at_nest := time_at_nest + timeStep]
    
    adults[adultsAtNest, on = "adultID", names(adultsAtNest) := adultsAtNest]
  }
  
  if(nrow(adultsFlying) > 0){
    
    adultsFlying[, `:=`(time_flying = time_flying + timeStep,
                        time_at_sea = time_at_sea + timeStep)]
    
    commuting <- adultsFlying[reached_target == "no"]
    atForagingSite <- adultsFlying[direction == "outbound" & reached_target == "yes"]
    
    if(runTests){
      testthat::expect_equal(nrow(commuting) + nrow(atForagingSite), nrow(adultsFlying))
    }
    
    if(nrow(commuting) > 0){
      current_paths <- follow_path(commuting, current_paths, world)
      adultsFlying[commuting, on = "adultID", names(commuting) := commuting]
    }
    
    if(nrow(atForagingSite) > 0){
      explore_foraging_area(atForagingSite, last_visited_cells, world, dim.x)
      adultsFlying[atForagingSite, on = "adultID", names(atForagingSite) := atForagingSite]
    }
    
    adults[adultsFlying, on = "adultID", names(adultsFlying) := adultsFlying]
  }
  
  if(nrow(adultsForaging) > 0){
    
    adultsForaging[, `:=`(time_foraging = time_foraging + timeStep,
                          time_at_sea = time_at_sea + timeStep)]
    
    # Fishing
    fishing_attempts <- rbinom(nrow(adultsForaging), 1, fishing_efficiency)
    success_idx <- which(fishing_attempts == 1)
    fail_idx <- which(fishing_attempts == 0)
    
    if(length(success_idx) > 0){
      adultsForaging[success_idx,  `:=`(
        last_fish_mass = rnorm(.N, fish_mass_mean, fish_mass_sd),
        last_fish_time = tot_minute,
        fish_counter = fish_counter + 1
      )]
      adultsForaging[success_idx, `:=`(
        total_catch = total_catch + last_fish_mass,
        stomach_content = stomach_content + last_fish_mass
      )]
      new_fish <- adultsForaging[success_idx, .(adultID, mass = last_fish_mass, time = last_fish_time)]
      fish_to_digest <- rbindlist(list(fish_to_digest, new_fish), use.names = TRUE)
      fish_to_digest <- fish_to_digest[time >= (tot_minute - time_to_digest)]
    }
    
    if(length(fail_idx) > 0){
      adultsForaging[fail_idx, fishing_fails := fishing_fails + 1]
    }
    
    adultsForaging[stomach_content >= full_threshold, full := "yes"]
    
    adults[adultsForaging, on = "adultID", names(adultsForaging) := adultsForaging]
  }
  
  if(nrow(adultsResting) > 0){
    
    adultsResting[, `:=`(time_resting = time_resting + timeStep,
                         time_at_sea = time_at_sea + timeStep)]
    adults[adultsResting, on = "adultID", names(adultsResting) := adultsResting]
  
  }
  
  # Update last visited cells
  
  last_visited_cells <- rbindlist(list(last_visited_cells, adults[, .(adultID, cell)]), use.names = TRUE)
  last_visited_cells <- last_visited_cells[, tail(.SD, 5), by = adultID]
  
  return(list(adults, chicks, trip_data, current_paths, fish_to_digest, last_visited_cells))
}
