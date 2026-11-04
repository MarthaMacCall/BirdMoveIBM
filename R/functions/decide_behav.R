decide_behav <- function(adults, current_paths, day_night, day, minute,
                         max_time_at_nest, world, return_threshold,
                         coloCell, max_fails, rest_threshold,
                         stop_reduc_factor, daytime_length) {
  
  if(day_night == "day") {
    
    if(day >= 2 & minute == 1){
      adults[, behav := behav_before_night]
    }
    
    adultsAtNest <- adults[behav == "nest"]
    adultsFlying <- adults[behav == "flying"]
    adultsForaging <- adults[behav == "foraging"]
    adultsResting <- adults[behav == "resting"]
    
    if(runTests){
      testthat::expect_equal(nrow(adultsAtNest) + nrow(adultsFlying) + nrow(adultsForaging) + nrow(adultsResting), nrow(adults))
    }
    
    # Adults at nest ------------------------------------------------------------------------------------------------------
    
    if(nrow(adultsAtNest) > 0){
      
      # Number of adults at nest and total time at nest by pair
      counts <- adultsAtNest[, .(
        N = .N,
        sum_time = sum(time_at_nest),
        partner_dead = fifelse(pair %in% dead_adults$pair, "yes", "no")
      ), by = pair]
      
      # Classify pairs
      pairs_takingTurns <- counts[N == 2, pair]
      pairs_alone <- counts[N == 1 & partner_dead == "no", pair]
      pairs_partner_dead <- counts[N == 1 & partner_dead == "yes", pair]
      
      if(runTests){
        testthat::expect_equal(length(pairs_alone) + length(pairs_takingTurns) + length(pairs_partner_dead), length(unique(adultsAtNest$pair)))
      }
      
      # --- Taking turns ---
      if (length(pairs_takingTurns) > 0) {
        
        takingTurns <- adultsAtNest[pair %in% pairs_takingTurns]
        leaving_idx <- takingTurns[, .I[sample(which(time_at_nest == max(time_at_nest)), 1)], by = pair]$V1
        leaving <- takingTurns[leaving_idx]
        
        leaving[, `:=`(
          behav = "flying",
          direction = "outbound",
          reached_target = "no",
          full = "no",
          rested = "no",
          gave_up_trip = "no",
          time_at_nest = 0,
          tripID = tripID + 1
        )]
        
        leaving <- choose_foraging_site(leaving, foragingSites_cdf)
        current_paths <- find_shortest_path(current_paths, leaving, world, tr_avoidYes, tr_avoidNo)
        adultsAtNest[leaving, on = "adultID", names(leaving) := leaving]
      }
      
      # --- Alone ---
      if(length(pairs_alone) > 0){
        
        alone <- adultsAtNest[pair %in% pairs_alone]
        leaving_idx <- which(alone$time_at_nest >= max_time_at_nest)
        
        if(length(leaving_idx) > 0){
          
          leaving <- alone[leaving_idx]
          
          leaving[, `:=`(
            behav = "flying",
            direction = "outbound",
            reached_target = "no",
            full = "no", 
            rested = "no",
            gave_up_trip = "no",
            time_at_nest = 0,
            tripID = tripID + 1
          )]
          
          leaving <- choose_foraging_site(leaving, foragingSites_cdf)
          current_paths <- find_shortest_path(current_paths, leaving, world, tr_avoidYes, tr_avoidNo)
          adultsAtNest[leaving, on = "adultID", names(leaving) := leaving]
        }
      }
      
      # --- Partner dead ---
      if(length(pairs_partner_dead) > 0){
        
        partner_dead <- adultsAtNest[pair %in% pairs_partner_dead]
        leaving_idx <- which(partner_dead$time_at_nest >= 1440)
        
        if(length(leaving_idx) > 0){
          
          leaving <- partner_dead[leaving_idx]
          
          leaving[, `:=`(
            behav = "flying",
            direction = "outbound",
            reached_target = "no",
            full = "no", 
            rested = "no",
            gave_up_trip = "no",
            time_at_nest = 0,
            tripID = tripID + 1
          )]
          
          leaving <- choose_foraging_site(leaving, foragingSites_cdf)
          current_paths <- find_shortest_path(current_paths, leaving, world, tr_avoidYes, tr_avoidNo)
          adultsAtNest[leaving, on = "adultID", names(leaving) := leaving]
        }
      }
      
      adults[adultsAtNest, on = "adultID", names(adultsAtNest) := adultsAtNest]
    }
    
    # Adults flying --------------------------------------------------------------------------------------------------------
    
    if(nrow(adultsFlying) > 0){
      
      adultsFlying[, prob_foraging_here := world$probForaging[cell]]
      
      # At sea for >24h, still not full
      giveUp <- adultsFlying[direction == "outbound" &
                               time_at_sea >= return_threshold &
                               full == "no"]
      # Flying towards foraging site, not full yet, <24h
      outbound <- adultsFlying[direction == "outbound" &
                                 reached_target == "no" &
                                 full == "no" &
                                 time_at_sea < return_threshold]
      # At foraging site, not full yet, <24h
      atSite <- adultsFlying[direction == "outbound" &
                               reached_target == "yes" &
                               full == "no" &
                               time_at_sea < return_threshold]
      # Inbound, full and rested
      inboundFull <- adultsFlying[direction == "inbound" &
                                    reached_target == "no" &
                                    full == "yes" &
                                    rested == "yes"]
      # Inbound after giving up (not full and rested)
      inboundNotFull <- adultsFlying[direction == "inbound" &
                                       reached_target == "no" &
                                       gave_up_trip == "yes" &
                                       full == "no" &
                                       adultsFlying$rested == "no"]
      # Arrived at nest
      arrivedAtNest <- adultsFlying[direction == "inbound" &
                                      reached_target == "yes"]
      
      if(runTests){
        testthat::expect_equal(nrow(giveUp) + nrow(outbound) + nrow(atSite) + nrow(inboundFull) + nrow(inboundNotFull) + nrow(arrivedAtNest),
                               nrow(adultsFlying))
      }
      
      if(nrow(giveUp) > 0){
        giveUp[, `:=`(
          gave_up_trip = "yes",
          behav = "flying",
          direction = "inbound",
          destination = coloCell,
          reached_target = "no"
        )]
        current_paths <- find_shortest_path(current_paths, giveUp, world, tr_avoidYes, tr_avoidNo)
        adultsFlying[giveUp, on = "adultID", names(giveUp) := giveUp]
      }
      
      if(nrow(outbound) > 0){
        probs <- stop_reduc_factor * outbound[, prob_foraging_here]
        rand <- runif(nrow(outbound))
        outbound[, behav := fifelse(rand < probs, "foraging", "flying")]
        adultsFlying[outbound, on = "adultID", names(outbound) := outbound]
      }
      
      if(nrow(atSite) > 0){
        probs <- atSite[, prob_foraging_here]
        rand <- runif(nrow(atSite))
        atSite[, behav := fifelse(rand < probs, "foraging", "flying")]
        adultsFlying[atSite, on = "adultID", names(atSite) := atSite]
      }
      
      if(nrow(inboundFull) > 0){
        probs <- stop_reduc_factor * inboundFull[, prob_foraging_here]
        rand <- runif(nrow(inboundFull))
        inboundFull[, behav := fifelse(rand < probs, "foraging", "flying")]
        adultsFlying[inboundFull, on = "adultID", names(inboundFull) := inboundFull]
      }
      
      if(nrow(inboundNotFull) > 0){
        probs <- inboundNotFull[, prob_foraging_here]
        rand <- runif(nrow(inboundNotFull))
        inboundNotFull[, behav := fifelse(rand < probs, "foraging", "flying")]
        adultsFlying[inboundNotFull, on = "adultID", names(inboundNotFull) := inboundNotFull]
      }
      
      if(nrow(arrivedAtNest) > 0){
        arrivedAtNest[, behav := "nest"]
        adultsFlying[arrivedAtNest, on = "adultID", names(arrivedAtNest) := arrivedAtNest]
      }
      
      adults[adultsFlying, on = "adultID", names(adultsFlying) := adultsFlying]
    }
    
    # Adults foraging -----------------------------------------------------------------------------------------------------
    
    if(nrow(adultsForaging) > 0){
      
      # At sea for too long and still not full
      giveUp <- adultsForaging[direction == "outbound" &
                                 time_at_sea >= return_threshold &
                                 full == "no"]
      # Full and not rested yet
      needToRest <- adultsForaging[full == "yes" &
                                     rested == "no"]
      # Flying towards foraging site, not full yet, <24 h
      outbound <- adultsForaging[direction == "outbound" &
                                   reached_target == "no" &
                                   full == "no" &
                                   time_at_sea < return_threshold]
      # At foraging site, not full yet, < 24h, failed 5 times to catch a prey in current cell
      atSite_move <- adultsForaging[direction == "outbound" &
                                      reached_target == "yes" &
                                      full == "no" &
                                      time_at_sea < return_threshold &
                                      fishing_fails == max_fails]
      # At foraging site, not full yet, < 24h, failed <5 times to catch a prey in current cell --> continue foraging
      atSite_stay <- adultsForaging[direction == "outbound" &
                                      reached_target == "yes" &
                                      full == "no" &
                                      time_at_sea < return_threshold &
                                      fishing_fails < max_fails]
      # Flying towards the colony, full and rested, do not need to take a short rest
      inbound <- adultsForaging[direction == "inbound" &
                                  reached_target == "no" &
                                  full == "yes" &
                                  rested == "yes" &
                                  stomach_content < rest_threshold]
      # Flying towards the colony, full and rested, need to take a short rest
      inbound_rest <- adultsForaging[direction == "inbound" &
                                       reached_target == "no" &
                                       full == "yes" &
                                       rested == "yes" &
                                       stomach_content >= rest_threshold]
      # Flying towards the colony after giving up, not full yet, failed 5 times to catch a prey in current cell
      gaveUp_move <- adultsForaging[direction == "inbound" &
                                      reached_target == "no" &
                                      gave_up_trip == "yes" &
                                      full == "no" &
                                      fishing_fails == max_fails]
      # Flying towards the colony after giving up, not full yet, failed <5 times to catch a prey in current cell --> continue foraging
      gaveUp_stay <- adultsForaging[direction == "inbound" &
                                      reached_target == "no" &
                                      gave_up_trip == "yes" &
                                      full == "no" &
                                      fishing_fails < max_fails]
      
      if(runTests){
        testthat::expect_equal(nrow(giveUp) + nrow(needToRest) + nrow(outbound) + nrow(atSite_move) + nrow(atSite_stay) +
                                 nrow(inbound) + nrow(inbound_rest) + nrow(gaveUp_move) + nrow(gaveUp_stay), nrow(adultsForaging))
      }
      
      if(nrow(giveUp) > 0){
        giveUp[, `:=`(
          gave_up_trip = "yes",
          behav = "flying",
          direction = "inbound",
          destination = coloCell,
          reached_target = "no",
          fishing_fails = 0
        )]
        current_paths <- find_shortest_path(current_paths, giveUp, world, tr_avoidYes, tr_avoidNo)
        adultsForaging[giveUp, on = "adultID", names(giveUp) := giveUp]
      }
      
      if(nrow(needToRest) > 0){
        needToRest[, `:=`(
          behav = "resting",
          rest_type = "long",
          fishing_fails = 0
        )]
        adultsForaging[needToRest, on = "adultID", names(needToRest) := needToRest]
      }
      
      if(nrow(outbound) > 0){
        outbound[, `:=`(
          behav = "flying",
          fishing_fails = 0
        )]
        adultsForaging[outbound, on = "adultID", names(outbound) := outbound]
      }
      
      if(nrow(atSite_move) > 0){
        atSite_move[, `:=`(
          behav = "flying",
          fishing_fails = 0
        )]
        adultsForaging[atSite_move, on = "adultID", names(atSite_move) := atSite_move]
      }
      
      if(nrow(inbound) > 0){
        inbound[, `:=`(
          behav = "flying",
          fishing_fails = 0
        )]
        adultsForaging[inbound, on = "adultID", names(inbound) := inbound]
      }
      
      if(nrow(inbound_rest) > 0){
        inbound_rest[, `:=`(
          behav = "resting",
          rest_type = "short",
          fishing_fails = 0
        )]
        adultsForaging[inbound_rest, on = "adultID", names(inbound_rest) := inbound_rest]
      }
      
      if(nrow(gaveUp_move) > 0){
        gaveUp_move[, `:=`(
          behav = "flying",
          fishing_fails = 0
        )]
        adultsForaging[gaveUp_move, on = "adultID", names(gaveUp_move) := gaveUp_move]
      }
      
      adults[adultsForaging, on = "adultID", names(adultsForaging) := adultsForaging]
    }
    
    # Adults resting ------------------------------------------------------------------------------------------------------
    
    if(nrow(adultsResting) > 0){
      
      continueToRest <- adultsResting[stomach_content >= rest_threshold]
      stopLongRest <- adultsResting[rest_type == "long" &
                                      stomach_content < rest_threshold]
      stopShortRest <- adultsResting[rest_type == "short" & 
                                       stomach_content < rest_threshold]
      
      if(runTests){
        testthat::expect_equal(nrow(continueToRest) + nrow(stopLongRest) + nrow(stopShortRest), nrow(adultsResting))
      }
      
      if(nrow(stopLongRest) > 0){
        stopLongRest[,  `:=`(
          rested = "yes",
          behav = "flying",
          direction = "inbound",
          destination = coloCell,
          reached_target = "no",
          rest_type = "short"
        )]
        current_paths <- find_shortest_path(current_paths, stopLongRest, world, tr_avoidYes, tr_avoidNo)
        adultsResting[stopLongRest, on = "adultID", names(stopLongRest) := stopLongRest]
      }
      
      if(nrow(stopShortRest) > 0){
        stopShortRest[, behav := "flying"]
        adultsResting[stopShortRest, on = "adultID", names(stopShortRest) := stopShortRest]
      }
      
      adults[adultsResting, on = "adultID", names(adultsResting) := adultsResting]
    }
    
  } else if(day_night == "night") {
    
    if(minute == daytime_length + 1){
      adults[direction == "inbound" & reached_target == "yes", behav := "nest"]
      adults[, behav_before_night := behav]
      adults[behav != "nest", behav := "resting"]
      adults[, fishing_fails := 0]
    }
    
  }
  return(list(adults, current_paths))
}
