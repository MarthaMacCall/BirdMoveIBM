###################
# IBM MAIN SCRIPT #
###################

######################
# 1/ Load R packages #
######################

{
  library(here)
  library(testthat)
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(gdistance)
  library(raster)
  library(data.table)
  library(future.apply)
  library(profvis)
  library(parallel)
  library(RColorBrewer)
}

#####################
# 2/ Load functions #
#####################

{
  source(here("R", "functions", "chick_needs.R"))
  source(here("R", "functions", "decide_behav.R"))
  source(here("R", "functions", "perform_behav.R"))
  source(here("R", "functions", "choose_foraging_site.R"))
  source(here("R", "functions", "find_shortest_path.R"))
  source(here("R", "functions", "follow_path.R"))
  source(here("R", "functions", "explore_foraging_area.R"))
  source(here("R", "functions", "chick_unattendance.R"))
  source(here("R", "functions", "collision_risk.R"))
  source(here("R", "functions", "digest.R"))
  source(here("R", "functions", "calculate_eexpend.R"))
  source(here("R", "functions", "update_adults.R"))
  source(here("R", "functions", "update_chicks.R"))
}

#################################
# 3/ Create the model landscape #
#################################

{
  cell_attributes <- read_csv2(here("data", "cell_attributes.csv")) %>%
    rename(xcor = centroid.x, ycor = centroid.y) %>%
    dplyr::mutate(xcor = as.integer(factor(xcor)),
                  ycor = as.integer(factor(ycor)),
                  probUse = probUse,# / 100,
                  probForaging = probForaging)# / 100)
  
  # Grid dimensions  
  dim.x <- length(unique(cell_attributes$xcor))
  dim.y <- length(unique(cell_attributes$ycor))
  
  # Create a raster for each cell attribute
  world_cellID <- rasterFromXYZ(cell_attributes[,c('xcor','ycor','cellID')])
  world_cellUse <- rasterFromXYZ(cell_attributes[,c('xcor','ycor','cellUse_num')])
  world_probUse <- rasterFromXYZ(cell_attributes[,c('xcor','ycor','probUse')])
  world_probForaging <- rasterFromXYZ(cell_attributes[,c('xcor','ycor','probForaging')])
  world_colRisk <- rasterFromXYZ(cell_attributes[,c('xcor','ycor','colRisk')])
  
  # Create a RasterStack object
  world <- stack(world_cellID, world_cellUse, world_probUse, world_probForaging, world_colRisk)
  rm(world_cellID, world_cellUse, world_probUse, world_probForaging, world_colRisk)
  
  # Quick plots to check that everything looks ok
  # plot(world$cellID, asp = 1)
  # plot(world$cellUse_num, asp = 1)
  # plot(world$probUse, asp = 1)
  # plot(world$probForaging, asp = 1)
  # plot(world$colRisk, asp = 1)
  
  # Get IDs of land cells
  landCells <- world$cellID[world$cellUse_num == 2]
  
  # Get IDs of OWF cells
  owfCells <- world$cellID[world$cellUse_num == 3]
  
  # Get the ID and coordinates of the colony cell
  coloCell <- world$cellID[world$cellUse_num == 1]
  coloCoords <- xyFromCell(world, coloCell)
  
  # Identify cells considered as foraging sites (> 20 km away from the colony & probForaging > 0)
  ## Calculate distance to colony of each cell (in km)
  cell_attributes$distToColo <- 5*sqrt((cell_attributes$xcor - coloCoords[,"x"])^2 +
                                         (cell_attributes$ycor - coloCoords[,"y"])^2)
  ## Create a dataframe to store cellID and probForaging of foraging sites 
  foragingSites <- cell_attributes[cell_attributes$distToColo >= 20 & cell_attributes$probForaging > 0,
                                   c("cellID", "probForaging")]
  ## Create a subset excluding cells inside OWFs (for birds that avoid OWFs)
  foragingSites_outOWF <- subset(foragingSites, !(cellID %in% owfCells))
  
  ## Quick plots to check the location of foraging sites including/excluding OWF cells
  # plot(world$cellUse_num)
  # points(xyFromCell(world, foragingSites$cellID), pch = "+")
  # plot(world$cellUse_num)
  # points(xyFromCell(world, foragingSites_outOWF$cellID), pch = "+")
  
  ## Cumulative distribution function
  compute_cdf <- function(foragingSites, foragingSites_outOWF) {
    list(
      # CDF for adults that do not avoid OWFs
      cdf0 = cumsum(foragingSites$probForaging) / sum(foragingSites$probForaging),
      cellID0 = foragingSites$cellID,
      # CDF for adults that avoid OWFs
      cdf1 = cumsum(foragingSites_outOWF$probForaging) / sum(foragingSites_outOWF$probForaging),
      cellID1 = foragingSites_outOWF$cellID
    )
  }
  foragingSites_cdf <- compute_cdf(foragingSites, foragingSites_outOWF)
  rm(foragingSites, foragingSites_outOWF)
  
  # Create transition layers for the find_shortest_path function
  
  ## Transition layer to calculate shortest paths avoiding land and OWF cells
  r_avoidYes <- cell_attributes[,c("xcor","ycor","cellUse")]
  r_avoidYes$cost <- ifelse(r_avoidYes$cellUse == "land" | r_avoidYes$cellUse == "owf", 1e10, 1)
  r_avoidYes <- rasterFromXYZ(r_avoidYes[,c("xcor", "ycor", "cost")])
  # plot(r_avoidYes)
  tr_avoidYes <- transition(r_avoidYes, transitionFunction = function(x) 1/mean(x), directions = 8)
  tr_avoidYes <- geoCorrection(tr_avoidYes, type = "c", multpl = FALSE)
  
  # Transition layer to calculate shortest paths avoiding land but not OWF cells
  r_avoidNo <- cell_attributes[,c("xcor","ycor","cellUse")]
  r_avoidNo$cost <- ifelse(r_avoidNo$cellUse == "land", 1e10, 1)
  r_avoidNo <- rasterFromXYZ(r_avoidNo[,c("xcor", "ycor", "cost")])
  # plot(r_avoidNo)
  tr_avoidNo <- transition(r_avoidNo, transitionFunction = function(x) 1/mean(x), directions = 8)
  tr_avoidNo <- geoCorrection(tr_avoidNo, type = "c", multpl = FALSE)
  
  rm(r_avoidNo, r_avoidYes, cell_attributes)
}  

###########################
# 4/ Set model parameters #
###########################

{
  # Simulation loop parameters
  iterations <- 50
  avoidance_rates <- c(0, 0.6, 0.7, 0.8, 0.9, 1)
  collision_options <- c("with_coll", "without_coll")
  
  # Global parameters
  runTests <- FALSE
  numDays <- 91
  timeStep <- 6
  daytime_length <- 1080
  
  # Parameters to initialize the population
  numPairs <- 1000
  initWeight_adults_mean <- 3101
  initWeight_adults_sd <- 197
  initWeight_chicks_mean <- 79.3
  initWeight_chicks_sd <- 11.2
  
  # Parameters required for the decide_behav() function
  max_time_at_nest <- 3600
  rest_threshold <- 750
  return_threshold <- 1440
  max_fails <- 5
  stop_reduc_factor <- 0.8
  
  # Parameters required for the perform_behav() function
  fishing_efficiency <- 0.35
  fish_mass_mean <- 100
  fish_mass_sd <- 10
  weight_limit <- 2100
  full_threshold <- 1000
  
  # Parameter required for the digest() function
  time_to_digest <- 600
  
  # Parameters required for the calculate_eexpend() function
  energy_nest <- 0.000582
  energy_rest <- 0.0007476
  wingspan <- 1.80
  da <- (pi * wingspan^2) / 4
  air_dens <- 1.223
  v <- 15.3
  k <- 1.2
  g <- 9.81
  cdb <- 0.1
  bmr <- 0.0027
  cpro <- 8.4
  aspect_ratio <- 14.1
  x1 <- cpro / aspect_ratio
  r <- 1.1
  conv_eff <- 0.23
  
  # Parameters required for the update_adults() and/or update_chicks() functions
  fish_energy_density <- 5.8
  assim <- 0.76
  energy_tissue <- 13
  prod_efficiency <- 0.75
  lethal_weight <- 1800
  
  # Create a data frame to store chick parameters that need to be updated daily as chicks grow (chick_food_max, eexpend_chicks, energy_tissue_chicks and chick_lethal_weight)
  {
    today <- 0:numDays
    nnn <- length(today)
    # Daily energy requirements (growth + maintenance)
    energy_req <- ((0.0005 * today^4) - (0.0873 * today^3) + (3.9246 * today^2) - (13.393 * today)) +
      ((-0.0001481 * today^4) + (0.02065 * today^3) - (0.6630 * today^2) + (19.73 * today) - 63.69)
    # Daily maximum food intake
    chick_food_max <- ifelse(
      today <= 5,
      2 * today,
      0.9 * (1/assim) * energy_req / fish_energy_density
    )
    # Daily maximum energy gains
    egain_max <- chick_food_max * assim * fish_energy_density
    # Daily energy expenditure (in kJ/day): formula derived from Montevecchi et al. (1984)
    eexpend_chicks <- ifelse(
      today <= 3,
      today,
      (19.73 * today) - (0.6630 * today^2) + (0.02065 * today^3) - (0.0001481 * today^4) - 63.69
    )
    # Daily tissue energy density (in kJ/g): formula derived from Montevecchi et al. 1984
    energy_tissue_chicks <- ifelse(
      today <= 56,
      3.35 + (0.16 * today),
      13
    )
    # Growth curve of a chick weighing 30 g at hatching (worst case scenario)
    # used to derive the daily lethal chick weight (60% of the worst case scenario weight)
    min_weight <- numeric(nnn)
    min_weight[1] <- 30
    for(i in 2:nnn){
      min_weight[i] <- min_weight[i-1] +
        ((egain_max[i] - eexpend_chicks[i]) *
           prod_efficiency / energy_tissue_chicks[i])
    }
    chick_lethal_weight <- 0.6 * min_weight #
    # Final data table
    chick_params <- data.table(
      today = today[-1],
      chick_food_max = chick_food_max[-1],
      eexpend_chicks = eexpend_chicks[-1],
      energy_tissue_chicks = energy_tissue_chicks[-1],
      chick_lethal_weight = chick_lethal_weight[-1]
    )
    rm(today, nnn, i, energy_req, chick_food_max, egain_max, eexpend_chicks, energy_tissue_chicks, min_weight, chick_lethal_weight) 
  }
}

###################
# Simulation loop #
###################

for(iter in 1:iterations){
  for(coll in collision_options){
    for(avoidance_rate in avoidance_rates){
      if(!(coll == "with_coll" & avoidance_rate == 1)){ #Exclude the scenario with 100% macro-avoidance and collision risk (no collisions can occur if all individuals avoid OWFs)
        
        # Create a folder to store model outputs
        output_folder <- here::here(paste(coll, "_and_", avoidance_rate*100, "_pct_macro_avoidance", sep = ""))
        dir.create(output_folder, showWarnings = TRUE, recursive = TRUE)
        
        # Initialize the adult population
        adults <- data.table(
          adultID = seq(1:(numPairs*2)),
          sex = rep(c("M","F"), numPairs),
          pair = rep(1:numPairs, each = 2),
          avoids_owf = rbinom(n = numPairs*2, size = 1, prob = avoidance_rate),
          alive_dead = "alive",
          weight = rnorm(numPairs*2, mean = initWeight_adults_mean, sd = initWeight_adults_sd),
          cell = coloCell,
          behav = "nest",
          behav_before_night = "",
          time_at_nest = rep(c(2,1), numPairs),
          tripID = 0,
          direction = "",
          destination = 0,
          reached_target = "",
          full = "no",
          rested = "no",
          gave_up_trip = "no",
          rest_type = "",
          time_resting = 0,
          time_flying = 0,
          time_foraging = 0,
          time_at_sea = 0,
          prob_foraging_here = 0,
          stomach_content = 0,
          fish_counter = 0,
          total_catch = 0,
          digested_food = 0,
          last_fish_time = 0,
          last_fish_mass = 0,
          fishing_fails = 0,
          food_given_to_chick = 0,
          egain = 0,
          eexpend = 0,
          eexpend_trip = 0
        )
          
        # Dead adults (none at the beginning)
        dead_adults <- data.table(
          adultID = integer(),
          sex = character(),
          pair = integer(),
          avoids_owf = integer(),
          alive_dead = character(),
          weight = double(),
          cell = integer(),
          behav = character(),
          behav_before_night = character(),
          time_at_nest = integer(),
          tripID = integer(),
          direction = character(),
          destination = integer(),
          reached_target = character(),
          full = character(),
          rested = character(),
          gave_up_trip = character(),
          rest_type = character(),
          time_resting = integer(),
          time_flying = integer(),
          time_foraging = integer(),
          time_at_sea = integer(),
          prob_foraging_here = double(),
          stomach_content = double(),
          fish_counter = integer(),
          total_catch = double(),
          digested_food = double(),
          last_fish_time = integer(),
          last_fish_mass = double(),
          fishing_fails = integer(),
          food_given_to_chick = double(),
          egain = double(),
          eexpend = double(),
          eexpend_trip = double()
        )
        
        # Initialize the chick population
        chicks <- data.table(
          chickID = seq(1:numPairs),
          pair = seq(1:numPairs),
          alive_dead = "alive",
          weight = rnorm(numPairs, mean = initWeight_chicks_mean, sd = initWeight_chicks_sd),
          dayfed = 0,
          chick_food_max = 0,
          chick_food = 0,
          egain = 0,
          unattended_time = 0
        )
          
        # Dead chicks (none at the beginning)
        dead_chicks <- data.table(
          chickID = integer(),
          pair = integer(),
          alive_dead = character(),
          weight = double(),
          dayfed = integer(),
          chick_food_max = double(),
          chick_food = double(),
          egain = double(),
          unattended_time = double()
        )
        
        # Table to store daily adult data
        daily_adult_data <- data.table(day = integer(),
                                       adultID = integer(),
                                       total_catch = double(),
                                       egain = double(),
                                       eexpend = double(),
                                       weight = double(),
                                       alive_dead = character())
        
        # Table to store daily chick data
        daily_chick_data <- data.table(day = integer(),
                                       chickID = integer(),
                                       chick_food = double(),
                                       weight = double(),
                                       alive_dead = character(),
                                       unattended_time = double())
        
        # Table to store trip metrics
        trip_data <- data.table(adultID = integer(),
                                tripID = integer(),
                                day = integer(),
                                gave_up_trip = character(),
                                time_flying = double(),
                                time_foraging = double(),
                                time_resting = double(),
                                time_at_sea = double(),
                                fish_counter = double(),
                                stomach_content = double(),
                                food_given_to_chick = double(),
                                eexpend_trip = double())
        
        # Table to store data on captured fish to be digested
        fish_to_digest <- data.table(adultID = integer(),
                                     mass = double(),
                                     time = integer())
        
        # List containing the paths currently followed by adults
        current_paths <- vector("list", length = numPairs*2)
        names(current_paths) <- adults$adultID
        
        # Table to store the IDs of the last five cells visited by each adult
        last_visited_cells <- data.table(adultID = adults$adultID,
                                         cell = coloCell)
      }
      # (Optional) Table to store some adult parameters at each time step
      # adult_behav <- data.table(day = integer(),
      #                           minute = integer(),
      #                           adultID = integer(),
      #                           sex = character(),
      #                           pair = integer(),
      #                           avoids_owf = integer(),
      #                           alive_dead = character(),
      #                           weight = double(),
      #                           cell = integer(),
      #                           behav = character(),
      #                           behav_before_night = character(),
      #                           time_at_nest = integer(),
      #                           tripID = integer(),
      #                           direction = character(),
      #                           destination = integer(),
      #                           reached_target = character(),
      #                           full = character(),
      #                           rested = character(),
      #                           gave_up_trip = character(),
      #                           rest_type = character(),
      #                           time_resting = integer(),
      #                           time_flying = integer(),
      #                           time_foraging = integer(),
      #                           time_at_sea = integer(),
      #                           prob_foraging_here = double(),
      #                           stomach_content = double(),
      #                           fish_counter = integer(),
      #                           total_catch = double(),
      #                           digested_food = double(),
      #                           last_fish_time = integer(),
      #                           last_fish_mass = double(),
      #                           fishing_fails = integer(),
      #                           food_given_to_chick = double(),
      #                           egain = double(),
      #                           eexpend = double(),
      #                           eexpend_trip = double())
      
      # Loop through each day of the chick-rearing period
      tot_minute <- 1
      
      for(day in 1:numDays){
        
        print(paste("Macro-avoidance rate:", avoidance_rate, "/", coll, "/ iteration:", iter, "/ day:", day, "/ nb adults:", nrow(adults), "/ nb chicks:", nrow(chicks)))
        
        for(minute in seq(1, 1435, by = timeStep)){ # until the last minute of the day
          
          # Set day/night
          day_night <- ifelse(minute < daytime_length + 1, "day", "night")
          
          # The first minute of each day, calculate the chicks' food requirements for the day
          if(minute == 1){ chick_needs(chicks, chick_params, day) }
          
          # At each time step:
          
          ##1/ Decide the behaviour of each adult
          resDecideBehav <- decide_behav(adults, current_paths, day_night, day, minute,
                                         max_time_at_nest, world, return_threshold,
                                         coloCell, max_fails, rest_threshold,
                                         stop_reduc_factor, daytime_length)
          adults <- resDecideBehav[[1]]
          current_paths <- resDecideBehav[[2]]
          
          ##2/ Each adult then performs its behaviour
          resPerformBehav <- perform_behav(adults, chicks, trip_data, current_paths, fish_to_digest,
                                           last_visited_cells, day, tot_minute, timeStep,
                                           fishing_efficiency, fish_mass_mean, fish_mass_sd,
                                           full_threshold, time_to_digest)
          adults <- resPerformBehav[[1]]
          chicks <- resPerformBehav[[2]]
          trip_data <- resPerformBehav[[3]]
          current_paths <- resPerformBehav[[4]]
          fish_to_digest <- resPerformBehav[[5]]
          last_visited_cells <- resPerformBehav[[6]]
          
          # (Optional) Record each adult's behaviour
          # adult_behav <- rbindlist(list(adult_behav, cbind(adults, day = day, minute = minute)), use.names = TRUE)
          
          ##3/ Determine whether adults flying or foraging within OWFs collide with a wind turbine
          if(coll == "with_coll"){
            resCollisionRisk <- collision_risk(adults, dead_adults, daily_adult_data, world)
            adults <- resCollisionRisk[[1]]
            dead_adults <- resCollisionRisk[[2]]
            daily_adult_data <- resCollisionRisk[[3]]
          }
          
          ##4/ Adults digest some of the fish they have previously ingested
          digest(adults, fish_to_digest, tot_minute, time_to_digest, timeStep)
          
          ##5/ Calculate the energy expenditure of adults during the time step according to their behaviour
          calculate_eexpend(adults, timeStep, energy_nest, energy_rest, k,
                            g, air_dens, da, v, cdb, wingspan, x1, bmr,
                            conv_eff, r)
          
          ##5/ Calculate chick unattended time
          chick_unattendance(adults, chicks, timeStep)
          
          # At the end of the day
          if(minute == 1435){
            
            ##7/ Update chicks
            resUpdateChicks <- update_chicks(chicks, daily_chick_data, dead_chicks,
                                             chick_params, day, assim, fish_energy_density,
                                             prod_efficiency, lethal_weight)
            chicks <- resUpdateChicks[[1]]
            daily_chick_data <- resUpdateChicks[[2]]
            dead_chicks <- resUpdateChicks[[3]]
            
            ##8/ Update adults
            resUpdateAdults <- update_adults(adults, daily_adult_data, dead_adults,
                                             assim, fish_energy_density, energy_tissue,
                                             lethal_weight)
            adults <- resUpdateAdults[[1]]
            daily_adult_data <- resUpdateAdults[[2]]
            dead_adults <- resUpdateAdults[[3]]
          }
          
          # Update tot_minute counter
          tot_minute <- tot_minute + timeStep
          
        } # End of the day
        
        # At the end of the last day
        if(day == numDays){
          # Export final outputs
          write_csv2(daily_adult_data, file.path(output_folder, paste0("daily_adult_data_", iter, ".csv")))
          write_csv2(daily_chick_data, file.path(output_folder, paste0("daily_chick_data_", iter, ".csv")))
          write_csv2(trip_data, file.path(output_folder, paste0("trip_data_", iter, ".csv")))
          write_csv2(dead_adults, file.path(output_folder, paste0("dead_adults_", iter, ".csv")))
          write_csv2(dead_chicks, file.path(output_folder, paste0("dead_chicks_", iter, ".csv")))
          
          # Clean global environment
          rm(daily_adult_data, adults, dead_adults, daily_chick_data, chicks, dead_chicks,
             current_paths, fish_to_digest, last_visited_cells, resDecideBehav,
             resPerformBehav, resUpdateAdults, resUpdateChicks, resCollisionRisk, trip_data)
          
          # Free unused R memory
          gc()
        }
        
      }
    }
  }
}
