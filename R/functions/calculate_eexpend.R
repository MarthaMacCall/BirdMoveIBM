calculate_eexpend <- function(adults, timeStep, energy_nest, energy_rest, k,
                              g, air_dens, da, v, cdb, wingspan, x1, bmr,
                              conv_eff, r){
  
  # Adults at nest
  adults[behav == "nest", eexpend := eexpend + (energy_nest * weight * timeStep)]
  
  # Adults resting
  adults[behav == "resting", `:=`(
    eexpend = eexpend + (energy_rest * weight * timeStep),
    eexpend_trip = eexpend_trip + (energy_rest * weight * timeStep)
  )]
  
  # Adults flying or foraging
  flying_ids <- adults$behav %in% c("flying", "foraging")
  
  if(any(flying_ids)){
    # Extract vectorized variables
    wt <- adults$weight[flying_ids]
    sc <- adults$stomach_content[flying_ids]
    
    # All-up mass (kg): body mass + stomach content
    all_mass <- (wt + sc) / 1000  # in kg
    
    # Induced power Pind (J/s)
    Pind <- (k * all_mass^2 * g^2) / (2 * air_dens * da * v)
    
    # Parasite power Ppar (J/s)
    bfa <- 0.00813 * (all_mass ^ 0.666) # body frontal area (m^2)
    fpa <- bfa * cdb # equivalent flat-plate area (m^2)
    Ppar <- (air_dens * fpa * v^3) / 2
    
    # Profile power Ppro (J/s)
    vmp <- (0.807 * k ^ 0.25 * all_mass ^ 0.5 * g ^ 0.5) /
      (air_dens ^ 0.5 * wingspan ^ 0.5 * bfa ^ 0.25 * cdb ^ 0.25) # minimum power speed (m/s): the speed at which  the mechanical power required to maintain level flight is minimum
    Pind_vmp <- (k * all_mass^2 * g^2) / (2 * air_dens * da * vmp) # induced power at minimum power speed
    Ppar_vmp <- (air_dens * fpa * vmp^3) / 2 # parasite power at minimum power speed
    Ppro <- (Pind_vmp + Ppar_vmp) * x1
    
    # Mechanical power Pmech (J/s): mechanical power required from the muscles to fly
    Pmech <- Pind + Ppar + Ppro
    
    # Chemical power Pchem (J/s): rate of consumption of fuel energy
    Pbmr <- bmr * wt # basal metabolic rate of each bird (J/s) depending on their body mass
    Pmet <- Pbmr * conv_eff # metabolic power (J/s): mechanical equivalent of the basal metabolic rate
    Pchem <- r * (Pmech + Pmet) / conv_eff # chemical power in J/s
    Pchem_min <- Pchem * 60 / 1000  # chemical power in kJ/min
    
    # Energy expenditure during time step
    eexpend_new <- Pchem_min * timeStep
    
    adults[flying_ids, `:=`(
      eexpend = eexpend + eexpend_new,
      eexpend_trip = eexpend_trip + eexpend_new
    )]
  }
}
