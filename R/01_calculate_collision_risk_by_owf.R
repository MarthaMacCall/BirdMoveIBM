# For each offshore wind farm (OWF) included in the IBM, this script calculates the collision risk for a single gannet spending one 6-minute timestep in a 25 km² grid cell within the OWF
# First, monthly collision risk estimates (June, July and August) are calculated for each OWF using the extended Band model (Band, 2012)
# This is performed using the band_crm() function from the stochLAB package (option 3)
# Monthly collision risk estimates are then divided by the number of daylight hours in the corresponding month and further divided by 10 to obtain collision risk estimates for a 6-minute timestep
# Finally, the estimates for June, July and August are averaged to obtain a mean collision risk estimate for the chick-rearing period

library(stochLAB)
library(dplyr)
library(readr)
library(tidyr)
library(here)

here::here()

# Bird parameters required for the band_crm() function (option 3)
flight_speed <- 13.9         # Flight speed in m/s
body_lt <- 0.94              # Body length in m
wing_span <- 1.73            # Wing span in m
flight_type <- "flapping"    # Flapping or gliding flight
avoid_rt_ext <- 0.97         # Within-windfarm avoidance rate (Cook et al. 2014)
noct_activity <- 0           # Proportion of daytime activity levels
prop_upwind <- 0.5           # Proportion of flights that are upwind

# Bird density by month (bird / km²)
dens_month <- data.frame(
  month = c("Jun", "Jul", "Aug"),   # Months corresponding to the chick-rearing period for Northern gannets
  dens = rep(0.04, 3)               # 1 bird / 25 km² grid cell --> 0.04 bird / km²
)

# Flight height distribution from Johnston et al. 2014
gen_fhd <- Johnston_Flight_heights_SOSS %>%
  filter(variable=="Gannet.est") %>%
  dplyr::select(height,prop)

# OWF parameters required for the band_crm() function (option 3)
owf_params <- read.csv(here("Data", "owf_parameters.csv"), sep = ";", dec = ",")

owf_params$blade_pitch <- owf_params$blade_pitch * pi/180

# Proportion of operational time by month
turb_oper_month <- pivot_longer(owf_params[,c(1,12:14)], cols = starts_with("oper")) %>%
  rename(month = name,
         prop_oper = value) %>%
  mutate(month = rep(c("Jun", "Jul", "Aug"), nrow(owf_params)),
         prop_oper = prop_oper/100)

# Collision risk by month for each OWF using the extended Band model
col_risks <- data.frame(
  owf = rep(owf_params$owf, each = 3),
  month = rep(c("Jun", "Jul", "Aug"), nrow(owf_params))
)

for(i in owf_params$owf){
  col_risks$col_risk_month[col_risks$owf == i] <- band_crm(
    model_options = 3,
    flight_speed = flight_speed,
    body_lt = body_lt,
    wing_span = wing_span,
    flight_type = flight_type,
    avoid_rt_ext = avoid_rt_ext,
    noct_activity = noct_activity,
    dens_month = dens_month,
    prop_upwind = prop_upwind,
    gen_fhd = gen_fhd,
    rotor_speed = owf_params$rotor_speed[owf_params$owf == i],
    rotor_radius = owf_params$rotor_radius[owf_params$owf == i],
    blade_width = owf_params$blade_width[owf_params$owf == i],
    blade_pitch = owf_params$blade_pitch[owf_params$owf == i],
    n_blades = owf_params$n_blades[owf_params$owf == i],
    hub_height = owf_params$hub_height[owf_params$owf == i],
    chord_prof = chord_prof_5MW,
    n_turbines = owf_params$n_turbines[owf_params$owf == i],
    turb_oper_month = turb_oper_month[turb_oper_month$owf == i, c("month", "prop_oper")],
    wf_width = owf_params$wf_width[owf_params$owf == i],
    wf_latitude = owf_params$wf_latitude[owf_params$owf == i],
    tidal_offset = owf_params$tidal_offset[owf_params$owf == i],
    lrg_arr_corr = FALSE
  )$opt3
  
  col_risks$daylight_hrs[col_risks$owf == i] <-   # Number of daylight hours by month
    Day_Length(owf_params$wf_latitude[owf_params$owf == i])$Day[c(6,7,8)]
}

# Divide by the number of daylight hours in each month and by 10 to get the collision risk over 6 min
col_risks$col_risk_timestep <- col_risks$col_risk_month/col_risks$daylight_hrs/10

# Average over the chick-rearing period (June-July-August)
col_risks <- col_risks %>%
  group_by(owf) %>%
  summarise(colRisk = mean(col_risk_timestep))

write_csv2(col_risks, here("data", "collision_risks.csv"))
