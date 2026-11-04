###################
# RESULT ANALYSIS #
###################

library(data.table)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(here)

here::here()

###########################
# SUMMARIZE ADULT OUTPUTS #
###########################

adult_files <- list.files(path = here::here(), pattern = "daily_adult_data", full.names = TRUE, recursive = TRUE)

collision  <- str_extract(basename(dirname(adult_files)), "^[^_]+")
avoid_rate <- as.numeric(str_extract(basename(dirname(adult_files)), "\\d+(?=_pct)"))
replicate  <- as.numeric(str_extract(basename(adult_files), "\\d+(?=\\.csv$)"))

numPairs <- 1000

adult_nrj_list <- list()
adult_mass_list <- list()
adult_mort_list <- list()

for(i in seq_along(adult_files)) {
  
  dt <- fread(
    adult_files[i],
    select = c("day", "alive_dead", "weight", "egain", "eexpend", "total_catch")
  )
  
  dt[, `:=`(
    collision = collision[i],
    avoid_rate = avoid_rate[i],
    replicate = replicate[i]
  )]
  
  dt <- dt[alive_dead != "dead_collision"]
  
  adult_nrj_list[[i]] <- dt[, .(day, egain, eexpend, total_catch, collision, avoid_rate, replicate)]
  
  dt <- dt[day == 91 & alive_dead == "alive"]
  
  if(nrow(dt) == 0) next
  
  adult_mass_list[[i]] <- dt[, .(weight, collision, avoid_rate, replicate)]
  
  adult_mort_list[[i]] <- dt[, .(
    n_alive = .N,
    mortality = 100 * (numPairs*2 - .N) / (numPairs*2)
  ), by = .(collision, avoid_rate, replicate)]
  
  rm(dt)
  gc()
}

adult_nrj_all <- rbindlist(adult_nrj_list)
adult_mass_all <- rbindlist(adult_mass_list)
adult_mort_all <- rbindlist(adult_mort_list)

rm(adult_nrj_list, adult_mass_list, adult_mort_list)
gc()

## ADULT MASS ##
adult_mass_summary <- adult_mass_all[, .(
  mean = mean(weight),
  sd   = sd(weight),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## ADULT MORTALITY
adult_mort_summary <- adult_mort_all[, .(
  mean = mean(mortality),
  sd   = sd(mortality),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## ADULT DAILY ENERGY GAINS
adult_egain_summary <- adult_nrj_all[, .(
  mean = mean(egain),
  sd = sd(egain),
  n = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## ADULT DAILY TOTAL CATCH
adult_total_catch_summary <- adult_nrj_all[, .(
  mean = mean(total_catch),
  sd = sd(total_catch),
  n = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## ADULT DAILY ENERGY EXPENDITURE
adult_eexpend_summary <- adult_nrj_all[, .(
  mean = mean(eexpend),
  sd = sd(eexpend),
  n = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## ADULT DAILY ENERGY BALANCE
adult_ebalance_summary <- adult_nrj_all[,
                                        ebalance := egain/eexpend
][, .(
  mean = mean(ebalance),
  sd = sd(ebalance),
  n = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

write.csv2(adult_mass_summary, here::here("adult_mass.csv"))
write.csv2(adult_mort_summary, here::here("adult_mortality.csv"))
write.csv2(adult_egain_summary, here::here("adult_egain.csv"))
write.csv2(adult_total_catch_summary, here::here("adult_total_catch.csv"))
write.csv2(adult_eexpend_summary, here::here("adult_eexpend.csv"))
write.csv2(adult_ebalance_summary, here::here("adult_ebalance.csv"))

rm(adult_nrj_all, adult_mass_all, adult_mort_all, adult_files, avoid_rate, collision, i, replicate)
gc()

###########################
# SUMMARIZE CHICK OUTPUTS #
###########################

chick_files <- list.files(path = here::here(), pattern = "daily_chick_data", full.names = TRUE, recursive = TRUE)

collision  <- str_extract(basename(dirname(chick_files)), "^[^_]+")
avoid_rate <- as.numeric(str_extract(basename(dirname(chick_files)), "\\d+(?=_pct)"))
replicate  <- as.numeric(str_extract(basename(chick_files), "\\d+(?=\\.csv$)"))

chick_provision_list <- list()
chick_mass_list <- list()
chick_mort_list <- list()
chick_unattend_list <- list()

for(i in seq_along(chick_files)) {
  
  dt <- fread(
    chick_files[i],
    select = c("day", "alive_dead", "chick_food", "weight", "unattended_time")
  )
  
  dt[, `:=`(
    collision = collision[i],
    avoid_rate = avoid_rate[i],
    replicate = replicate[i]
  )]
  
  chick_provision_list[[i]] <- dt[, .(day, chick_food, collision, avoid_rate, replicate)]
  
  chick_unattend_list[[i]] <- dt[, .(unattended_time, collision, avoid_rate, replicate)]
  
  dt <- dt[day == 91 & alive_dead == "alive"]
  
  if(nrow(dt) == 0) next
  
  chick_mass_list[[i]] <- dt[, .(weight, collision, avoid_rate, replicate)]
  
  chick_mort_list[[i]] <- dt[, .(
    n_alive = .N,
    mortality = 100 * (numPairs - .N) / numPairs
  ), by = .(collision, avoid_rate, replicate)]
  
  rm(dt)
  gc()
}

chick_provision_all <- rbindlist(chick_provision_list)
chick_mass_all <- rbindlist(chick_mass_list)
chick_mort_all <- rbindlist(chick_mort_list)
chick_unattend_all <- rbindlist(chick_unattend_list)

rm(chick_provision_list, chick_mass_list, chick_mort_list, chick_unattend_list)
gc()

## CHICK MASS ##
chick_mass_summary <- chick_mass_all[, .(
  mean = mean(weight),
  sd   = sd(weight),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## CHICK MORTALITY ##
chick_mort_summary <- chick_mort_all[, .(
  mean = mean(mortality),
  sd   = sd(mortality),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## CHICK PROVISIONING ##
numDays <- 91
fish_energy_density <- 5.8
prod_efficiency <- 0.75
assim <- 0.76

provision_max <- data.table(day = 0:numDays)

provision_max[,
              energy_req := fifelse(
                day <= 5,
                NA,
                ((0.0005 * day ^ 4) - (0.0873 * day ^ 3) + (3.9246 * day ^ 2) - (13.393 * day)) # energy required for growth (kJ)
                +((- 0.0001481 * day ^ 4) + (0.02065 * day ^ 3) - (0.6630 * day ^ 2) + (19.73 * day) - 63.69) # energy required for maintenance (kJ)
              )
][,
  chick_food_max := fifelse(
    day <= 5,
    2 * day,
    0.9 * (1 / assim) * energy_req / fish_energy_density
  )
]

provision_max <- provision_max[day != 0]

setDT(chick_provision_all)

chick_provision_all <- merge(
  chick_provision_all,
  provision_max,
  by = "day",
  all.x = TRUE
)[, pct_provision_max := 100 * (chick_food / chick_food_max)]

chick_provision_summary <- chick_provision_all[, .(
  mean = mean(pct_provision_max),
  sd   = sd(pct_provision_max),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## CHICK UNATTENDED TIME ##
chick_unattend_summary <- chick_unattend_all[, .(
  mean = mean(unattended_time),
  sd   = sd(unattended_time),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

write.csv2(chick_provision_summary, here::here("chick_provisioning.csv"))
write.csv2(chick_mass_summary, here::here("chick_mass.csv"))
write.csv2(chick_mort_summary, here::here("chick_mortality.csv"))

rm(chick_mort_all, chick_mass_all, chick_provision_all, chick_unattend_all, provision_max, assim,
   avoid_rate, chick_files, collision, fish_energy_density, i, numDays,
   prod_efficiency, replicate, numPairs)
gc()

##########################
# SUMMARIZE TRIP OUTPUTS #
##########################

trip_files <- list.files(path = here::here(), pattern = "trip_data", full.names = TRUE, recursive = TRUE)

collision  <- str_extract(basename(dirname(trip_files)), "^[^_]+")
avoid_rate <- as.numeric(str_extract(basename(dirname(trip_files)), "\\d+(?=_pct)"))
replicate  <- as.numeric(str_extract(basename(trip_files), "\\d+(?=\\.csv$)"))

trip_duration_list <- list()
trip_budget_list <- list()
trip_nrj_list <- list()

for(i in seq_along(trip_files)) {
  
  dt <- fread(
    trip_files[i],
    select = c("time_flying", "time_foraging", "time_resting", "time_at_sea", "eexpend_trip")
  )
  
  dt[, `:=`(
    collision = collision[i],
    avoid_rate = avoid_rate[i],
    replicate = replicate[i]
  )]
  
  trip_duration_list[[i]] <- dt[, .(time_at_sea, collision, avoid_rate, replicate)]
  
  trip_budget_list[[i]] <- dt[, .(time_flying, time_foraging, time_resting, time_at_sea, collision, avoid_rate, replicate)]
  
  trip_nrj_list[[i]] <- dt[, .(eexpend_trip, collision, avoid_rate, replicate)]
  
  rm(dt)
  gc()
}

trip_duration_all <- rbindlist(trip_duration_list)
trip_budget_all <- rbindlist(trip_budget_list)
trip_nrj_all <- rbindlist(trip_nrj_list)

rm(trip_duration_list, trip_budget_list, trip_nrj_list, avoid_rate, collision, replicate, trip_files)
gc()

## TRIP DURATION ##
trip_duration_all <- trip_duration_all[, time_at_sea := time_at_sea/60]

trip_duration_summary <- trip_duration_all[, .(
  mean = mean(time_at_sea),
  sd   = sd(time_at_sea),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## TIME-ACTIVITY BUDGET ##
trip_budget_all <- trip_budget_all[, `:=`(
  time_flying = time_flying/60,
  time_foraging = time_foraging/60,
  time_resting = time_resting/60,
  time_at_sea = time_at_sea/60
)]

trip_budget_all <- melt(
  trip_budget_all[, .(
    collision,
    avoid_rate,
    flying = (time_flying + time_foraging) / time_at_sea,
    resting = time_resting / time_at_sea
  )],
  id.vars = c("collision", "avoid_rate"),
  variable.name = "behaviour",
  value.name = "prop_time"
)

trip_budget_summary <- trip_budget_all[, .(
  mean = mean(prop_time),
  sd   = sd(prop_time),
  n    = .N), by = .(behaviour, collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

## ENERGY EXPENDED BY TRIP ##
trip_nrj_summary <- trip_nrj_all[, .(
  mean = mean(eexpend_trip),
  sd   = sd(eexpend_trip),
  n    = .N), by = .(collision, avoid_rate)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

rm(trip_budget_all, trip_nrj_all)
gc()

write.csv2(trip_duration_summary, here::here("trip_duration.csv"))
write.csv2(trip_budget_summary, here::here("trip_budget.csv"))
write.csv2(trip_nrj_summary, here::here("trip_nrj.csv"))

###################
# PLOT EVERYTHING #
###################

adult_mort_summary$avoid_rate <- as.factor(adult_mort_summary$avoid_rate)
chick_mort_summary$avoid_rate <- as.factor(chick_mort_summary$avoid_rate)
adult_mass_summary$avoid_rate <- as.factor(adult_mass_summary$avoid_rate)
chick_mass_summary$avoid_rate <- as.factor(chick_mass_summary$avoid_rate)
trip_duration_summary$avoid_rate <- as.factor(trip_duration_summary$avoid_rate)
adult_ebalance_summary$avoid_rate <- as.factor(adult_ebalance_summary$avoid_rate)
trip_budget_summary$avoid_rate <- as.factor(trip_budget_summary$avoid_rate)
trip_nrj_summary$avoid_rate <- as.factor(trip_nrj_summary$avoid_rate)
adult_egain_summary$avoid_rate <- as.factor(adult_egain_summary$avoid_rate)
chick_provision_summary$avoid_rate <- as.factor(chick_provision_summary$avoid_rate)
chick_unattend_summary$avoid_rate <- as.factor(chick_unattend_summary$avoid_rate)

adult_mort_summary <- adult_mort_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))

adult_mort_summary$scenario <- factor(adult_mort_summary$scenario,
                                      levels = c("baseline scenario",
                                                 "collision risk only",
                                                 "displacement/barrier effects only",
                                                 "collision risk and displacement/barrier effects"))

chick_mort_summary <- chick_mort_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
chick_mort_summary$scenario <- factor(chick_mort_summary$scenario,
                                      levels = c("baseline scenario",
                                                 "collision risk only",
                                                 "displacement/barrier effects only",
                                                 "collision risk and displacement/barrier effects"))

adult_mass_summary <- adult_mass_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
adult_mass_summary$scenario <- factor(adult_mass_summary$scenario,
                                      levels = c("baseline scenario",
                                                 "collision risk only",
                                                 "displacement/barrier effects only",
                                                 "collision risk and displacement/barrier effects"))

chick_mass_summary <- chick_mass_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
chick_mass_summary$scenario <- factor(chick_mass_summary$scenario,
                                      levels = c("baseline scenario",
                                                 "collision risk only",
                                                 "displacement/barrier effects only",
                                                 "collision risk and displacement/barrier effects"))

trip_duration_summary <- trip_duration_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
trip_duration_summary$scenario <- factor(trip_duration_summary$scenario,
                                         levels = c("baseline scenario",
                                                    "collision risk only",
                                                    "displacement/barrier effects only",
                                                    "collision risk and displacement/barrier effects"))

adult_ebalance_summary <- adult_ebalance_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
adult_ebalance_summary$scenario <- factor(adult_ebalance_summary$scenario,
                                          levels = c("baseline scenario",
                                                     "collision risk only",
                                                     "displacement/barrier effects only",
                                                     "collision risk and displacement/barrier effects"))

trip_budget_summary <- trip_budget_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
trip_budget_summary$scenario <- factor(trip_budget_summary$scenario,
                                       levels = c("baseline scenario",
                                                  "collision risk only",
                                                  "displacement/barrier effects only",
                                                  "collision risk and displacement/barrier effects"))

trip_nrj_summary <- trip_nrj_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
trip_nrj_summary$scenario <- factor(trip_nrj_summary$scenario,
                                    levels = c("baseline scenario",
                                               "collision risk only",
                                               "displacement/barrier effects only",
                                               "collision risk and displacement/barrier effects"))

adult_egain_summary <- adult_egain_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
adult_egain_summary$scenario <- factor(adult_egain_summary$scenario,
                                       levels = c("baseline scenario",
                                                  "collision risk only",
                                                  "displacement/barrier effects only",
                                                  "collision risk and displacement/barrier effects"))

chick_provision_summary <- chick_provision_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
chick_provision_summary$scenario <- factor(chick_provision_summary$scenario,
                                           levels = c("baseline scenario",
                                                      "collision risk only",
                                                      "displacement/barrier effects only",
                                                      "collision risk and displacement/barrier effects"))

chick_unattend_summary <- chick_unattend_summary %>%
  mutate(scenario = case_when(
    collision == "without" & avoid_rate == 0 ~ "baseline scenario",
    collision == "with" & avoid_rate == 0 ~ "collision risk only",
    collision == "without" ~ "displacement/barrier effects only",
    collision == "with" ~ "collision risk and displacement/barrier effects"
  ))
chick_unattend_summary$scenario <- factor(chick_unattend_summary$scenario,
                                          levels = c("baseline scenario",
                                                     "collision risk only",
                                                     "displacement/barrier effects only",
                                                     "collision risk and displacement/barrier effects"))

cb_colors <- c(
  "#0072B2",
  "#E69F00",
  "#009E73",
  "#CC79A7"
)

plot_adult_mort <- ggplot(adult_mort_summary, aes(x = avoid_rate, y = mean, colour = scenario)) + 
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(expand = FALSE) +
  labs(
    title = "a)",
    color = "Scenario",
    y = "Adult mortality (%)") +
  theme_classic(base_size = 16) +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),
        axis.text = element_text(size = 14, colour = "black"),
        legend.position = c(0.6,0.9),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        plot.margin = margin(10, 15, 10, 15))

plot_chick_mort <- ggplot(chick_mort_summary , aes(x = avoid_rate, y = mean, colour = scenario)) + 
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(expand = FALSE) +
  labs(
    title = "b)",
    y = "Chick mortality (%)") +
  theme_classic(base_size = 16) +
  theme(plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16, margin = margin(r = 10)),
        axis.text = element_text(size = 14, colour = "black"),
        legend.position = "none",
        plot.margin = margin(10, 15, 10, 15)) #+
#scale_y_continuous(breaks = seq(0, 1.25, 0.25))

plot_adult_mass <- ggplot(adult_mass_summary , aes(x = avoid_rate, y = mean, colour = scenario)) + 
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    linewidth = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(expand = FALSE) +
  labs(
    title = "c)",
    x = "Macro-avoidance rate (%)",
    y = "Adult final body mass (g)") +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 15)
  )

plot_chick_mass <- ggplot(chick_mass_summary , aes(x = avoid_rate, y = mean, colour = scenario)) + 
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)) +
  geom_point(size = 2,
             position = position_dodge(0.4)) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(expand = FALSE) +
  labs(
    title = "d)",
    x = "Macro-avoidance rate (%)",
    y = "Chick final body mass (g)") +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin = margin(10, 15, 10, 15)
  )

#Appendix

plot_trip_dur <- ggplot(trip_duration_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    linewidth = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(expand = FALSE) +
  labs(
    title = "a)",
    colour = "Scenario",
    y = "Trip duration (h)") +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position = c(0.6,0.25),
    legend.text = element_text(size = 14),
    legend.title = element_blank(),
    plot.margin = margin(10, 15, 10, 15))

plot_adult_ebalance <- ggplot(adult_ebalance_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "b)",
    y = "Adult daily energy efficiency"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text    = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin  = margin(10, 15, 10, 15)
  )

plot_trip_budget <- ggplot(trip_budget_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    linewidth = 0.6,
    position = position_dodge(width = 0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(width = 0.4)
  ) +
  facet_wrap(
    ~behaviour,
    scales = "free_y",
    nrow = 2,
    labeller = as_labeller(c(
      "flying" = "Flying / foraging",
      "resting" = "Resting"
    ))
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "c)",
    y = "Proportion of trip duration",
    colour = "Collision"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text    = element_text(size = 14, colour = "black"),
    strip.text   = element_text(size = 14),
    strip.background = element_blank(),
    legend.position = "none",
    plot.margin  = margin(10, 15, 10, 15)
  )

plot_trip_nrj <- ggplot(data = trip_nrj_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "d)",
    y = expression("Energy expended per trip (  "*kJ*".trip"^-1*")")
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text    = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin  = margin(10, 15, 10, 15)
  )

plot_adult_egain <- ggplot(adult_egain_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "e)",
    x = "Macro-avoidance rate (%)",
    y = expression("Adult daily energy gains ("*kJ*".day"^-1*")")
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text    = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin  = margin(10, 15, 10, 15)
  )

plot_chick_provision <- ggplot(chick_provision_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "f)",
    x = "Macro-avoidance rate (%)",
    y = "Chick provisioning rate\n(% of daily energy needs)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text    = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin  = margin(10, 15, 10, 15)
  )

plot_chick_unattend <- ggplot(chick_unattend_summary, aes(x = avoid_rate, y = mean, colour = scenario)) +
  geom_errorbar(
    aes(ymin = lower_bound, ymax = upper_bound),
    width = 0.15,
    size = 0.6,
    position = position_dodge(0.4)
  ) +
  geom_point(
    size = 2,
    position = position_dodge(0.4)
  ) +
  scale_colour_manual(values = cb_colors,
                      labels = c("Baseline",
                                 "Collision only",
                                 "Displacement/barrier effects only",
                                 "Collision and displacement/barrier effects")) +
  scale_x_discrete(drop = FALSE) +
  labs(
    title = "g)",
    x = "Macro-avoidance rate (%)",
    y = expression("Duration of chick\nunattendance (min.day"^-1*")")
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title   = element_text(size = 18),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10, l=10), vjust = 0.5),
    axis.text    = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin  = margin(10, 15, 10, 15)
  )

## EXPORT PLOTS ##
g1 <- grid.arrange(plot_adult_mort, plot_chick_mort, plot_adult_mass, plot_chick_mass, ncol = 2, nrow = 2)
ggsave(file = "results_main.jpg", g1,
       width = 30, 
       height = 25, 
       units = "cm")
g2 <- grid.arrange(plot_trip_dur, plot_adult_ebalance, plot_trip_budget, plot_trip_nrj, plot_adult_egain, plot_chick_provision, plot_chick_unattend, ncol = 2, nrow = 4)
ggsave(file = "results_appendix.jpg", g2,
       width = 30, 
       height = 40, 
       units = "cm")

####################
# MODEL VALIDATION #
####################

gps_data <- fread(here::here("data", "HMM_Rouzic_Morus_bassanus.csv"))
gps_data <- gps_data[, .(indID, tripID, datetime, longitude, latitude, behaviour)
][, datetime := as.POSIXct(datetime, format = "%d/%m/%Y %H:%M", tz = "GMT")
][!(year(datetime) %in% c(2006, 2022))
][order(tripID, datetime)]

# TRIP DURATION #

trip_duration_gps <- gps_data[, .(
  tripDuration = as.numeric(difftime(last(datetime), first(datetime), units = "hours"))),
  by = tripID
][, source := "gps"
][, .(tripDuration, source)]

trip_duration_gps_summ <- trip_duration_gps[, .(
  mean = mean(tripDuration),
  sd   = sd(tripDuration),
  n    = .N)
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

write.csv2(trip_duration_gps_summ, here::here("trip_duration_gps.csv"))

trip_duration_model <- trip_duration_all[collision == "without" & avoid_rate == 0
][, .(tripDuration = time_at_sea)
][, source := "model"]

trip_duration_comp <- rbindlist(
  list(trip_duration_gps, trip_duration_model),
  use.names = TRUE
)

plot_valid_trip_duration <- ggplot(trip_duration_comp, aes(x = source, y = tripDuration, fill = source)) +
  geom_boxplot(
    width = 0.6,
    linewidth = 0.5,
    outlier.size = 1.2,
    outlier.alpha = 0.6
  ) +
  scale_x_discrete(labels = c("GPS tracked\ntrips", "Simulated\ntrips")) +
  scale_fill_manual(values = c("grey30", "grey70")) +
  labs(title = "a)",
       y = "Trip duration (h)") +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_y_continuous(breaks = seq(0, 150, 20))

# TIME-ACTIVITY BUDGET #

trip_budget_gps <- gps_data[, behaviour := fifelse(behaviour == "resting", "resting", "flying")
][, .(n = .N), by = .(tripID, behaviour)
][, freq := n / sum(n), by = tripID
][, .(behaviour = as.factor(behaviour), prop_time = freq)
][, source := "gps"]

trip_budget_gps_summ <- trip_budget_gps[, .(
  mean = mean(prop_time),
  sd   = sd(prop_time),
  n    = .N), by = behaviour
][,
  error := qnorm(0.975)*sd/sqrt(n)
][,
  `:=`(
    lower_bound = mean - error,
    upper_bound = mean + error
  )
]

write.csv2(trip_budget_gps_summ, here::here("trip_budget_gps.csv"))

trip_budget_model <- trip_budget_all[collision == "without" & avoid_rate == 0
][, .(behaviour, prop_time)
][, source := "model"]

trip_budget_comp <- rbindlist(
  list(trip_budget_gps, trip_budget_model),
  use.names = TRUE
)

plot_valid_trip_budget <- ggplot(trip_budget_comp, aes(x = behaviour, y = prop_time, fill = source)) +
  geom_boxplot(
    width = 0.6,
    linewidth = 0.5,
    outlier.size = 1.2,
    outlier.alpha = 0.6
  ) +
  scale_x_discrete(labels = c("Flying / foraging", "Resting")) +
  scale_fill_manual(
    values = c("grey30", "grey70"),
    labels = c("GPS tracked trips", "Simulated trips")
  ) +
  labs(title = "b)",
       y = "Proportion of trip duration", fill = NULL) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14, colour = "black"),
    legend.position = "right",
    legend.text = element_text(size = 13),
    legend.key.size = unit(0.8, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

plots_model_valid <- grid.arrange(plot_valid_trip_duration, plot_valid_trip_budget, ncol = 2, widths = c(1.3, 2))

ggsave(file = here::here("model_validation.jpg"), plots_model_valid,
       width = 30, 
       height = 10, 
       units = "cm")
