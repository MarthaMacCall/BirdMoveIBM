chick_unattendance <- function(adults, chicks, timeStep){
  n_at_nest <- adults[, .(n = sum(behav == "nest")), by = pair]
  both_at_sea <- n_at_nest[n == 0, pair]
  chicks[pair %in% both_at_sea, unattended_time := unattended_time + timeStep]
}