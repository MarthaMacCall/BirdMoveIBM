chick_needs <- function(chicks, chick_params, day){
  
  # Chicks that have not been fed yesterday can receive a double ration today (chick_food_max of today + chick_food_max of yesterday)
  today_food <- chick_params[today == day, chick_food_max]
  yesterday_food <- if (day > 1) chick_params[today == (day-1), chick_food_max] else 0
  
  chicks[, chick_food_max := fifelse(dayfed == (day-1),
                                     today_food,
                                     today_food + yesterday_food)]
  
}
