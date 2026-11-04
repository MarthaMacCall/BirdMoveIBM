digest <- function(adults, fish_to_digest, tot_minute, time_to_digest, timeStep){
  
  # Step 1: calculate digested mass per adult during this time step
  digestion <- fish_to_digest[time >= tot_minute - time_to_digest,
                              .(digested = sum(mass * timeStep / time_to_digest)),
                              by = adultID]
  
  if(nrow(digestion) > 0){
    # Step 2: limit digested mass to current stomach content
    adults[digestion, on = "adultID", digested := pmin(stomach_content, i.digested)]
    
    # Step 3: update stomach_content and digested_food in place
    adults[!is.na(digested), `:=`(
      stomach_content = stomach_content - digested,
      digested_food = digested_food + digested
    )]
    
    # Remove temporary column
    adults[, digested := NULL]
    
    if(runTests){
      stopifnot(all(adults$stomach_content >= 0))
    }
  }
}