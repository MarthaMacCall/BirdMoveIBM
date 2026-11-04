choose_foraging_site <- function(group, foragingSites_cdf) {
  
  idx0 <- which(group$avoids_owf == 0)
  idx1 <- which(group$avoids_owf == 1)
  
  if(length(idx0) > 0){
    u0 <- runif(length(idx0))
    group$destination[idx0] <- foragingSites_cdf$cellID0[findInterval(u0, foragingSites_cdf$cdf0) + 1]
  }
  
  if(length(idx1) > 0){
    u1 <- runif(length(idx1))
    group$destination[idx1] <- foragingSites_cdf$cellID1[findInterval(u1, foragingSites_cdf$cdf1) + 1]
  }
  
  return(group)
}
