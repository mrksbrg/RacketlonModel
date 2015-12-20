find_profiles <- function(n_sim, p1_tt, p1_ba, p1_sq, p1_te) {
  # sum of own skills
  sum_skills <- p1_tt + p1_ba + p1_sq + p1_te
  
  # number of profiles to test
  profile <- NA
  
  # players
  profile[1] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt, p1_ba, p1_sq, p1_te) > 0) # mirror
  profile[2] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, sum_skills/4, sum_skills/4, sum_skills/4, sum_skills/4) > 0) # balanced
  #profile[3] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, 17031, 21081, 19962, 16985) > 0) # ratzer
  
  # specialists
  step <- 300
  for (i in 1:5) {
    profile[i*10+1] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba-i*step/3, p1_sq-i*step/3, p1_te-i*step/3) > 0)
    profile[i*10+2] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step/3, p1_ba+i*step, p1_sq-i*step/3, p1_te-i*step/3) > 0)
    profile[i*10+3] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step/3, p1_ba-i*step/3, p1_sq+i*step, p1_te-i*step/3) > 0)
    profile[i*10+4] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step/3, p1_ba-i*step/3, p1_sq-i*step/3, p1_te+i*step) > 0)
  }
  
  # dual skilled
  step <- 200
  for (i in 1:5) {
    profile[i*100+1] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba+i*step, p1_sq-i*step, p1_te-i*step) > 0)
    profile[i*100+2] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba-i*step, p1_sq+i*step, p1_te-i*step) > 0)
    profile[i*100+3] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba-i*step, p1_sq-i*step, p1_te+i*step) > 0)
    profile[i*100+4] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step, p1_ba+i*step, p1_sq+i*step, p1_te-i*step) > 0)
    profile[i*100+5] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step, p1_ba-i*step, p1_sq+i*step, p1_te+i*step) > 0)
  }
  
  # achilles
  step <- 300
  for (i in 1:5) {
    profile[i*1000+1] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step, p1_ba+i*step/3, p1_sq+i*step/3, p1_te+i*step/3) > 0)
    profile[i*1000+2] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step/3, p1_ba-i*step, p1_sq+i*step/3, p1_te+i*step/3) > 0)
    profile[i*1000+3] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step/3, p1_ba+i*step/3, p1_sq-i*step, p1_te+i*step/3) > 0)
    profile[i*1000+4] <- sum(play_match(n_sim, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step/3, p1_ba+i*step/3, p1_sq+i*step/3, p1_te-i*step) > 0)
  }
  
  # find best and worst 5%
  quants <- quantile(profile, c(.05, .95), na.rm=TRUE)
  print("Best player profiles")
  best_inds <- which(profile >= quants[2])
  print(best_inds)
  print(" (% wins)")
  best_stats <- round(profile[best_inds] / n_sim, 2)
  print(best_stats)
  print("Worst player profiles")
  worst_inds <- which(profile <= quants[1])
  print(worst_inds)
  print(" (% wins)")
  worst_stats <- round(profile[worst_inds] / n_sim, 2)
  print(worst_stats)
  
  # return it as a list
  list(best_inds, best_stats, worst_inds, worst_stats)
}