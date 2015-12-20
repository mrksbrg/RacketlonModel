# result <- play_match(100000, 15687, 17065, 16077, 14351, 16323, 16928, 16039, 14379) # Borg - Hellberg
# result <- play_match(100000, 15687, 17065, 16077, 14351, 17031, 21081, 19962, 16985) # Borg - Ratzer
# result <- play_match(100000, 17031, 21081, 19962, 16985, 17510, 19942, 18641, 16764) # Ratzer - Jønsson
find_antiprofile <- function(p1_tt, p1_ba, p1_sq, p1_te) {
  # sum of own skills
  sum_skills <- p1_tt + p1_ba + p1_sq + p1_te
  
  # number of profiles to test
  profile <- NA
  
  # players
  profile[1] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt, p1_ba, p1_sq, p1_te) > 0) # mirror
  profile[2] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, sum_skills/4, sum_skills/4, sum_skills/4, sum_skills/4) > 0) # balanced
  #profile[3] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, 17031, 21081, 19962, 16985) > 0) # ratzer
  
  # specialists
  step <- 150
  for (i in 1:10) {
    profile[i*10+1] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba-i*step/3, p1_sq-i*step/3, p1_te-i*step/3) > 0)
    profile[i*10+2] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step/3, p1_ba+i*step, p1_sq-i*step/3, p1_te-i*step/3) > 0)
    profile[i*10+3] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step/3, p1_ba-i*step/3, p1_sq+i*step, p1_te-i*step/3) > 0)
    profile[i*10+4] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step/3, p1_ba-i*step/3, p1_sq-i*step/3, p1_te+i*step) > 0)
  }
  
  # dual skilled
  step <- 100
  for (i in 1:10) {
    profile[i*100+1] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba+i*step, p1_sq-i*step, p1_te-i*step) > 0)
    profile[i*100+2] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba-i*step, p1_sq+i*step, p1_te-i*step) > 0)
    profile[i*100+3] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step, p1_ba-i*step, p1_sq-i*step, p1_te+i*step) > 0)
    profile[i*100+4] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step, p1_ba+i*step, p1_sq+i*step, p1_te-i*step) > 0)
    profile[i*100+5] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step, p1_ba-i*step, p1_sq+i*step, p1_te+i*step) > 0)
  }
  
  # achilles
  step <- 150
  for (i in 1:10) {
    profile[i*1000+1] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt-i*step, p1_ba+i*step/3, p1_sq+i*step/3, p1_te+i*step/3) > 0)
    profile[i*1000+2] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step/3, p1_ba-i*step, p1_sq+i*step/3, p1_te+i*step/3) > 0)
    profile[i*1000+3] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step/3, p1_ba+i*step/3, p1_sq-i*step, p1_te+i*step/3) > 0)
    profile[i*1000+4] <- sum(play_match(1000, p1_tt, p1_ba, p1_sq, p1_te, p1_tt+i*step/3, p1_ba+i*step/3, p1_sq+i*step/3, p1_te-i*step) > 0)
  }
  
  anti_profile_id <- which.min(profile)
  best_profile_id <- which.min(profile)
  
  profile
}
