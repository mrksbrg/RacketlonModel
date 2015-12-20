# play_match(15687, 17065, 16077, 14351, 16323, 16928, 16039, 14379)
play_match <- function(p1_tt, p1_ba, p1_sq, p1_te, p2_tt, p2_ba, p2_sq, p2_te) {
  result_tt <- 1:100000
  result_ba <- 1:100000
  result_sq <- 1:100000
  result_te <- 1:100000
  diff <- 1:100000
  for (i in 1:100000) {
    result_tt[i] <- 0
    result_ba[i] <- 0
    result_sq[i] <- 0
    result_te[i] <- 0
    diff[i] <- 0
    
    # table tennis
    p1_performance <- rnorm(1, p1_tt, 100)
    p2_performance <- rnorm(1, p2_tt, 100)
    result_tt[i] <- play_tt_set(p1_tt, p2_tt)
    diff[i] <- result_tt[i]
    
    # badminton
    p1_performance <- rnorm(1, p1_ba, 100)
    p2_performance <- rnorm(1, p2_ba, 100)
    result_ba[i] <- play_ba_set(p1_performance, p2_performance)
    diff[i] <- diff[i] + result_ba[i]
    
    # squash
    p1_performance <- rnorm(1, p1_sq, 100)
    p2_performance <- rnorm(1, p2_sq, 100)
    result_sq[i] <- play_sq_set(p1_performance, p2_performance)
    diff[i] <- diff[i] + result_sq[i]
    
    # tennis
    p1_performance <- rnorm(1, p1_te, 100)
    p2_performance <- rnorm(1, p2_te, 100)
    result_te[i] <- play_te_set(p1_performance, p2_performance, diff[i])
    diff[i] <- diff[i] + result_te[i]
  }
  diff
}
