# simulate the score in a tennis set
play_te_set <- function(p1_performance, p2_performance, total_diff) {
  p1_score <- 0
  p2_score <- 0
  points_left <- 21
  if (abs(p1_performance - p2_performance) < 100) {
    p2_prob <- 0.5
  }
  else if (p1_performance > p2_performance) {
    if (p1_performance - p2_performance < 200) {
      p2_prob <- 0.48
    }
    else if (p1_performance - p2_performance < 300) {
      p2_prob <- 0.46
    }
    else if (p1_performance - p2_performance < 400) {
      p2_prob <- 0.44
    }
    else if (p1_performance - p2_performance < 500) {
      p2_prob <- 0.444
    }
    else if (p1_performance - p2_performance < 1000) {
      p2_prob <- 0.382
    }
    else if (p1_performance - p2_performance < 1500) {
      p2_prob <- 0.346
    }
    else if (p1_performance - p2_performance < 2000) {
      p2_prob <- 0.344
    }
    else {
      p2_prob <- 0.10
    }
  }
  else {
    if (p2_performance - p1_performance < 200) {
      p2_prob <- 0.52
    }
    else if (p2_performance - p1_performance < 300) {
      p2_prob <- 0.54
    }
    else if (p2_performance - p1_performance < 400) {
      p2_prob <- 0.56
    }
    else if (p2_performance - p1_performance < 500) {
      p2_prob <- 0.556
    }
    else if (p2_performance - p1_performance < 1000) {
      p2_prob <- 0.618
    }
    else if (p2_performance - p1_performance < 1500) {
      p2_prob <- 0.654
    }
    else if (p2_performance - p1_performance < 2000) {
      p2_prob <- 0.656
    }
    else {
      p2_prob <- 0.9
    } 
  }
  
  # play the set
  if (abs(total_diff) <= 21) {
    if (total_diff > 0) {
      while (p1_score < (22-total_diff) && p2_score < 21) {
        if (runif(1, 0, 1) <= p2_prob) {
          p2_score <- p2_score + 1
        }
        else {
          p1_score <- p1_score + 1
        }
      }
    }
    else if (total_diff < 0) {
      while (p1_score < 21 && p2_score < (22+total_diff)) {
        if (runif(1, 0, 1) <= p2_prob) {
          p2_score <- p2_score + 1
        }
        else {
          p1_score <- p1_score + 1
        }
      }
    }
    else {
      while (p1_score < 21 && p2_score < 21) {
        if (runif(1, 0, 1) <= p2_prob) {
          p2_score <- p2_score + 1
        }
        else {
          p1_score <- p1_score + 1
        }
      }
    }
    
    # possible gummiarm tiebreak
    if (p1_score + total_diff - p2_score == 0) {
      if (runif(1, 0, 1) <= p2_prob) {
        p2_score <- p2_score + 1
      }
      else {
        p1_score <- p1_score + 1
      }
    }
  }
  
  result <- p1_score - p2_score
}