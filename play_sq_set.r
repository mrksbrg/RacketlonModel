# simulate the score in a squash set
play_sq_set <- function(p1_performance, p2_performance) {
  p1_score <- 0
  p2_score <- 0
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
      p2_prob <- 0.437
    }
    else if (p1_performance - p2_performance < 1000) {
      p2_prob <- 0.327
    }
    else if (p1_performance - p2_performance < 1500) {
      p2_prob <- 0.273
    }
    else if (p1_performance - p2_performance < 2000) {
      p2_prob <- 0.228
    }
    else {
      p2_prob <- 0.05
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
      p2_prob <- 0.563
    }
    else if (p2_performance - p1_performance < 1000) {
      p2_prob <- 0.673
    }
    else if (p2_performance - p1_performance < 1500) {
      p2_prob <- 0.709
    }
    else if (p2_performance - p1_performance < 2000) {
      p2_prob <- 0.727
    }
    else {
      p2_prob <- 0.95
    } 
  }
  # play the set
  while (p1_score < 21 && p2_score < 21) {
    if (runif(1, 0, 1) <= p2_prob) {
      p2_score <- p2_score + 1
    }
    else {
      p1_score <- p1_score + 1
    }
  }
  # possible tiebreak
  while (abs(p1_score-p2_score)<2) {
    if (runif(1, 0, 1) <= p2_prob) {
      p2_score <- p2_score + 1
    }
    else {
      p1_score <- p1_score + 1
    }
  }
  result <- p1_score - p2_score
}