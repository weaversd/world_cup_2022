#historical draw probability if 0.235 -> d = 83.202

 predict_game <- function(hometeam, awayteam){
  home_elo <- elo_rankings$Elo[elo_rankings$Country == hometeam]
  away_elo <- elo_rankings$Elo[elo_rankings$Country == awayteam]
  difference_elo <- (home_elo - away_elo)
  # win_percent_bin_home <- 1 / ((10^(-(difference_elo/400))) + 1)
  # win_percent_bin_away <- 1 - win_percent_bin_home
  
  win_percent_home <- 1 / ((10^(((83.202 - difference_elo)/400))) + 1)
  win_percent_away <- 1 / ((10^(((83.202 + difference_elo)/400))) + 1)
  draw_percent <- 1 - (win_percent_home + win_percent_away)
  win_random <- runif(1)
  if (win_random < win_percent_home) {
    result <- predict_goals(draw = F, home_winner = T)
    home_score <- result[1]
    away_score <- result[2]
  } else if (win_random > ( 1- win_percent_away)) {
    result <- predict_goals(draw = F, home_winner = F)
    home_score <- result[1]
    away_score <- result[2]
  } else {
    result <- predict_goals(draw = T, home_winner = F)
    home_score <- result[1]
    away_score <- result[2]
  }
  
  return(c(home_score, away_score))
}

predict_goals <- function(draw = F, home_winner = T) {
  goal_random <- runif(1)
  closest_index <- which.min(abs(goal_probabilities$cutoff - goal_random))
  goal_value <- goal_probabilities$goals_n[closest_index]
  if (draw == F && goal_value == 0) {
    goal_value <- 1
  }
  if (draw == T) {
    home_score <- (2*round(goal_value/2))/2
    away_score <- (2*round(goal_value/2))/2
  } else {
    all_combos <- expand.grid(0:8, 0:8)
    all_combos$total <- all_combos$Var1 + all_combos$Var2
    all_combos <- all_combos[all_combos$Var1 != all_combos$Var2,]
    all_combos <- all_combos[all_combos$total == goal_value,]
    if (home_winner == T) {
      all_combos <- all_combos[all_combos$Var1 > all_combos$Var2,]
      random_index <- sample(1:nrow(all_combos), 1)
      home_score <- all_combos$Var1[random_index]
      away_score <- all_combos$Var2[random_index]
    } else if (home_winner == F) {
      all_combos <- all_combos[all_combos$Var1 < all_combos$Var2,]
      random_index <- sample(1:nrow(all_combos), 1)
      home_score <- all_combos$Var1[random_index]
      away_score <- all_combos$Var2[random_index]
    }
  }
  return(c(home_score, away_score))
}


predict_group_stage_results <- function(schedule_df){
  for (i in 1:nrow(schedule_df)) {
    if (is.na(schedule_df$home_score[i]) && schedule_df$knockout[i] == F) {
      predicted_score <- predict_game(schedule_df$home[i], schedule_df$away[i])
      schedule_df$home_score[i] <- predicted_score[1]
      schedule_df$away_score[i] <- predicted_score[2]
    }
  }
  schedule_df <- populate_winner_loser(schedule_df)
  return(schedule_df)
}


elo_rankings <- read.table("www/WF_elo_ratings.csv", sep = ",", header = T)
goal_probabilities <- read.csv("www/goal_number_probabilities.csv", sep = ",",
                                 header = T)
