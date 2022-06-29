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

predict_n_tournaments <- function(schedule_df, n_reps){
  group_stage_results_counter <- data.frame(country = team_list,
                                           group_wins = 0,
                                           group_runner_up = 0,
                                           group_win_perc = 0,
                                           group_runner_up_perc = 0,
                                           advance_count = 0,
                                           advance_perc = 0,
                                           quarters_count = 0,
                                           semis_count = 0,
                                           finals_count = 0,
                                           champions_count = 0,
                                           quarters_perc = 0,
                                           semis_perc = 0,
                                           finals_perc = 0,
                                           champions_perc = 0)
  for (i in 1:n_reps) {
    group_stage_results <- predict_group_stage_results(schedule_df)
    group_stage_final_summary <- summarize_group_results(group_stage_results)
    ko_results <- predict_KOs(schedule_df, group_stage_final_summary)
    
    quarters_teams <- ko_results[ko_results$group == "eighth",]$winner
    for (j in 1:length(quarters_teams)) {
      group_stage_results_counter$quarters_count[group_stage_results_counter$country == quarters_teams[j]] <- {
        group_stage_results_counter$quarters_count[group_stage_results_counter$country == quarters_teams[j]] + 1
      }
    }
    
    semis_teams <- ko_results[ko_results$group == "quarter",]$winner
    for (j in 1:length(semis_teams)) {
      group_stage_results_counter$semis_count[group_stage_results_counter$country == semis_teams[j]] <- {
        group_stage_results_counter$semis_count[group_stage_results_counter$country == semis_teams[j]] + 1
      }
    }
    
    finals_teams <- ko_results[ko_results$group == "semi",]$winner
    for (j in 1:length(finals_teams)) {
      group_stage_results_counter$finals_count[group_stage_results_counter$country == finals_teams[j]] <- {
        group_stage_results_counter$finals_count[group_stage_results_counter$country == finals_teams[j]] + 1
      }
    }
    
    champion_teams <- ko_results[ko_results$group == "final",]$winner
    for (j in 1:length(champion_teams)) {
      group_stage_results_counter$champions_count[group_stage_results_counter$country == champion_teams[j]] <- {
        group_stage_results_counter$champions_count[group_stage_results_counter$country == champion_teams[j]] + 1
      }
    }
    
    
    for (j in 1:8) {
      winner <- group_stage_final_summary$winner[j]
      runner_up <- group_stage_final_summary$runner_up[j]
      group_stage_results_counter$group_wins[group_stage_results_counter$country == winner] <- {
        group_stage_results_counter$group_wins[group_stage_results_counter$country == winner] + 1
      }
      group_stage_results_counter$group_runner_up[group_stage_results_counter$country == runner_up] <- {
        group_stage_results_counter$group_runner_up[group_stage_results_counter$country == runner_up] + 1
      }
      group_stage_results_counter$advance_count[group_stage_results_counter$country == winner] <- {
        group_stage_results_counter$advance_count[group_stage_results_counter$country == winner] + 1
      }
      group_stage_results_counter$advance_count[group_stage_results_counter$country == runner_up] <- {
        group_stage_results_counter$advance_count[group_stage_results_counter$country == runner_up] + 1
      }
    }
  }
  group_stage_results_counter$group_win_perc <- (group_stage_results_counter$group_wins / n_reps) * 100
  group_stage_results_counter$group_runner_up_perc <- (group_stage_results_counter$group_runner_up / n_reps) * 100
  group_stage_results_counter$advance_perc <- (group_stage_results_counter$advance_count / n_reps) * 100
  group_stage_results_counter$quarters_perc <- (group_stage_results_counter$quarters_count / n_reps) * 100
  group_stage_results_counter$semis_perc <- (group_stage_results_counter$semis_count / n_reps) * 100
  group_stage_results_counter$finals_perc <- (group_stage_results_counter$finals_count / n_reps) * 100
  group_stage_results_counter$champions_perc <- (group_stage_results_counter$champions_count / n_reps) * 100
  return(group_stage_results_counter)
}

elo_rankings <- read.table("www/WF_elo_ratings.csv", sep = ",", header = T)
team_list <- elo_rankings$Country
goal_probabilities <- read.csv("www/goal_number_probabilities.csv", sep = ",",
                                 header = T)

predict_KOs <- function(schedule_df, group_stage_summary) {
  kodf <- schedule_df[schedule_df$knockout == T,]
  for (i in 1:8) {
    if (!(kodf$home[i] %in% team_list)) {
      group_letter <- str_extract(kodf$home[i], "[A-Z]")
      kodf$home[i] <- group_stage_summary$winner[group_stage_summary$group == group_letter]
    }
    if (!(kodf$away[i] %in% team_list)) {
      group_letter <- str_extract(kodf$away[i], "[A-Z]")
      kodf$away[i] <- group_stage_summary$runner_up[group_stage_summary$group == group_letter]
    }
  }
  for (i in 1:8) {
    game_score <- predict_game(kodf$home[i], kodf$away[i])
    kodf$home_score[i] <- game_score[1]
    kodf$away_score[i] <- game_score[2]
    kodf <- populate_winner_loser(kodf)
  }
  for (i in 9:12) {
    if (!(kodf$home[i] %in% team_list)) {
      game_number <- as.numeric(str_extract(kodf$home[i], "[:digit:][:digit:]"))
      kodf$home[i] <- kodf$winner[kodf$game_n == game_number]
    }
    if (!(kodf$away[i] %in% team_list)) {
      game_number <- as.numeric(str_extract(kodf$away[i], "[:digit:][:digit:]"))
      kodf$away[i] <- kodf$winner[kodf$game_n == game_number]
    }
  }
  for (i in 9:12) {
    game_score <- predict_game(kodf$home[i], kodf$away[i])
    kodf$home_score[i] <- game_score[1]
    kodf$away_score[i] <- game_score[2]
    kodf <- populate_winner_loser(kodf)
  }
  for (i in 13:14) {
    if (!(kodf$home[i] %in% team_list)) {
      game_number <- as.numeric(str_extract(kodf$home[i], "[:digit:][:digit:]"))
      kodf$home[i] <- kodf$winner[kodf$game_n == game_number]
    }
    if (!(kodf$away[i] %in% team_list)) {
      game_number <- as.numeric(str_extract(kodf$away[i], "[:digit:][:digit:]"))
      kodf$away[i] <- kodf$winner[kodf$game_n == game_number]
    }
  }
  for (i in 13:14) {
    game_score <- predict_game(kodf$home[i], kodf$away[i])
    kodf$home_score[i] <- game_score[1]
    kodf$away_score[i] <- game_score[2]
    kodf <- populate_winner_loser(kodf)
  }
  if (!(kodf$home[15] %in% team_list)) {
    kodf$home[15] <- kodf$loser[13]
  }
  if (!(kodf$away[15] %in% team_list)) {
    kodf$away[15] <- kodf$loser[14]
  }
  if (!(kodf$home[16] %in% team_list)) {
    kodf$home[16] <- kodf$winner[13]
  }
  if (!(kodf$away[16] %in% team_list)) {
    kodf$away[16] <- kodf$winner[14]
  }
  for (i in 15:16) {
    game_score <- predict_game(kodf$home[i], kodf$away[i])
    kodf$home_score[i] <- game_score[1]
    kodf$away_score[i] <- game_score[2]
    kodf <- populate_winner_loser(kodf)
  }
  

  return(kodf)
}
