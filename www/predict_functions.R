
elo_rankings <- read.table("www/WF_elo_ratings.csv", sep = ",", header = T)
team_list <- elo_rankings$Country
goal_probabilities <- read.csv("www/goal_number_probabilities.csv", sep = ",",
                               header = T)


#historical draw probability if 0.235 -> d = 83.202

 predict_game <- function(hometeam, awayteam, elo_rankings){
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


predict_group_stage_results <- function(schedule_df, elo_rankings){
  for (i in 1:nrow(schedule_df)) {
    if (is.na(schedule_df$home_score[i]) && schedule_df$knockout[i] == F) {
      predicted_score <- predict_game(schedule_df$home[i], schedule_df$away[i], elo_rankings)
      schedule_df$home_score[i] <- predicted_score[1]
      schedule_df$away_score[i] <- predicted_score[2]
    }
  }
  schedule_df <- populate_winner_loser(schedule_df)
  return(schedule_df)
}

predict_n_tournaments <- function(schedule_df, n_reps, elo_rankings){
  
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
    # percent_done <- round((i/n_reps) * 100)
    # if (i > 0) {
    #   previous_percent_done <- round(((i-1)/n_reps) * 100)
    # } else {
    #   previous_percent_done <- 0
    # }
    # if ((percent_done %% 10 == 0) && (previous_percent_done %% 10 !=0 )) {
    #   print(paste0(percent_done, "% complete"))
    # }
    group_stage_results <- predict_group_stage_results(schedule_df, elo_rankings)
    group_stage_final_summary <- summarize_group_results(group_stage_results)
    ko_results <- predict_KOs(schedule_df, group_stage_final_summary, elo_rankings)
    
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


predict_KOs <- function(schedule_df, group_stage_summary, elo_rankings) {
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
    if (is.na(kodf$home_score[i])) {
      game_score <- predict_game(kodf$home[i], kodf$away[i], elo_rankings)
      kodf$home_score[i] <- game_score[1]
      kodf$away_score[i] <- game_score[2]
      if (game_score[1] == game_score[2]) {
        kodf$home_penalty[i] <- sample(2:5, 1)
        kodf$away_penalty[i] <- sample(2:5, 1)
        if (kodf$home_penalty[i] == kodf$away_penalty[i]) {
          kodf$home_penalty[i] <- kodf$home_penalty[i] + 1
        }
      }
    }
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
    if (is.na(kodf$home_score[i])) {
      game_score <- predict_game(kodf$home[i], kodf$away[i], elo_rankings)
      kodf$home_score[i] <- game_score[1]
      kodf$away_score[i] <- game_score[2]
      if (game_score[1] == game_score[2]) {
        kodf$home_penalty[i] <- sample(2:5, 1)
        kodf$away_penalty[i] <- sample(2:5, 1)
        if (kodf$home_penalty[i] == kodf$away_penalty[i]) {
          kodf$home_penalty[i] <- kodf$home_penalty[i] + 1
        }
      }
    }
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
    if (is.na(kodf$home_score[i])) {
      game_score <- predict_game(kodf$home[i], kodf$away[i], elo_rankings)
      kodf$home_score[i] <- game_score[1]
      kodf$away_score[i] <- game_score[2]
      if (game_score[1] == game_score[2]) {
        kodf$home_penalty[i] <- sample(2:5, 1)
        kodf$away_penalty[i] <- sample(2:5, 1)
        if (kodf$home_penalty[i] == kodf$away_penalty[i]) {
          kodf$home_penalty[i] <- kodf$home_penalty[i] + 1
        }
      }
    }
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
    if (is.na(kodf$home_score[i])) {
      game_score <- predict_game(kodf$home[i], kodf$away[i], elo_rankings)
      kodf$home_score[i] <- game_score[1]
      kodf$away_score[i] <- game_score[2]
      if (game_score[1] == game_score[2]) {
        kodf$home_penalty[i] <- sample(2:5, 1)
        kodf$away_penalty[i] <- sample(2:5, 1)
        if (kodf$home_penalty[i] == kodf$away_penalty[i]) {
          kodf$home_penalty[i] <- kodf$home_penalty[i] + 1
        }
      }
    }
    kodf <- populate_winner_loser(kodf)
  }
  

  return(kodf)
}


# 
# create_time_course_predictions <- function(schedule_df, n_reps = 100) {
#   matchday_vector <- unique(schedule_df$matchday)
#   matchday_df <- data.frame(matchday_name = matchday_vector,
#                             matchday_n = 1:7)
#   schedule_df$matchday_n <- 0
#   label_list <- c("start", "group_1", "group_2", "group_3",
#                   "knockout_1", "quarters", "semis", "final")
#   predicted_results_list <- list()
#   mddf_list <- list()
#   for (i in 1:nrow(schedule_df)){
#     schedule_df$matchday_n[i] <- matchday_df$matchday_n[matchday_df$matchday_name == schedule_df$matchday[i]]
#   }
#   for (i in 0:7) {
#     mddf <- schedule_df
#     mddf$home_score[mddf$matchday_n > i] <- NA
#     mddf$away_score[mddf$matchday_n > i] <- NA
#     mddf$winner[mddf$matchday_n > i] <- NA
#     mddf$loser[mddf$matchday_n > i] <- NA
#     mddf$draw[mddf$matchday_n > i] <- NA
#     mddf$home_penalty[mddf$matchday_n > i] <- NA
#     mddf$away_penalty[mddf$matchday_n > i] <- NA
# 
#     mddf_list[[i+1]] <- mddf
# 
#     if (i > 0 && isTRUE(all.equal(mddf_list[[i]], mddf))) {
#       predicted_results_list[[i+1]] <- predicted_results_list[[i]]
#       print(paste0("round ", i+1, " of 8"))
#       print("using previous")
#     } else {
#       print(paste0("round ", i+1, " of 8"))
#       md_predictions <- predict_n_tournaments(mddf, n_reps)
#       md_predictions$label <- i
#       predicted_results_list[[i+1]] <- md_predictions
#     }
#   }
#   predicted_results_df <- bind_rows(predicted_results_list)
#   #predicted_results_df$label <- factor(predicted_results_df$label, levels = label_list)
#   print("done")
#   return(predicted_results_df)
# }
# 



create_time_course_predictions <- function(schedule_df, n_reps = 100, elo_rankings) {
  matchday_vector <- unique(schedule_df$matchday)
  matchday_df <- data.frame(matchday_name = matchday_vector,
                            matchday_n = 1:7)
  schedule_df$matchday_n <- 0
  label_list <- c("start", "group_1", "group_2", "group_3",
                  "knockout_1", "quarters", "semis", "final")
  predicted_results_list <- list()
  mddf_list <- list()
  for (i in 1:nrow(schedule_df)){
    schedule_df$matchday_n[i] <- matchday_df$matchday_n[matchday_df$matchday_name == schedule_df$matchday[i]]
  }
  for (i in 0:7) {
    mddf <- schedule_df
    mddf$home_score[mddf$matchday_n > i] <- NA
    mddf$away_score[mddf$matchday_n > i] <- NA
    mddf$winner[mddf$matchday_n > i] <- NA
    mddf$loser[mddf$matchday_n > i] <- NA
    mddf$draw[mddf$matchday_n > i] <- NA
    mddf$home_penalty[mddf$matchday_n > i] <- NA
    mddf$away_penalty[mddf$matchday_n > i] <- NA
    
    mddf_list[[i+1]] <- mddf
    
    if (i > 0 && isTRUE(all.equal(mddf_list[[i]], mddf))) {
      predicted_results_list[[i+1]] <- predicted_results_list[[i]]
      print(paste0("round ", i+1, " of 8"))
      print("using previous")
    } else {
      print(paste0("round ", i+1, " of 8"))
      round_10_list <- list()
      for (j in 1:10){
        print(paste0(j*10, "% complete"))
        md_predictions <- predict_n_tournaments(mddf, (n_reps%/%10), elo_rankings)
        round_10_list[[j]] <- md_predictions
        
        round_10_long <- bind_rows(round_10_list)
        md_predictions <- round_10_long %>%
          group_by(country) %>%
          summarise_all(list("avg" = mean, "sd" = sd))
        
      }
      md_predictions$matchday <- i
      predicted_results_list[[i+1]] <- md_predictions
    }
  }
  predicted_results_df <- bind_rows(predicted_results_list)
  predicted_results_df$group <- ""
  for (i in 1:nrow(predicted_results_df)) {
    predicted_results_df$group[i] <- elo_rankings$group[elo_rankings$Country == predicted_results_df$country[i]]
  }
  print("done")
  return(predicted_results_df)
}

