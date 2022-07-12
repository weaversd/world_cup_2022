elo_rankings <- read.table("www/WF_elo_ratings.csv", sep = ",", header = T)
team_list <- elo_rankings$Country
goal_probabilities <- read.csv("www/goal_number_probabilities.csv", sep = ",",
                               header = T)

populate_winner_loser <- function(schedule_df){
  for (i in 1:nrow(schedule_df)) {
    if (!is.na(schedule_df$home_score[i]) && !is.na(schedule_df$away_score[i])) {
      if (schedule_df$home_score[i] == schedule_df$away_score[i] && schedule_df$knockout[i] == F) {
        schedule_df$winner[i] <- "Draw"
        schedule_df$loser[i] <- "Draw"
        schedule_df$draw[i] <- T
      } else if (schedule_df$home_score[i] > schedule_df$away_score[i]){
        schedule_df$winner[i] <- schedule_df$home[i]
        schedule_df$loser[i] <- schedule_df$away[i]
        schedule_df$draw[i] <- F
      } else if (schedule_df$home_score[i] < schedule_df$away_score[i]){
        schedule_df$winner[i] <- schedule_df$away[i]
        schedule_df$loser[i] <- schedule_df$home[i]
        schedule_df$draw[i] <- F
      } else {
        if (schedule_df$home_penalty[i] > schedule_df$away_penalty[i]) {
          schedule_df$winner[i] <- schedule_df$home[i]
          schedule_df$loser[i] <- schedule_df$away[i]
          schedule_df$draw[i] <- T
        } else if (schedule_df$home_penalty[i] < schedule_df$away_penalty[i]) {
          schedule_df$winner[i] <- schedule_df$away[i]
          schedule_df$loser[i] <- schedule_df$home[i]
          schedule_df$draw[i] <- T
        }
      }
    }
  }
  return(schedule_df)
}


create_group_tables <- function(schedule_df, updateProgress = NULL){
   group_tables <- list()
   for (group in LETTERS[1:8]) {
       group_games <- schedule_df[schedule_df$group == group,]
       group_teams <- unique(c(group_games$home, group_games$away))
       group_table <- data.frame(country = group_teams,
                                 Pts = 0,
                                 GP = 0,                
                                 W = 0,
                                 L = 0,
                                 D = 0,
                                 GS = 0,
                                 GA = 0,
                                 GD = 0,
                                 win_group_percent = NA,
                                 advance_KO_percent = NA,
                                 quarters_percent = NA,
                                 semis_percent = NA,
                                 finals_percent = NA,
                                 champions_percent = NA,
                                 won_vs = "",
                                 lost_vs = "",
                                 drew_vs = "")
       group_table <- add_column(group_table, "&nbsp" = group_teams, .before = "country")
         for (team in group_teams) {
             team_games <- group_games[group_games$away == team | group_games$home == team,]
             team_games <- team_games[!is.na(team_games$home_score),]
             group_table$W[group_table$country == team] <- sum(grepl(team, team_games$winner))
             group_table$L[group_table$country == team] <- sum(grepl(team, team_games$loser))
             group_table$D[group_table$country == team] <- sum(team_games$draw, na.rm = T)
             group_table$GP[group_table$country == team] <- nrow(team_games)
             goals_scored <- 0
             goals_against <- 0
             won_vs_string <- ""
             lost_vs_string <- ""
             drew_vs_string <- ""
             if (nrow(team_games > 0)){
                 for (i in 1:nrow(team_games)){
                     if (team_games$home[i] == team) {
                         goals_scored <- goals_scored + team_games$home_score[i]
                         goals_against <- goals_against + team_games$away_score[i]
                       } else if (team_games$away[i] == team) {
                         goals_scored <- goals_scored + team_games$away_score[i]
                         goals_against <- goals_against + team_games$home_score[i]
                       }
                     if (team_games$winner[i] == team){
                         won_vs_string <- paste0(won_vs_string, ";", team_games$loser[i])
                       } else if (team_games$loser[i] == team) {
                         lost_vs_string <- paste0(lost_vs_string, ";", team_games$winner[i])
                       } else if (team_games$winner[i] == "Draw" && team_games$home[i] == team) {
                         drew_vs_string <- paste0(drew_vs_string, ";", team_games$away[i])
                       } else if (team_games$winner[i] == "Draw" && team_games$away[i] == team) {
                         drew_vs_string <- paste0(drew_vs_string, ";", team_games$home[i])
                       }
                   
                   won_vs_string <- str_remove(won_vs_string, "^;")
                   lost_vs_string <- str_remove(lost_vs_string, "^;")
                   drew_vs_string <- str_remove(drew_vs_string, "^;")
                   group_table$GS[group_table$country == team] <- goals_scored
                   group_table$GA[group_table$country == team] <- goals_against
                   group_table$won_vs[group_table$country == team] <- won_vs_string
                   group_table$lost_vs[group_table$country == team] <- lost_vs_string
                   group_table$drew_vs[group_table$country == team] <- drew_vs_string
                   }
               }
            
             group_table$GD <- group_table$GS - group_table$GA
             group_table$Pts <- group_table$D + (3*group_table$W)
             group_table <- group_table[order(-group_table$Pts, -group_table$GD, -group_table$GS),]
             row.names(group_table) <- NULL
             
             
         }
       group_tables[[group]] <- group_table
   }
   return(group_tables)
 }

summarize_group_results <- function(schedule_df) {
  group_tables <- create_group_tables(schedule_df)
  
  winner_vec <- rep("", 8)
  runner_up_vec <- rep("", 8)
  
  for (i in 1:8) {
    winner_vec[i] <- group_tables[[i]]$country[1]
    runner_up_vec[i] <- group_tables[[i]]$country[2]
  }
  
  summary_df <- data.frame(group = LETTERS[1:8],
                           winner = winner_vec,
                           runner_up = runner_up_vec)
  return(summary_df)
}

populate_percentates_groups <- function(group_tables, summary_table) {
  for (i in 1:8) {
    group_table <- group_tables[[i]]
    for (j in 1:nrow(group_table)) {
      team <- group_table$country[j]
      group_tables[[i]]$win_group_percent[j] <- summary_table$group_win_perc[summary_table$country == team]
      group_tables[[i]]$advance_KO_percent[j] <- summary_table$advance_perc[summary_table$country == team]
      group_tables[[i]]$quarters_percent[j] <- summary_table$quarters_perc[summary_table$country == team]
      group_tables[[i]]$semis_percent[j] <- summary_table$semis_perc[summary_table$country == team]
      group_tables[[i]]$finals_percent[j] <- summary_table$finals_perc[summary_table$country == team]
      group_tables[[i]]$champions_percent[j] <- summary_table$champions_perc[summary_table$country == team]
    }
  }
  return(group_tables)
}



create_game_display_df_list <- function(schedule_df) {
  list_dfs <- list()
  for (i in 1:nrow(schedule_df)) {
    row <- schedule_df[i,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    df <- data.frame(date = row$date,
                     time_ET = row$time_est,
                     TV = row$TV,
                     group = row$group,
                     country = teams,
                     score = scores)
    if (!is.na(penalties[1])) {
      df$penalties <- penalties
    }
    df <- add_column(df, "&nbsp" = df$country, .before = "country")
    list_dfs[[i]] <- df
  }
  return(list_dfs)
}

image1 <- sprintf("data:image/png;base64,%s", base64encode("www/FS1.png"))
image2 <- sprintf("data:image/png;base64,%s", base64encode("www/FOX.png"))
flag_image <- function(country) {
  image <- sprintf("data:image/png;base64,%s", base64encode(paste0("www/flag_images/", country, ".png")))
  return(image)
}

flag_list <- list()
for (team in team_list) {
  flag_list[[team]] <- paste0("www/flag_images/", team, ".png")
}

flag_list_encoded <- list()
for (team in team_list) {
  flag_list_encoded[[team]] <- sprintf("data:image/png;base64,%s", base64encode(paste0("www/flag_images/", team, ".png")))
}

tv_image_tile <- formatter("img",
                           src = x ~ ifelse(x == "FS1", image1, image2),
                           width = 30, 
                           NA)
flag_image_tile <- formatter("img",
                             src = x ~ lapply(x, function(i) {
                               flag_list_encoded[[i]]
                             }),
                             width = 30,
                             NA)


create_goal_stat_df <- function(schedule_df) {
  if (all(is.na(schedule_df$home_score))) {
    return(data.frame(country = team_list[order(team_list)],
                      GF = 0,
                      GA = 0,
                      GD = 0,
                      game_n = 0))
  } else {
    goal_stat_list <- list()
    for (team in team_list) {
      team_df <- schedule_df[schedule_df$home == team | schedule_df$away == team,]
      team_df <- team_df[!is.na(team_df$home_score),]
      if (nrow(team_df) == 0) {
        break
      }
      goals_for_vec <- rep(0, nrow(team_df) +1)
      goals_against_vec <- rep(0, nrow(team_df) +1)
      goal_dif_vec <- rep(0, nrow(team_df) +1)
      game_count_vec <- rep(0, nrow(team_df) +1)
      for (i in 1:nrow(team_df)) {
        temp <- team_df[i,]
        if (temp$home[1] == team) {
          goals_for_vec[i+1] <- temp$home_score[1] +goals_for_vec[i]
          goals_against_vec[i+1] <- temp$away_score[1] + goals_against_vec[i]
        } else {
          goals_for_vec[i+1] <- temp$away_score[1] +goals_for_vec[i]
          goals_against_vec[i+1] <- temp$home_score[1] + goals_against_vec[i]
        }
        goal_dif_vec[i+1] <- (goals_for_vec[i+1] - goals_against_vec[i+1]) + goal_dif_vec[i]
        game_count_vec[i+1] <- i
      }
      team_goal_df <- data.frame(country = team,
                                 GF = goals_for_vec,
                                 GA = goals_against_vec,
                                 GD = goal_dif_vec,
                                 game_n = game_count_vec)
      goal_stat_list[[team]] <- team_goal_df
    }
    goal_plot_df <- bind_rows(goal_stat_list)
    goal_plot_df <- goal_plot_df[order(goal_plot_df$country),]
    return(goal_plot_df)
  }
}



