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
      }
    }
  }
  return(schedule_df)
}



create_group_tables <- function(schedule_df){
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
                                 won_vs = "",
                                 lost_vs = "",
                                 drew_vs = "")
       
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
                         lost_vs_string <- paste0(won_vs_string, ";", team_games$winner[i])
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
             
             group_tables[[group]] <- group_table
        }
   }
   return(group_tables)
 }
