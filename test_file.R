game49_df <- {
  row <- schedule_df[schedule_df$game_n == 49,]
  teams <- c(row$home[1], row$away[1])
  scores <- c(row$home_score[1], row$away_score[1])
  pentalties <- c(row$home_penalty[1], row$away_penalty[1])
  return <- data.frame(date = row$date[1],
                       TV = row$TV[1],
                       country = teams,
                       score = scores)
  if(scores[1] ==  scores[2] && !is.na(scores[1])) {
    return$penalties <- penalties
  }
  return(return)
}
output$game_49 <- renderTable(game49_df())