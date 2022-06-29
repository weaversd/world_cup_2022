library(readxl)
historical_df <- read_xlsx("www/WorldCupMatches.xlsx")

recent_df <- historical_df[historical_df$Year >= 2002,]
recent_df$total_goals <- recent_df$`Home Team Goals` + recent_df$`Away Team Goals`
recent_df$Draw <- F
for (i in 1:nrow(recent_df)) {
  if (recent_df$`Home Team Goals`[i] == recent_df$`Away Team Goals`[i]){
    recent_df$Draw[i] <- T
  }
}


average_goals_per_game <- mean(recent_df$total_goals)
standard_deviation_goals_per_game <- sd(recent_df$total_goals)

library(ggplot2)
ggplot(recent_df) +
  geom_histogram(aes(x = total_goals), binwidth = 1)


draw_probability <- sum(recent_df$Draw) / nrow(recent_df)

sample(recent_df$total_goals, 1)
zero <- nrow(recent_df[recent_df$total_goals == 0,]) / nrow(recent_df)
one <- nrow(recent_df[recent_df$total_goals == 1,]) / nrow(recent_df)
two <- nrow(recent_df[recent_df$total_goals == 2,]) / nrow(recent_df)
three <- nrow(recent_df[recent_df$total_goals == 3,]) / nrow(recent_df) 
four <- nrow(recent_df[recent_df$total_goals == 4,]) / nrow(recent_df)
five <- nrow(recent_df[recent_df$total_goals == 5,]) / nrow(recent_df) 
six <- nrow(recent_df[recent_df$total_goals == 6,]) / nrow(recent_df)
seven <- nrow(recent_df[recent_df$total_goals == 7,]) / nrow(recent_df)
eight <- nrow(recent_df[recent_df$total_goals == 8,]) / nrow(recent_df)

goal_n_probs <- data.frame(goals_n = 0:8,
                           probability = c(zero, one, two, three, four,
                                           five, six, seven, eight))
goal_n_probs$cutoff <- 1
goal_n_probs$cutoff[1] <- goal_n_probs$cutoff[2] + goal_n_probs$probability[1]
goal_n_probs$cutoff[2] <- goal_n_probs$cutoff[3] + goal_n_probs$probability[2]
goal_n_probs$cutoff[3] <- goal_n_probs$cutoff[4] + goal_n_probs$probability[3]
goal_n_probs$cutoff[4] <- goal_n_probs$cutoff[5] + goal_n_probs$probability[4]
goal_n_probs$cutoff[5] <- goal_n_probs$cutoff[6] + goal_n_probs$probability[5]
goal_n_probs$cutoff[6] <- goal_n_probs$cutoff[7] + goal_n_probs$probability[6]
goal_n_probs$cutoff[7] <- goal_n_probs$cutoff[8] + goal_n_probs$probability[7]
goal_n_probs$cutoff[8] <- goal_n_probs$cutoff[9] + goal_n_probs$probability[8]
goal_n_probs$cutoff[9] <- 0 + goal_n_probs$probability[9]

sum(goal_n_probs$probability)

write.csv(goal_n_probs, "www/goal_number_probabilities.csv", row.names = F)
