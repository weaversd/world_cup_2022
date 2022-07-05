library(ggplot2)
color_list <- c("#66A3D8", "#003000", "#ECB626", "#169150", "#F4CA15",
                "#D7080F", "#000F70", "#E39B99", "#DA161D", "#FAEA0E",
                "#BE0007", "#00228E", "#010201", "#686806", "#D5D5CD",
                "#D9000D", "#159154", "#6B542B", "#FC5B00", "#C91F3A", 
                "#3F9F3E", "#62002F", "#016C36", "#179253", "#0D4076",
                "#3F3F3F", "#FAEA10", "#DE1116", "#969696", "#03044B",
                "#496AB0", "#00A236")
colors <- data.frame(row.names = team_list[order(team_list)], color = color_list)
label_list <- c("start", "group game 1", "group game 2", "group game 3",
                "knockout round 1", "quarters", "semis", "final")
#to look at the colors for testing:
# color_test_df <- data.frame(teams = team_list[order(team_list)],
#                             number = 1:32,
#                             color = color_list)
# 
# test <- ggplot(data = color_test_df) +
#   geom_point(aes(x = teams, y = number), size = 12, color = color_test_df$color) +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# test


# label_list <- c("start", "group_1", "group_2", "group_3",
#                 "knockout_1", "quarters", "semis", "final")

create_plotly_KO_chance <- function(prediction_df, n_reps, error_type = "No") {
  team_list <- unique(prediction_df$country)
  color_list <- colors[team_list,]
  
  plot <- ggplot(data = prediction_df) +
    geom_point(aes(x = matchday, y = advance_perc_avg, color = country, plus_minus = advance_perc_sd)) +
    geom_line(aes(x = matchday, y = advance_perc_avg, color = country, plus_minus = advance_perc_sd)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = 0:max(prediction_df$matchday), labels = label_list[1:(max(prediction_df$matchday)+1)]) +
    scale_colour_manual(values = color_list) +
    scale_y_continuous(breaks = (seq(0,100,10))) +
    labs(x = "Tournament Stage", y = "% Chance", title = paste0("Advance to KOs (based on ", (n_reps %/% 10) * 10, " simulations)")) +
    coord_cartesian(ylim = c(0,100))

  
  if (error_type == "Error Bars") {
    plot <- plot + geom_errorbar(aes(x = matchday, ymin = advance_perc_avg - advance_perc_sd,
                                     ymax = advance_perc_avg + advance_perc_sd, 
                                     color = country), alpha = 0.5, width = 0.1)
  } else if (error_type == "Shading") {
    plot <- plot + geom_ribbon(aes(x = matchday, ymin = advance_perc_avg - advance_perc_sd,
                                   ymax = advance_perc_avg + advance_perc_sd, 
                                   fill = country), alpha = 0.1) +
                                   scale_fill_manual(values = color_list)
  }
  
  
  
  plotly_return <- ggplotly(plot,
           tooltip = c("x", "y", "colour", "plus_minus"))
  
  plotly_return <- layout(plotly_return, xaxis = list(title = "Tournament Stage"), yaxis = list(title = "% Chance"),
                          title = paste0("Advance to KOs (based on ", (n_reps %/% 10) * 10, " simulations)"),
                          margin = list(l = 50, b =50, r = 50, t = 80))
  
  return(plotly_return)
}

create_plotly_group_win_chance <- function(prediction_df, n_reps, error_type = "No") {
  team_list <- unique(prediction_df$country)
  color_list <- colors[team_list,]
  
  plot <- ggplot(data = prediction_df) +
    geom_point(aes(x = matchday, y = group_win_perc_avg, color = country, plus_minus = group_win_perc_sd)) +
    geom_line(aes(x = matchday, y = group_win_perc_avg, color = country, plus_minus = group_win_perc_sd)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = 0:max(prediction_df$matchday), labels = label_list[1:(max(prediction_df$matchday)+1)]) +
    scale_colour_manual(values = color_list) +
    scale_y_continuous(breaks = (seq(0,100,10))) +
    labs(x = "Tournament Stage", y = "% Chance", title = paste0("Win Group (based on ", (n_reps %/% 10) * 10, " simulations)")) +
    coord_cartesian(ylim = c(0,100))
  
  
  if (error_type == "Error Bars") {
    plot <- plot + geom_errorbar(aes(x = matchday, ymin = group_win_perc_avg - group_win_perc_sd,
                                     ymax = group_win_perc_avg + group_win_perc_sd, 
                                     color = country), alpha = 0.5, width = 0.1)
  } else if (error_type == "Shading") {
    plot <- plot + geom_ribbon(aes(x = matchday, ymin = group_win_perc_avg - group_win_perc_sd,
                                   ymax = group_win_perc_avg + group_win_perc_sd, 
                                   fill = country), alpha = 0.1) +
      scale_fill_manual(values = color_list)
  }
  
  plotly_return <- ggplotly(plot,
                            tooltip = c("x", "y", "colour", "plus_minus"))
  
  plotly_return <- layout(plotly_return, xaxis = list(title = "Tournament Stage"), yaxis = list(title = "% Chance"),
                          title = paste0("Win Group (based on ", (n_reps %/% 10) * 10, " simulations)"),
                          margin = list(l = 50, b =50, r = 50, t = 80))
  
  return(plotly_return)
}

create_plotly_quarters_chance <- function(prediction_df, n_reps, error_type = "No") {
  team_list <- unique(prediction_df$country)
  color_list <- colors[team_list,]
  
  plot <- ggplot(data = prediction_df) +
    geom_point(aes(x = matchday, y = quarters_perc_avg, color = country, plus_minus = quarters_perc_sd)) +
    geom_line(aes(x = matchday, y = quarters_perc_avg, color = country, plus_minus = quarters_perc_sd)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = 0:max(prediction_df$matchday), labels = label_list[1:(max(prediction_df$matchday)+1)]) +
    scale_colour_manual(values = color_list) +
    scale_y_continuous(breaks = (seq(0,100,10))) +
    labs(x = "Tournament Stage", y = "% Chance", title = paste0("Make Quarter Finals (based on ", (n_reps %/% 10) * 10, " simulations)")) +
    coord_cartesian(ylim = c(0,100))
  
  
  
  if (error_type == "Error Bars") {
    plot <- plot + geom_errorbar(aes(x = matchday, ymin = quarters_perc_avg - quarters_perc_sd,
                                     ymax = quarters_perc_avg + quarters_perc_sd, 
                                     color = country), alpha = 0.5, width = 0.1)
  } else if (error_type == "Shading") {
    plot <- plot + geom_ribbon(aes(x = matchday, ymin = quarters_perc_avg - quarters_perc_sd,
                                   ymax = quarters_perc_avg + quarters_perc_sd, 
                                   fill = country), alpha = 0.1) +
      scale_fill_manual(values = color_list)
  }
  
  plotly_return <- ggplotly(plot,
                            tooltip = c("x", "y", "colour", "plus_minus"))
  
  plotly_return <- layout(plotly_return, xaxis = list(title = "Tournament Stage"), yaxis = list(title = "% Chance"),
                          title = paste0("Make Quarter Finals (based on ", (n_reps %/% 10) * 10, " simulations)"),
                          margin = list(l = 50, b =50, r = 50, t = 80))
  
  return(plotly_return)
}

create_plotly_semis_chance <- function(prediction_df, n_reps, error_type  = "No") {
  team_list <- unique(prediction_df$country)
  color_list <- colors[team_list,]
  
  plot <- ggplot(data = prediction_df) +
    geom_point(aes(x = matchday, y = semis_perc_avg, color = country, plus_minus = semis_perc_sd)) +
    geom_line(aes(x = matchday, y = semis_perc_avg, color = country, plus_minus = semis_perc_sd)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = 0:max(prediction_df$matchday), labels = label_list[1:(max(prediction_df$matchday)+1)]) +
    scale_colour_manual(values = color_list) +
    scale_y_continuous(breaks = (seq(0,100,10))) +
    labs(x = "Tournament Stage", y = "% Chance", title = paste0("Make Semi Finals (based on ", (n_reps %/% 10) * 10, " simulations)")) +
    coord_cartesian(ylim = c(0,100))
  
  
  if (error_type == "Error Bars") {
    plot <- plot + geom_errorbar(aes(x = matchday, ymin = semis_perc_avg - semis_perc_sd,
                                     ymax = semis_perc_avg + semis_perc_sd, 
                                     color = country), alpha = 0.5, width = 0.1)
  } else if (error_type == "Shading") {
    plot <- plot + geom_ribbon(aes(x = matchday, ymin = semis_perc_avg - semis_perc_sd,
                                   ymax = semis_perc_avg + semis_perc_sd, 
                                   fill = country), alpha = 0.1) +
      scale_fill_manual(values = color_list)
  }
  
  plotly_return <- ggplotly(plot,
                            tooltip = c("x", "y", "colour", "plus_minus"))
  
  plotly_return <- layout(plotly_return, xaxis = list(title = "Tournament Stage"), yaxis = list(title = "% Chance"),
                          title = paste0("Make Semi Finals (based on ", (n_reps %/% 10) * 10, " simulations)"),
                          margin = list(l = 50, b =50, r = 50, t = 80))
  
  return(plotly_return)
}

create_plotly_finals_chance <- function(prediction_df, n_reps, error_type = "No") {
  team_list <- unique(prediction_df$country)
  color_list <- colors[team_list,]
  
  plot <- ggplot(data = prediction_df) +
    geom_point(aes(x = matchday, y = finals_perc_avg, color = country, plus_minus = finals_perc_sd)) +
    geom_line(aes(x = matchday, y = finals_perc_avg, color = country, plus_minus = finals_perc_sd)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = 0:max(prediction_df$matchday), labels = label_list[1:(max(prediction_df$matchday)+1)]) +
    scale_colour_manual(values = color_list) +
    scale_y_continuous(breaks = (seq(0,100,10))) +
    labs(x = "Tournament Stage", y = "% Chance", title = paste0("Make Final (based on ", (n_reps %/% 10) * 10, " simulations)"))  +
    coord_cartesian(ylim = c(0,100))
  
  
  if (error_type == "Error Bars") {
    plot <- plot + geom_errorbar(aes(x = matchday, ymin = finals_perc_avg - finals_perc_sd,
                                     ymax = finals_perc_avg + finals_perc_sd, 
                                     color = country), alpha = 0.5, width = 0.1)
  } else if (error_type == "Shading") {
    plot <- plot + geom_ribbon(aes(x = matchday, ymin = finals_perc_avg - finals_perc_sd,
                                   ymax = finals_perc_avg + finals_perc_sd, 
                                   fill = country), alpha = 0.1) +
      scale_fill_manual(values = color_list)
  }
  
  plotly_return <- ggplotly(plot,
                            tooltip = c("x", "y", "colour", "plus_minus"))
  
  plotly_return <- layout(plotly_return, xaxis = list(title = "Tournament Stage"), yaxis = list(title = "% Chance"),
                          title = paste0("Make Final (based on ", (n_reps %/% 10) * 10, " simulations)"),
                          margin = list(l = 50, b =50, r = 50, t = 80))
  
  return(plotly_return)
}

create_plotly_champions_chance <- function(prediction_df, n_reps, error_type = "No") {
  team_list <- unique(prediction_df$country)
  color_list <- colors[team_list,]
  
  plot <- ggplot(data = prediction_df) +
    geom_point(aes(x = matchday, y = champions_perc_avg, color = country, plus_minus = champions_perc_sd)) +
    geom_line(aes(x = matchday, y = champions_perc_avg, color = country, plus_minus = champions_perc_sd)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = 0:max(prediction_df$matchday), labels = label_list[1:(max(prediction_df$matchday)+1)]) +
    scale_colour_manual(values = color_list) +
    scale_y_continuous( breaks = (seq(0,100,10))) +
    labs(x = "Tournament Stage", y = "% Chance", title = paste0("Win World Cup (based on ", (n_reps %/% 10) * 10, " simulations)")) +
    coord_cartesian(ylim = c(0,100))
  
  
  
  if (error_type == "Error Bars") {
    plot <- plot + geom_errorbar(aes(x = matchday, ymin = champions_perc_avg - champions_perc_sd,
                                     ymax = champions_perc_avg + champions_perc_sd, 
                                     color = country), alpha = 0.5, width = 0.1)
  } else if (error_type == "Shading") {
    plot <- plot + geom_ribbon(aes(x = matchday, ymin = champions_perc_avg - champions_perc_sd,
                                   ymax = champions_perc_avg + champions_perc_sd, 
                                   fill = country), alpha = 0.1) +
      scale_fill_manual(values = color_list)
  }
  
  plotly_return <- ggplotly(plot,
                            tooltip = c("x", "y", "colour", "plus_minus"))
  
  plotly_return <- layout(plotly_return, xaxis = list(title = "Tournament Stage"), yaxis = list(title = "% Chance"),
                          title = paste0("Win World Cup (based on ", (n_reps %/% 10) * 10, " simulations)"),
                          margin = list(l = 50, b =50, r = 50, t = 80))
  
  return(plotly_return)
}
