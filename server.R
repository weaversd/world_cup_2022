library(shiny)
source("global.R")

server <- function(input, output, session) {
  
  
  schedule_import <- reactive({
    if (!is.null(input$scores_csv)) {
      import <- read.table(input$scores_csv$datapath, sep = ",", header = T)
    } else {
      import <- read.table("scores.csv", sep = ",", header = T)
    }
    import <- populate_winner_loser(import)
    import$date <- as.Date(import$date, "%m/%d/%Y")
    import <- add_column(import, "&nbsp" = import$home, .after = "home")
    import <- add_column(import, "&nbsp&nbsp" = import$away, .before = "away")
    return(import)
    })
  
  todays_date <- reactive(Sys.Date())
  output$today <- renderText(paste0("Today's Date: ", as.character(todays_date())))
  todays_games_list <- reactive({
    todays_games <- schedule_import()[schedule_import()$date == todays_date(),]
    if (nrow(todays_games) == 0) {
      return(NULL)
    }
    todays_games$date <- as.character(todays_games$date)
    create_game_display_df_list(todays_games)})
  
  next_game_date <- reactive(min(schedule_import()$date[which( schedule_import()$date > todays_date())]))
  output$next_game_day <- renderText({
    if (todays_date() >= as.Date("2022-12-18")) {
      "No Future Games"
    } else {
      paste0("Next Game Date: ",as.character(next_game_date()))
    }
  })


  output$todays_games_dfs <- renderUI({
    if(length(todays_games_list()) > 0) {
      lapply(1:length(todays_games_list()), function(i){
        if (todays_games_list()[[i]]$score[1] == todays_games_list()[[i]]$score[2]&& !is.na(todays_games_list()[[i]]$score[1])) {
          output[[paste0("todays_game",i)]] <- renderFormattable(formattable(todays_games_list()[[i]],
                                                                             list("&nbsp" = flag_image_tile,
                                                                                  TV = tv_image_tile,
                                                                                  score = color_tile("yellow", "yellow"),
                                                                                  penalties = color_tile("#ff8989", "lightgreen"))))
        } else {
          output[[paste0("todays_game",i)]] <- renderFormattable(formattable(todays_games_list()[[i]],
                                                                             list("&nbsp" = flag_image_tile,
                                                                                  TV = tv_image_tile,
                                                                                  score = color_tile("#ff8989", "lightgreen"))))
        }
        formattableOutput(paste0("todays_game", i))
      })
    } else {
      return(NULL)
    }
  })
  
  future_games_list <- reactive({
    future_games <- schedule_import()[schedule_import()$date == next_game_date(),]
    if (nrow(future_games) == 0) {
      return(NULL)
    }
    future_games$date <- as.character(future_games$date)
    create_game_display_df_list(future_games)
  })
  
  output$future_games_df <- renderUI({
    if(length(future_games_list()) > 0) {
      lapply(1:length(future_games_list()), function(i){
        if (future_games_list()[[i]]$score[1] == future_games_list()[[i]]$score[2]&& !is.na(future_games_list()[[i]]$score[1])) {
          output[[paste0("future_game",i)]] <- renderFormattable(formattable(future_games_list()[[i]],
                                                                             list("&nbsp" = flag_image_tile,
                                                                                  TV = tv_image_tile,
                                                                                  score = color_tile("yellow", "yellow"),
                                                                                  penalties = color_tile("#ff8989", "lightgreen"))))
        } else {
          output[[paste0("future_game",i)]] <- renderFormattable(formattable(future_games_list()[[i]],
                                                                             list("&nbsp" = flag_image_tile,
                                                                                  TV = tv_image_tile,
                                                                                  score = color_tile("#ff8989", "lightgreen"))))
        }
        
       
        formattableOutput(paste0("future_game", i))
      })
    } else {
      return(NULL)
    }
  })
  
  search_games_list <- reactive({
    search_games <- schedule_import()[schedule_import()$date == input$date_search,]
    if (nrow(search_games) == 0) {
      return(NULL)
    }
    search_games$date <- as.character(search_games$date)
    create_game_display_df_list(search_games)
  })
  
  output$seach_games_df <- renderUI({
    if(length(search_games_list()) > 0) {
      lapply(1:length(search_games_list()), function(i){
        if (search_games_list()[[i]]$score[1] == search_games_list()[[i]]$score[2]&& !is.na(search_games_list()[[i]]$score[1])) {
          output[[paste0("search_game",i)]] <- renderFormattable(formattable(search_games_list()[[i]],
                                                                             list("&nbsp" = flag_image_tile,
                                                                                  TV = tv_image_tile,
                                                                                  score = color_tile("yellow", "yellow"),
                                                                                  penalties = color_tile("#ff8989", "lightgreen"))))
        } else {
          output[[paste0("search_game",i)]] <- renderFormattable(formattable(search_games_list()[[i]],
                                                                             list("&nbsp" = flag_image_tile,
                                                                                  TV = tv_image_tile,
                                                                                  score = color_tile("#ff8989", "lightgreen"))))
        }
        formattableOutput(paste0("search_game", i))
      })
    } else {
      return(NULL)
    }
  })
  
  output$no_games_today <- renderText({
    if(is.null(todays_games_list())) {
      return <- "No games scheduled"
    } else {
      return <- NULL
    }
    return(return)
  })
  
  output$no_games_today_search <- renderText({
    if(is.null(search_games_list())) {
      return <- "No games scheduled on that date"
    } else {
      return <- NULL
    }
    return(return)
  })
  
  

  
  scores_template <- reactive(read.table("empty_scores.csv", sep = ",", header = T))
  
  output$download_scores <-  downloadHandler(
    filename = function() {
      paste0("scorestemplate.csv")
    },
    content = function(file) {
      write.csv(scores_template(), file, row.names = F)
    }
  )
  
  elo_default <- reactive(read.table("www/WF_elo_ratings.csv", sep = ",", header = T))
  
  output$download_elo <-  downloadHandler(
    filename = function() {
      paste0("defaultelo.csv")
    },
    content = function(file) {
      write.csv(elo_default(), file, row.names = F)
    }
  )
  
  display_schedule <- reactive({
    temp <- schedule_import()
    temp$date <- as.character(temp$date)
    if (input$team_pattern != "" && input$group_pattern != "") {
      temp <- temp[(grepl(input$team_pattern, temp$home, ignore.case = T) | grepl(input$team_pattern, temp$away, ignore.case = T)) & grepl(toupper(input$group_pattern), temp$group, ignore.case = F) ,]
    } else if (input$team_pattern != "") {
      temp <- temp[(grepl(input$team_pattern, temp$home, ignore.case = T) | grepl(input$team_pattern, temp$away, ignore.case = T)),]
    } else if (input$group_pattern != "") {
      temp <- temp[grepl(toupper(input$group_pattern), temp$group, ignore.case = F) ,]
    } else {
      temp
    }
    temp <- temp %>% relocate(home_score, .after = "&nbsp")
    temp <- temp %>% relocate(away_score, .before = "&nbsp&nbsp")
    return(temp)
  })
  
  elo_import <- reactive({
    if (!is.null(input$elo_csv)) {
      import <- read.table(input$elo_csv$datapath, sep = ",", header = T)
    } else {
      import <- elo_rankings
    }
    return(import)
  })
  
  #output$elo <- renderTable(elo_import())
                              
  teams <- reactive({
    team_list <- unique(c(schedule_import()$home, schedule_import()$away))
    team_list <- team_list[!grepl("[0-9]", team_list)]
    })
  
  
  group_simulation_results <- eventReactive(input$simulate_group, {
    predict_n_tournaments(schedule_import(), input$predict_group_n, elo_import())})
  
  table_list <- reactive({
    if (input$simulate_group) {
      temp <- create_group_tables(schedule_import())
      temp <- populate_percentates_groups(temp, group_simulation_results())
      for (i in length(temp)) {
        temp[[i]] <- temp[[i]][order(-temp[[i]]$Pts, -temp[[i]]$GD, -temp[[i]]$GS, -temp[[i]]$win_group_percent, -temp[[i]]$advance_KO_percent),]
      }
    } else {
      temp <- create_group_tables(schedule_import())
    }
    return(temp)
  })
  
  
  output$schedule <- renderFormattable(formattable(display_schedule(),
                                                   align = c("c","c","c","c","c","c","c","c","r","c","c","c","c","l","c","c","c","c","c"),
                                                   list("&nbsp" = flag_image_tile,
                                                        "&nbsp&nbsp" = flag_image_tile,
                                                        TV = tv_image_tile,
                                                        home_score = formatter("span",
                                                                         style = ~style(display = "block",
                                                                                        padding = "0 4px",
                                                                                        `border-radius` = "4px",
                                                                                        `background-color` = ifelse(winner == home, "lightgreen", ifelse(
                                                                                          loser == home, "#ff8989", ifelse(winner == "Draw", "#89edff", "white")
                                                                                        )))),
                                                        away_score = formatter("span",
                                                                         style = ~style(display = "block",
                                                                                        padding = "0 4px",
                                                                                        `border-radius` = "4px",
                                                                                        `background-color` = ifelse(winner == home, "#ff8989", ifelse(
                                                                                          loser == home, "lightgreen", ifelse(winner == "Draw", "#89edff", "white")
                                                                                        )))),
                                                        
                                                        home = formatter("span",
                                                                         style = ~style(display = "block",
                                                                                        padding = "0 4px",
                                                                                        `border-radius` = "4px",
                                                                                        `background-color` = ifelse(winner == home, "lightgreen", ifelse(
                                                                                          loser == home, "#ff8989", ifelse(winner == "Draw", "#89edff", "white")
                                                                                        )))),
                                                        away = formatter("span",
                                                                         style = ~style(display = "block",
                                                                                        padding = "0 4px",
                                                                                        `border-radius` = "4px",
                                                                                        `background-color` = ifelse(winner == home, "#ff8989", ifelse(
                                                                                          loser == home, "lightgreen", ifelse(winner == "Draw", "#89edff", "white")
                                                                                        )))))))
  
  
  
  lapply(LETTERS[1:8], function(i){ 
    output[[paste0("table_",i)]] <- renderFormattable({
      formattable(table_list()[[i]],
                  list("&nbsp" = flag_image_tile,
                       Pts = color_tile("transparent", "#89edff"),
                       W = color_tile("transparent", "lightgreen"),
                       L = color_tile("transparent", "#ff8989"),
                       D = color_tile("transparent", "yellow"),
                       GS = color_tile("transparent", "lightgreen"),
                       GA = color_tile("transparent", "#ff8989"),
                       GD = color_tile("#ff8989", "lightgreen"),
                       win_group_percent = color_bar("#ffa8a8", fun = function(x) x/100),
                       advance_KO_percent = color_bar("#ffdaa8", fun = function(x) x/100),
                       quarters_percent = color_bar("#f4ffa8", fun = function(x) x/100),
                       semis_percent = color_bar("#a8ffa9", fun = function(x) x/100),
                       finals_percent = color_bar("#a8deff", fun = function(x) x/100),
                       champions_percent = color_bar("#d4a8ff", fun = function(x) x/100)))
    })
  })
  
  prediction_df <- eventReactive(input$simulate_overall, {
    progress <- shiny::Progress$new(style = "notification")
    progress$set(message = "Simulating", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 10
      }
      progress$set(value = value, detail = detail)
    }
    
    
    
    create_time_course_predictions(schedule_import(),
                                   input$predict_overall_n, elo_import(),
                                   updateProgress)})
  
  
  display_prediction <- reactive({
    if (input$team_pattern_sim != "" && input$group_pattern_sim != "") {
      prediction_df()[(grepl(input$team_pattern_sim, prediction_df()$country, ignore.case = T)) & grepl(toupper(input$group_pattern_sim), prediction_df()$group, ignore.case = F) ,]
    } else if (input$team_pattern_sim != "") {
      prediction_df()[(grepl(input$team_pattern_sim, prediction_df()$country, ignore.case = T)),]
    } else if (input$group_pattern_sim != "") {
      prediction_df()[grepl(toupper(input$group_pattern_sim), prediction_df()$group, ignore.case = F) ,]
    } else {
      prediction_df()
    }
  })
  
  
  output$advance_KOs_plotly <- renderPlotly(create_plotly_KO_chance(display_prediction(), input$predict_overall_n, input$error_type))
  output$champions_plotly <- renderPlotly(create_plotly_champions_chance(display_prediction(), input$predict_overall_n, input$error_type))
  output$finals_plotly <- renderPlotly(create_plotly_finals_chance(display_prediction(), input$predict_overall_n, input$error_type))
  output$semis_plotly <- renderPlotly(create_plotly_semis_chance(display_prediction(), input$predict_overall_n, input$error_type))
  output$quarters_plotly <- renderPlotly(create_plotly_quarters_chance(display_prediction(), input$predict_overall_n, input$error_type))
  output$win_group_plotly <- renderPlotly(create_plotly_group_win_chance(display_prediction(), input$predict_overall_n, input$error_type))
  
  
  
  
  goal_plot_df <- reactive(create_goal_stat_df(schedule_import()))
  output$GF_plot <- renderPlotly(create_goals_for_plotly(goal_plot_df()))
  output$GA_plot <- renderPlotly(create_goals_against_plotly(goal_plot_df()))
  output$GD_plot <- renderPlotly(create_goal_dif_plotly(goal_plot_df()))
  
  
  
  #knockout games
  knockout_display <- reactive({
    temp <- schedule_import()
    temp$date <- as.character(temp$date)
    return(temp)})
  
  game49_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 49,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_49 <- renderFormattable({
    if (game49_df()$score[1] == game49_df()$score[2] && !is.na(game49_df()$score[1])) {
      formattable(game49_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game49_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game57_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 57,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_57 <- renderFormattable({
    if (game57_df()$score[1] == game57_df()$score[2]&& !is.na(game57_df()$score[1])) {
      formattable(game57_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game57_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game50_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 50,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_50 <- renderFormattable({
    if (game50_df()$score[1] == game50_df()$score[2]&& !is.na(game50_df()$score[1])) {
      formattable(game50_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game50_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game53_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 53,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_53 <- renderFormattable({
    if (game53_df()$score[1] == game53_df()$score[2]&& !is.na(game53_df()$score[1])) {
      formattable(game53_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game53_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
      
  
  game58_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 58,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_58 <- renderFormattable({
    if (game58_df()$score[1] == game58_df()$score[2]&& !is.na(game58_df()$score[1])) {
      formattable(game58_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game58_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game54_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 54,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_54 <- renderFormattable({
    if (game54_df()$score[1] == game54_df()$score[2]&& !is.na(game54_df()$score[1])) {
      formattable(game54_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game54_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game51_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 51,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_51 <- renderFormattable({
    if (game51_df()$score[1] == game51_df()$score[2]&& !is.na(game51_df()$score[1])) {
      formattable(game51_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game51_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game59_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 59,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_59 <- renderFormattable({
    if (game59_df()$score[1] == game59_df()$score[2]&& !is.na(game59_df()$score[1])) {
      formattable(game59_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game59_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game52_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 52,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_52 <- renderFormattable({
    if (game52_df()$score[1] == game52_df()$score[2]&& !is.na(game52_df()$score[1])) {
      formattable(game52_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game52_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game55_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 55,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_55 <- renderFormattable({
    if (game55_df()$score[1] == game55_df()$score[2]&& !is.na(game55_df()$score[1])) {
      formattable(game55_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game55_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game60_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 60,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_60 <- renderFormattable({
    if (game60_df()$score[1] == game60_df()$score[2]&& !is.na(game60_df()$score[1])) {
      formattable(game60_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game60_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game56_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 56,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_56 <- renderFormattable({
    if (game56_df()$score[1] == game56_df()$score[2]&& !is.na(game56_df()$score[1])) {
      formattable(game56_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game56_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game61_dfb <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 61,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_61b <- renderFormattable({
    if (game61_dfb()$score[1] == game61_dfb()$score[2]&& !is.na(game61_dfb()$score[1])) {
      formattable(game61_dfb(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game61_dfb(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  game62_dfa <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 62,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_62a <- renderFormattable({
    if (game62_dfa()$score[1] == game62_dfa()$score[2]&& !is.na(game62_dfa()$score[1])) {
      formattable(game62_dfa(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game62_dfa(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })

  
  game64_dfa <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 64,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_64a <- renderFormattable({
    if (game64_dfa()$score[1] == game64_dfa()$score[2]&& !is.na(game64_dfa()$score[1])) {
      formattable(game64_dfa(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game64_dfa(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
  
  
  game63_df <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 63,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams,
                         score = scores)
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties
    }
    return <- add_column(return, "&nbsp" = teams, .after = "TV")
    return(return)
  })
  output$game_63 <- renderFormattable({
    if (game63_df()$score[1] == game63_df()$score[2]&& !is.na(game63_df()$score[1])) {
      formattable(game63_df(),
                  list(TV = tv_image_tile,
                       score = color_tile("yellow", "yellow"),
                       "&nbsp" = flag_image_tile,
                       penalties = color_tile("#ff8989", "lightgreen")))
    } else {
      formattable(game63_df(),
                  list(TV = tv_image_tile,
                       "&nbsp" = flag_image_tile,
                       score = color_tile("#ff8989", "lightgreen")))
    }
  })
}
