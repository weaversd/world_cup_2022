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
    return(import)
    })
  
  todays_date <- reactive(Sys.Date())
  #todays_date <- reactive(as.Date("2022-12-19"))
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
        output[[paste0("todays_game",i)]] <- renderTable(todays_games_list()[[i]])
        tableOutput(paste0("todays_game", i))
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
        output[[paste0("future_game",i)]] <- renderTable(future_games_list()[[i]])
        tableOutput(paste0("future_game", i))
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
        output[[paste0("search_game",i)]] <- renderTable(search_games_list()[[i]])
        tableOutput(paste0("search_game", i))
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
      return(temp)
    } else {
      create_group_tables(schedule_import())
    }
  })
  
  
  output$schedule <- renderTable(display_schedule())
  
  
  
  lapply(LETTERS[1:8], function(i){ 
    output[[paste0("table_",i)]] <- renderTable({
      table_list()[[i]]
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
    return(return)
  })
  output$game_49 <- renderTable(game49_df())

  
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
    return(return)
  })
  output$game_57 <- renderTable(game57_df())
  
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
    return(return)
  })
  output$game_50 <- renderTable(game50_df())
  
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
    return(return)
  })
  output$game_53 <- renderTable(game53_df())
  
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
    return(return)
  })
  output$game_58 <- renderTable(game58_df())
  
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
    return(return)
  })
  output$game_54 <- renderTable(game54_df())
  
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
    return(return)
  })
  output$game_51 <- renderTable(game51_df())
  
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
    return(return)
  })
  output$game_59 <- renderTable(game59_df())
  
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
    return(return)
  })
  output$game_52 <- renderTable(game52_df())
  
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
    return(return)
  })
  output$game_55 <- renderTable(game55_df())
  
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
    return(return)
  })
  output$game_60 <- renderTable(game60_df())
  
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
    return(return)
  })
  output$game_56 <- renderTable(game56_df())
  
  game61_dfa <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 61,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams[1],
                         score = scores[1])
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties[1]
    }
    return(return)
  })
  output$game_61a <- renderTable(game61_dfa())
  game61_dfb <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 61,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams[2],
                         score = scores[2])
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties[2]
    }
    return(return)
  })
  output$game_61b <- renderTable(game61_dfb())
  
  game62_dfa <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 62,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams[1],
                         score = scores[1])
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties[1]
    }
    return(return)
  })
  output$game_62a <- renderTable(game62_dfa())
  game62_dfb <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 62,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams[2],
                         score = scores[2])
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties[2]
    }
    return(return)
  })
  output$game_62b <- renderTable(game62_dfb())
  
  game64_dfa <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 64,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams[1],
                         score = scores[1])
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties[1]
    }
    return(return)
  })
  output$game_64a <- renderTable(game64_dfa())
  game64_dfb <- reactive({
    row <- knockout_display()[knockout_display()$game_n == 64,]
    teams <- c(row$home[1], row$away[1])
    scores <- c(row$home_score[1], row$away_score[1])
    penalties <- c(row$home_penalty[1], row$away_penalty[1])
    return <- data.frame(round = row$group[1],
                         date = row$date[1],
                         TV = row$TV[1],
                         country = teams[2],
                         score = scores[2])
    if(scores[1] ==  scores[2] && !is.na(scores[1])) {
      return$penalties <- penalties[2]
    }
    return(return)
  })
  output$game_64b <- renderTable(game64_dfb())
  
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
    return(return)
  })
  output$game_63 <- renderTable(game63_df())
}
