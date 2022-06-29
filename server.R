library(shiny)
source("global.R")

server <- function(input, output, session) {
  
  schedule_import <- reactive({
    import <- read.table("scores.csv", sep = ",", header = T)
    import <- populate_winner_loser(import)
    return(import)
    })
  
  elo_import <- reactive({
    elo_rankings
  })
                              
  teams <- reactive({
    team_list <- unique(c(schedule_import()$home, schedule_import()$away))
    team_list <- team_list[!grepl("[0-9]", team_list)]
    })
  
  table_list <- reactive({
    create_group_tables(schedule_import())
  })

  output$schedule <- renderTable(schedule_import())
  output$elo1 <- renderText(predict_game("Qatar", "USA"))
  output$elo <- renderTable(elo_import())
  
  
  
  lapply(LETTERS[1:8], function(i){ 
    output[[paste0("table_",i)]] <- renderTable({
      table_list()[[i]]
    })
  })
}
