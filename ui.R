library(shiny)

ui <- navbarPage(title = "World Cup 2022",
                 tabPanel("Schedule",
                          fluidRow(column(3, textInput("group_pattern",
                                    "Group Search")),
                          column(3, textInput("team_pattern",
                                    "Team Search"))),
                          tableOutput("schedule")),
                 tabPanel("Groups",
                          fluidRow(column(5,  numericInput("predict_group_n",
                                                           "Number of Tournament Simulations",
                                                           min = 10, max = 1000, value = 100)),
                                   column(5, actionButton("simulate_group",
                                                          "Simulate"))),
                          tags$b(tags$i("Group A")), tableOutput("table_A"), tags$hr(),
                          tags$b(tags$i("Group B")), tableOutput("table_B"), tags$hr(),
                          tags$b(tags$i("Group B")), tableOutput("table_C"), tags$hr(),
                          tags$b(tags$i("Group C")), tableOutput("table_D"), tags$hr(),
                          tags$b(tags$i("Group D")), tableOutput("table_E"), tags$hr(),
                          tags$b(tags$i("Group E")), tableOutput("table_F"), tags$hr(),
                          tags$b(tags$i("Group F")), tableOutput("table_G"), tags$hr(),
                          tags$b(tags$i("Group G")), tableOutput("table_H"), tags$hr()),
                 tabPanel("Knockout",
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_49"))),
                          fluidRow(
                            column(3, offset = 3, tableOutput("game_57"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_50")),
                            column(3, offset = 3, tableOutput("game_61a"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_53")),
                            column(3, offset = 3, tableOutput("game_61b"))),
                          fluidRow(
                            column(3, offset = 3, tableOutput("game_58"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_54")),
                            column(3, offset = 6, tableOutput("game_64a"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_51")),
                            column(3, offset = 6, tableOutput("game_64b"))),
                          fluidRow(
                            column(3, offset = 3, tableOutput("game_59"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_52")),
                            column(3, offset = 3, tableOutput("game_62a"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_55")),
                            column(3, offset = 3, tableOutput("game_62b"))),
                          fluidRow(
                            column(3, offset = 3, tableOutput("game_60"))),
                          fluidRow(
                            column(3, offset = 0, tableOutput("game_56")),
                            column(3, offset = 6, tableOutput("game_63"))),
                          ),
                 
                 tabPanel("Stats/Plots/Analysis")
)