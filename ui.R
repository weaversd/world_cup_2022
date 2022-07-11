library(shiny)

ui <- navbarPage(title = "World Cup 2022",
                 tabPanel("Home",
                          tags$b("Welcome! Check out the 'How this Works' tab for some instructions"),
                          h1("Today's Games"),
                          textOutput("today"),
                          hr(),
                          fluidRow(column(4,uiOutput("todays_games_dfs"))),
                          textOutput("no_games_today"),
                          h1("Upcoming Games"),
                          textOutput("next_game_day"),
                          hr(),
                          fluidRow(column(4,uiOutput("future_games_df"))),
                          h2("Date Search"),
                          dateInput("date_search",
                                    "Select a Date"),
                          fluidRow(column(4,uiOutput("seach_games_df"))),
                          textOutput("no_games_today_search")),
                 tabPanel("Schedule",
                          fluidRow(column(3, wellPanel(
                            textInput("group_pattern", "Group Search"),
                            textInput("team_pattern", "Team Search"))),
                            column(4, wellPanel(
                              fileInput("scores_csv", "Upload Scores .csv file",
                                        accept = ".csv"),
                              downloadButton("download_scores", "Download Score Template")
                            )),
                            column(4,(wellPanel(
                              fileInput("elo_csv", "Upload Elo .csv file",
                                        accept = ".csv"),
                              downloadButton("download_elo", "Download Default Elo")
                            )))
                          ),
                          tableOutput("elo"),
                          formattableOutput("schedule")),
                 tabPanel("Groups",
                          fluidRow(column(5,  numericInput("predict_group_n",
                                                           "Number of Tournament Simulations",
                                                           min = 10, max = 1000, value = 100)),
                                   column(5, actionButton("simulate_group",
                                                          "Simulate"))),
                          tags$b(tags$i("Group A")), formattableOutput("table_A"), tags$hr(),
                          tags$b(tags$i("Group B")), formattableOutput("table_B"), tags$hr(),
                          tags$b(tags$i("Group C")), formattableOutput("table_C"), tags$hr(),
                          tags$b(tags$i("Group D")), formattableOutput("table_D"), tags$hr(),
                          tags$b(tags$i("Group E")), formattableOutput("table_E"), tags$hr(),
                          tags$b(tags$i("Group F")), formattableOutput("table_F"), tags$hr(),
                          tags$b(tags$i("Group G")), formattableOutput("table_G"), tags$hr(),
                          tags$b(tags$i("Group H")), formattableOutput("table_H"), tags$hr()),
                 tabPanel("Knockout",
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_49"))),
                          fluidRow(
                            column(3, offset = 3, formattableOutput("game_57"))),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_50")),
                            ),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_53")),
                            column(3, offset = 3, formattableOutput("game_61b"))),
                          fluidRow(
                            column(3, offset = 3, formattableOutput("game_58"))),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_54")),
                            column(3, offset = 6, formattableOutput("game_64a"))),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_51")),
                            ),
                          fluidRow(
                            column(3, offset = 3, formattableOutput("game_59"))),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_52")),
                            column(3, offset = 3, formattableOutput("game_62a"))),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_55")),
                            ),
                          fluidRow(
                            column(3, offset = 3, formattableOutput("game_60"))),
                          fluidRow(
                            column(3, offset = 0, formattableOutput("game_56")),
                            column(3, offset = 6, formattableOutput("game_63"))),
                          ),
                 
                 tabPanel("Stats/Plots/Analysis",
                            fluidRow(column(5, numericInput("predict_overall_n",
                                                             "Number of Tournament Simulations",
                                                             min = 10, max = 1000, value = 100)),
                                     column(5, actionButton("simulate_overall",
                                                            "Simulate"))),
                            fluidRow(tabsetPanel(
                              tabPanel("Predictions by Simulation",
                                       fluidRow(tags$head(
                                         tags$style(
                                           HTML(".shiny-notification {
                                                  height: 100px;
                                                  width: 400px;
                                                  position:fixed;
                                                  font-size: 20px;
                                                  top: calc(13% - 50px);;
                                                  left: calc(90% - 300px);;
                                                  }
                                                "
                                           )
                                         )
                                       ),
                                       column(3, offset = 1, textInput("group_pattern_sim",
                                                           "Group Search")),
                                       column(3, textInput("team_pattern_sim",
                                                           "Team Search")),
                                       column(3, selectInput("error_type",
                                                   "Show StDev?",
                                                   choices = c("No", "Error Bars", "Shading"),
                                                   selected = "No"))),
                                       tags$hr(),
                                       tabsetPanel(
                                         tabPanel("Win World Cup", withSpinner(plotlyOutput("champions_plotly", height = 600)), tags$hr()),
                                         tabPanel("Advance KOs", withSpinner(plotlyOutput("advance_KOs_plotly", height = 600)), tags$hr()),
                                         tabPanel("Win Group", withSpinner(plotlyOutput("win_group_plotly", height = 600)), tags$hr()),
                                         tabPanel("Advance Quarters", withSpinner(plotlyOutput("quarters_plotly", height = 600)), tags$hr()),
                                         tabPanel("Advance Semis", withSpinner(plotlyOutput("semis_plotly", height = 600)), tags$hr()),
                                         tabPanel("Advance Finals", withSpinner(plotlyOutput("finals_plotly", height = 600)), tags$hr()))),
                              tabPanel("Stats", "coming soon")
                            ))),
                 tabPanel("How this works",
                          includeMarkdown("Documentation.md"))
)