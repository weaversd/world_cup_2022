library(shiny)

ui <- navbarPage(title = "World Cup 2022",
                 tabPanel("Schedule",
                          tableOutput("schedule"),
                          tableOutput("elo")),
                 tabPanel("Groups",
                          tags$b(tags$i("Group A")), tableOutput("table_A"), tags$hr(),
                          tags$b(tags$i("Group B")), tableOutput("table_B"), tags$hr(),
                          tags$b(tags$i("Group B")), tableOutput("table_C"), tags$hr(),
                          tags$b(tags$i("Group C")), tableOutput("table_D"), tags$hr(),
                          tags$b(tags$i("Group D")), tableOutput("table_E"), tags$hr(),
                          tags$b(tags$i("Group E")), tableOutput("table_F"), tags$hr(),
                          tags$b(tags$i("Group F")), tableOutput("table_G"), tags$hr(),
                          tags$b(tags$i("Group G")), tableOutput("table_H"), tags$hr()),
                 tabPanel("Knockout"),
                 tabPanel("Stats/Plots/Analysis")
)