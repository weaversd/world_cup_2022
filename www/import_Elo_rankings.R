library(htmltools)
library(rvest)
library(stringi)

Elo_raw <- read_html("https://www.eloratings.net/2022_World_Cup")
Elo_nodes <- html_nodes(Elo_raw, 'subheader')
Elo_text <- html_text(Elo_nodes)

