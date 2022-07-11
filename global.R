#packages
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(shinycssloaders)
library(markdown)
library(plotly)
library(knitr)
library(formattable)
library(base64enc)
library(tibble)

#functions
source("www/functions.R")
source("www/predict_functions.R")
source("www/plots.R")


elo_rankings <- read.table("www/WF_elo_ratings.csv", sep = ",", header = T)
team_list <- elo_rankings$Country
goal_probabilities <- read.csv("www/goal_number_probabilities.csv", sep = ",",
                               header = T)
