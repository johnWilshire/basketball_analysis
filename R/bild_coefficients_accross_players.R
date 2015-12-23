library(bild)
library(parallel)

#load import functions
source("R/individual_games_bild_analysis.R")

ball <- read_bball_data("data/basketball_shots_curry.csv")
ball <- data_prepare(ball)



get_bild_model_for_player <- function (data, player_id, model_dependence){
  player_data <- ball[ball$player.player_id == player_id,]
  return(bild_model(player_data, dependence = model_dependence, summary = FALSE))
}

get_coefficients_of_models <- function (ball, model_dependence){
  # we exclude players that have taken less than 10 shots
  # I need a way to filter out players that havent made enough shots to bild a model...
  players <- unique(ball$player.player_id[table(ball$id) > 10])
  lapply(players, function (player){
    print(toString(player))
    player_data <- ball[ball$player.player_id == player,]
    return(bild_model(player_data, dependence = model_dependence, summary = FALSE))
    })
}

models <- get_coefficients_of_models(ball, "ind")
