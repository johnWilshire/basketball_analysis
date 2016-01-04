library(bild)
library(parallel)

#load import functions
source("R/individual_games_bild_analysis.R")

ball <- read_bball_data("data/basketball_shots_curry.csv")
ball <- data_prepare(ball)
ball <- unique(ball)


get_bild_model_for_player <- function (data, player_id, model_dependence){
  player_data <- ball[ball$player.player_id == player_id,]
  return(bild_model(player_data, dependence = model_dependence, summary = FALSE))
}

get_coefficients_of_models <- function (ball, model_dependence){
  # we exclude ids that have not taken more than 3 events
  rles_of_ids <- rle(sort(ball$id))
  many <- rles_of_ids$values[rles_of_ids$lengths > 3]
  # we need to subset our dataset so that the ids that do not occur enough are removed
  small_ball <- ball[ball$id %in% many,]
  
  # grab the players ids
  players <- unique(lapply(many, function (id){
    small_ball$player.player_id[small_ball$id == id][1]
    }))
  
  # we exclude the players that do not have more than two ids associated with them
  # I was getting some weird error and this fixed it by increasing from 2 (not sure why)
  players <- lapply(players, function (player_id){
    if(length(unique(small_ball$id[small_ball$player.player_id == player_id])) < 3){
      return(NULL)
    }
    return(player_id)
  })
  # filter out the NULL values returned in the list from lapply
  players <- players[!sapply(players, is.null)]
  
  # bild models
  return(
    lapply(players, function (player){
      print(toString(player))
      player_data <- small_ball[small_ball$player.player_id == player,]
      print(player_data[1,])
      return(bild_model(player_data, dependence = model_dependence, summary = FALSE))
  }))
}

models <- get_coefficients_of_models(ball, "ind")
