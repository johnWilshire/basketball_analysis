#load the dataset and bild library
library(bild)

### functions
# this way we can easily add / change covariates
# and reevaluate models
bild_model <- function(dataset, dependance, summary = FALSE, time = "shot_age"){
  model <- bild(makes_shot ~ shot_age + quater + score_diff + distance,
               data = dataset,
               trace = TRUE,
               aggregate = "shot_age",
               dependence = dependance,
               time = "shot_age")
  if (summary){
    summary(model)
  }
  return(model)
}

# shot age calculates the 'age' of a shot based off of the id
get_shot_age <- function (data) {
  shot_age <- rep(0,nrow(data))
  i <- 0
  id <- 0
  for (event_index in 1:nrow(data)){
    if (id != data$id[event_index]){
      id <- data$id[event_index]
      i <- 0
    } else {
      i <- i + 1
    }
    shot_age[event_index] <- i
  }
  return(shot_age)
}

# i tried fread in data.table but I was getting 
# an issue with the converting times / dates
ball <- read.csv("data/basketball_shots_curry_lebron_harden.csv", sep = "|")
ball <- unique(ball)

# change makes shot to numeric vector 1 = TRUE, 0 = FALSE
ball$makes_shot <- as.numeric(ball$makes_shot) - 1

# convert the time and dates to comfy POSIX
ball$date <-  lapply(ball$date, toString)
ball$date <- strptime(ball$date,"%B %d, %Y")

ball$time <-  lapply(ball$time, toString)
ball$time <- strptime(ball$time, "%M:%S")

# we do this before sort as we require id's to be next to each other
# each player in each game gets a different id
ball$id <- as.numeric(as.factor(paste(ball$game_id, ball$player.player_id)))
ball$shot_age <- get_shot_age(ball)

# calcuates counts needed for bild
# TODO find a better way to do this 
rles <- rle(ball$id)
# all id's are given the same counts
ball$counts <- rep(1,nrow(ball))
# for (i in 1:nrow(ball)){
#   ball$counts[i] <- rles$lengths[rles$values == ball$id[i]]
# }

ball <- ball[with(ball, order(time, decreasing = TRUE)),]
ball <- ball[with(ball, order(date, quater)),]

# this takes ar really long time
bild_2_idd <- bild_model(ball, "ind")
# bild_2_MC1 <- bild_model(ball, "MC1")
# bild_2_MC1R <- bild_model(ball, "MC1R")
# bild_2_MC2 <- bild_model(ball, "MC2")
# bild_2_MC2R <- bild_model(ball, "MC2R")

summary(ball)
# summary(ball)
# summary(ball)
# summary(ball)
# summary(ball)

# plot(bild_2_MC2, main = "bild_model_2")