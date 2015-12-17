#load the dataset and bild library
library(bild)


### functions 

# this way we can easily add / change covariates
# and reevaluate models
bild_model <- function(dataset, dependance, summary = FALSE, time = "shot_age"){
  model <- bild(makes_shot ~ shot_age + quarter + score_diff + distance,
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


ball <- read.csv("data/basketball_shots_curry.csv", sep = "|")
ball <- unique(ball)

# change makes shot to numeric vector 1 = TRUE, 0 = FALSE
ball$makes_shot <- as.numeric(ball$makes_shot) - 1

# convert the time and dates to comfy POSIX
ball$date <-  lapply(ball$date, toString)
ball$date <- strptime(ball$date,"%B %d, %Y")


# there should not be two different score
# events within a tenth of a second
ball$time <-  lapply(ball$time, toString)
ball$time <- strptime(ball$time, "%M:%S")
ball <- ball[with(ball, order(time, decreasing = TRUE)),]
ball <- ball[with(ball, order(date, quarter)),]

curry <- ball[ball$player.player_id == "curryst01",]
### Model 1: 
# we only have 1 id'd person only curry
# only with him for now

curry_3ptr_1 <- curry[curry$event_type == "3-pt",]
curry_3ptr_1$id <- 1

# for the bild time
curry_3ptr_1$shot_age <- get_shot_age(curry_3ptr_1)

# this takes some time
# there is an issue with the optimising of the log likeihood
# on random intercepts
bild_1_ind <- bild_model(curry_3ptr_1, "ind")
bild_1_MC1 <- bild_model(curry_3ptr_1, "MC1")
# bild_1_MC1R <- bild_model(curry_3ptr_1, "MC1R")
bild_1_MC2 <- bild_model(curry_3ptr_1, "MC2")
# bild_1_MC2R <- bild_model(curry_3ptr_1,"MC2R")

summary(bild_1_ind)
summary(bild_1_MC1)
# summary(bild_1_MC1R)
summary(bild_1_MC2)
# summary(bild_1_MC1R)

# plot(bild_1_MC2, main="bild_model_1")

### Model 2: 
# each game that curry has played in is given a different id
# each shot in that "bucket" is given a different shot_age in ascending order
curry_3ptr_2 <- curry[curry$event_type == "3-pt",]
# assign ids
curry_3ptr_2$id <- as.numeric(as.factor(curry_3ptr_2$game_id))

curry_3ptr_2$shot_age <- get_shot_age(curry_3ptr_2)

# calcuates counts needed for bild
rles <- rle(curry_3ptr_2$id)
for (i in 1:nrow(curry_3ptr_2)){
  curry_3ptr_2$counts[i] <- rles$lengths[rles$values == curry_3ptr_2$id[i]]
}

bild_2_idd <- bild_model(curry_3ptr_2, "ind")
bild_2_MC1 <- bild_model(curry_3ptr_2, "MC1")
bild_2_MC1R <- bild_model(curry_3ptr_2, "MC1R")
bild_2_MC2 <- bild_model(curry_3ptr_2, "MC2")
bild_2_MC2R <- bild_model(curry_3ptr_2, "MC2R")

summary(bild_2_idd)
summary(bild_2_MC1)
summary(bild_2_MC1R)
summary(bild_2_MC2)
summary(bild_2_MC2R)

# plot(bild_2_MC2, main = "bild_model_2")