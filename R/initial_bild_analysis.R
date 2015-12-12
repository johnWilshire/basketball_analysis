#load the dataset and bild library
library(bild)
ball <- read.csv("data/basketball_shots_curry.csv", sep = "|")

### Preparation 
# remove duplicates rows
ball <- unique(ball)

# change makes shot to numeric vector 1 = TRUE, 0 = FALSE
ball$makes_shot <- as.numeric(ball$makes_shot) - 1

# convert the time and dates to comfy POSIX 
ball$date <-  lapply(ball$date,toString)
ball$date <- strptime(ball$date,"%B %d, %Y")

# there is probably a better way to do this
# a player should not be able to have score events twice in a second so worrying about the 
# tenth of a second isnt really a good idea imho
ball$time <-  lapply(ball$time,toString)
ball$time <- strptime(ball$time,"%M:%S")
ball <- ball[with(ball, order(time, decreasing = TRUE)),]
ball <- ball[with(ball, order(date, quater)),]

curry <- ball[ball$player.player_id == "curryst01",]

### Bild

# we will call this a lot so I made a function for it,
# this way we can easily add / change covariates
# and reevaluate models

bild_model <- function(dataset, dependance){
  return(bild(makes_shot ~ shot_age + quater + score_diff + distance,
               data = dataset,
               trace = TRUE,
               aggregate = "shot_age",
               dependence = dependance,
               time = "shot_age"))
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
  shot_age
}

### Model 1: 
# we only have 1 id'd person only curry
# only with him for now

curry_3ptr_1 <- curry[curry$event_type == "3-pt",]
curry_3ptr_1$id <- 1
# create an age of the shots from the curry
curry_3ptr_1$shot_age <- get_shot_age(curry_3ptr_1)

bild_1_ind <- bild_model(curry_3ptr_1, "ind")
bild_1_MC1 <- bild_model(curry_3ptr_1, "MC1")
bild_1_MC2 <- bild_model(curry_3ptr_1, "MC2")
summary(bild_1_ind)
summary(bild_1_MC1)
summary(bild_1_MC2)
# plot(bild_1_MC2, main="bild_model_1")

### Model 2: 
# each game that curry has played in is given a different id
# each shot in that "bucket" is given a different shot_age in ascending order
curry_3ptr_2 <- curry[curry$event_type == "3-pt",]

# assign ids
curry_3ptr_2$id <- as.numeric(as.factor(curry_3ptr_2$game_id))
# assign shot ages
# this could be done without a for loop but i am not well versed enough in R / functional programming
curry_3ptr_2$shot_age <- get_shot_age(curry_3ptr_2)

# trying to assign a "counts" col in the dataset to determine weights and maybe get around the error
rles <- rle(curry_3ptr_2$id)
for (i in 1:nrow(curry_3ptr_2)){
  curry_3ptr_2$counts[i] <- rles$lengths[rles$values == curry_3ptr_2$id[i]]
}

bild_2_idd <- bild_model(curry_3ptr_2,"ind")
bild_2_MC1 <- bild_model(curry_3ptr_2,"MC1")
bild_2_MC2 <- bild_model(curry_3ptr_2,"MC2")

summary(bild_2_idd)
summary(bild_2_MC1)
summary(bild_2_MC2)
plot(bild_2_MC2, main="bild_model_2")
