
### functions
# this way we can easily add / change covariates
# and reevaluate models
bild_model <-
  function(dataset, dependence, summary = FALSE, time = "shot_age") {
    model <-
      bild(
        makes_shot ~ shot_age + quarter + score_diff + distance,
        data = dataset,
        trace = TRUE,
        aggregate = "shot_age",
        dependence = dependence,
        time = "shot_age"
      )
    if (summary) {
      summary(model)
    }
    return(model)
  }


# builds all models of different dependencess
# "ind", "MC1", "MC1R", "MC2", "MC2R"
bild_all_models <- function (dataset){
  models <- c()
  dependences <- c("ind", "MC1","MC1R", "MC2", "MC2R")
  for (dep in dependences){
    print(paste("model with dependences: ", dep))
    models <- c(models, bild_model(dataset, dep, TRUE))
  }
  names(models) <- dependences
  return (models)
}


# shot age calculates the 'age' of a shot based off of the id
get_shot_age <- function (data) {
  shot_age <- rep(0, nrow(data))
  i <- 0
  id <- 0
  for (event_index in 1:nrow(data)) {
    if (id != data$id[event_index]) {
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
read_bball_data <- function(input_file){
  ball <- read.csv(input_file, sep = "|")
  return(ball)
}

data_prepare <- function(ball){
  # change makes shot to numeric vector 1 = TRUE, 0 = FALSE
  ball$makes_shot <- as.numeric(ball$makes_shot) - 1
  
  # convert the time and dates to comfy POSIX
  ball$date <-  lapply(ball$date, toString)
  ball$date <- strptime(ball$date,"%B %d, %Y")
  
  ball$time <-  lapply(ball$time, toString)
  ball$time <- strptime(ball$time, "%M:%S")
  
  # we do this before sort as we require id's to be next to each other
  # each player in each game gets a different id
  ball$id <-
    as.numeric(as.factor(paste(ball$game_id, ball$player.player_id)))
  ball$shot_age <- get_shot_age(ball)
  
  # calcuates counts needed for bild
  #rles <- rle(ball$id)
  # all id's are given the same counts
  ball$counts <- rep(1,nrow(ball))
  # for (i in 1:nrow(ball)){
  #   ball$counts[i] <- rles$lengths[rles$values == ball$id[i]]
  # }
  
  ball <- ball[with(ball, order(time, decreasing = TRUE)),]
  ball <- ball[with(ball, order(date, quarter)),]
  return(ball)
}

subset_bball_by_year <- function(ball, year = 2005){
  ball_specific_year <- ball[grepl(year, ball$game_id),]
  return(ball_specific_year)
}

save_model <- function(all_models, name = "output.rds"){
  save(all_models, file = name)
}

