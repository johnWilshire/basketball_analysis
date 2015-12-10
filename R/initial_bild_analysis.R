#load the dataset and bild library
library(bild)
ball <- read.csv("data/basketball_shots_curry.csv", sep = '|')

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

# curry is a beast
# this is only with him for now
curry <- ball[ball$player.player_id == 'curryst01',]
curry3ptr <- curry[curry$event_type == "3-pt",]
curry3ptr$id <- 1
# create an age of the shots from the curry
curry3ptr$shot_age <- 1:nrow(curry3ptr)

# age is not a sub multiple of the number of rows ? error ?
curry3ptr <- curry3ptr[1:(nrow(curry3ptr) - 4 ),]

### BILD
bild_model <- bild(makes_shot ~ shot_age + quater + score_diff +distance, 
                   data = curry3ptr, 
                   trace = TRUE,
                   time="shot_age")
summary(bild_model)


