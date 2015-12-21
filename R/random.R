# this script is to explore bilds performance on a "random datset"

library(bild)

# num ids must divide n with no remainder ~
random_dataset_for_bild <- function (n, num_ids, prob = 0.5){
  success <- rbinom(n, 1, prob)
  events_per_id <- n / num_ids
  id <- as.vector(sapply(1:num_ids,function(x) {rep(x,events_per_id)}))
  counts <- rep(1,n)
  age <- as.vector(sapply(1:num_ids,function(x) {1:events_per_id}))
  return(data.frame(id, success, counts, age))
}

r <- random_dataset_for_bild(100000,100)

bild_ind <- bild(success ~ age, data = r, dependence = "ind", time = "age")
summary(bild_ind)

bild_MC1 <- bild(success ~ age, data = r, dependence = "MC1", time = "age")
summary(bild_MC1)

bild_MC2 <- bild(success ~ age, data = r, dependence = "MC2", time = "age")
summary(bild_MC2)

bild_MC1R <- bild(success ~ age, data = r, dependence = "MC1R", time = "age")
summary(bild_MC1R)

bild_MC2R <- bild(success ~ age, data = r, dependence = "MC2R", time = "age")
summary(bild_MC2R)