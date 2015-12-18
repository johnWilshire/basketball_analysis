# installs the required librarys to run the analysis on katana

# start a session and then source("librarys.R")

install.packages("devtools")

install.packages("bild")
install.packages("plyr")

devtools::install_github("richfitz/storr")
devtools::install_github("richfitz/remake")