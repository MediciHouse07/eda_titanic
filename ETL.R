library(dplyr)
library(ggplot2)

gender <- read.csv("data/gender_submission.csv")
train <- read.csv("data/train.csv")

#d <- ggplot(mpg, aes(fl))
#d + geom_bar() 