#Lee Przybylski
#9/26/2021
library(ggplot2)

#Data Exploration with runs scored data
options(stringsAsFactors = FALSE)
games <- read.csv("MLB2019.csv")
head(games)
games$park <- paste(games$Home, "park", sep = "")
summary(games)
ggplot(games, aes(x= Home, y = Runs_Home)) + geom_boxplot()

#Model in home team runs 
pois1 <- glm(Runs_Home ~ Home + Away, data = games, family = poisson(link = "log"))
summary(pois1)

1-pchisq(deviance(pois1), 2370)
#This shows there is evidence of overdispersion

#Add park as a predictor
pois2 <- glm(Runs_Home ~ Home + Away + park, data = games, family = poisson(link = "log"))
summary(pois2)

#We need to reshape the pdata