#Lee Przybylski
#9/26/2021
#library(ggplot2)
#library(plyr)

#Data Exploration with runs scored data
options(stringsAsFactors = FALSE)
games <- read.csv("Data/RawData/mlbodds2021.csv")
head(games)
summary(games)
names(games)
vars <- names(games)[c(1,3:21)]
games <- games[,vars]
head(games)

#Assign year to each game 
games$year <- 2021

#Assign id number for each game
N <- length(games$Date)
gameID <- rep(1:(N/2), each = 2)
games$gameID <- gameID
tail(games)

#Get opposing pitcher
indx <- 0:(N-1)
odds <- seq(from = 1, to = N , by = 2)
indx[odds] = indx[odds] + 2
indx
games$OppPitcher <- games$Pitcher[indx]
head(games)

#Get opposing team
games$Opp <- games$Team[indx]
head(games)
#Get Opposing team score
games$OppScore <- games$Final[indx]
#Get win or lose
games$Win <- ifelse(games$Final > games$OppScore, 1, 0)
head(games)

#Get venues
venue <- games$Team
for (j in 1:N){
  if (games$VH[j] == "V"){
    venue[j] = games$Team[j+1]
  }
}
games$Venue <- venue

#Unique marker for ID and team
games$obsID <- paste(games$Team, games$gameID, sep = "")

#Maybe add a sum of the first 5 innings for starting pitchers
#Maybe add dates
