#Lee Przybylski
#11/16/2021

#Create a dataframe of pitchers with ERA, WAR, FIP from Fangraphs, and model estimates from our favorite model.
#Fangraphs data selects starting pitchers with a min of 30 IP
library(stringr)

season <- 2021
file <- paste("Data/Fangraphs/FGPitching", season, ".csv", sep = "")
pitchers <- read.csv(file, header = TRUE, sep = ",")
head(pitchers)
names(pitchers)[1] <- "Name"

metrics <- c("Name", "Team", "W", "L", "IP", "BABIP", "ERA", "xERA", "FIP", "xFIP", "WAR")
pitchdf <- pitchers[,metrics]
head(pitchdf)

unique(df$OppPitcher)
#Pitcher names are coded as first initial, last name, and -L if lefthanded
split_names <- str_split(pitchdf$Name, " ", simplify = TRUE)
finit <- substring(split_names[,1], 1, 1)
codenames <- toupper(paste(finit, split_names[,2], sep = ""))
ind <- codenames %in% rownames(ranef(poiss7.5)$OppPitcher)
sum(ind)

codenames_L <- paste(codenames, "-L", sep = "")
ind_L <- codenames_L %in% rownames(ranef(poiss7.5)$OppPitcher)
sum(ind_L)

ind_tot <- ind | ind_L
sum(ind_tot)

###############
pitcher_search<- function(string){
  char_min <- stringdist::stringdist(string, team_list)
  indx <- which.min(char_min)
  tm <- team_list[indx][1]
  tm
}