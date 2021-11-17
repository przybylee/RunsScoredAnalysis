#Lee Przybylski
#11/16/2021

#Create a dataframe of pitchers with ERA, WAR, FIP from Fangraphs, and model estimates from our favorite model.
#Fangraphs data selects starting pitchers with a min of 30 IP
library(stringr)
library(DataCombine)

source("Code/Functions/frmt_data.R")

season <- 2021
file <- paste("Data/BaseballProspectus/bpexp", season, ".csv", sep = "")
pitchers <- read.csv(file, header = TRUE, sep = ",")
head(pitchers)
names(pitchers)[1] <- "bpid"

#Read in data from the season and fit the model
gamefile <- paste("DAta/RawData/mlbodds", season, ".csv", sep = "")
dfgames <- frmt_data(gamefile, season)
head(dfgames)

#Convert team names in pitchers to match games
tms1 <- unique(pitchers$Team)
tms2 <- unique(dfgames$Team)
from <- sort(tms1[!(tms1 %in% tms2)])
to <- c("", sort(tms2[!(tms2 %in% tms1)]))
dfreplace <- data.frame(cbind(from, to))
pitchers <- FindReplace(pitchers, Var = "Team", replaceData = dfreplace)

#Fit the model to the games of chosen season
poiss3 <- glmer(Final ~ home + (1| Team) + (1| Opp) + (1|Venue) + (1|OppPitcher) 
                + (1|gameID) + (1| obsID), data = dfgames, family = poisson())

#We have 3 more steps
#1. Extract blups for OppPitcher from poiss3
#2. Match blups to all possible pitcher on pitchers data
#3. Create dataframe with blups and drop the cases where blups were not available 

#Pitcher names are coded as first initial, last name, and -L if lefthanded
split_names <- str_split(pitchers$Name, " ", simplify = TRUE)
finit <- substring(split_names[,1], 1, 1)
codenames <- toupper(paste(finit, split_names[,2], sep = ""))
eff_names <- rownames(ranef(poiss3)$OppPitcher)
ind <- codenames %in% eff_names
sum(ind)
codenames[ind]

codenames_L <- paste(codenames, "-L", sep = "")
ind_L <- codenames_L %in% eff_names

codenames_R <- paste(codenames, "-R", sep = "")
ind_R <- codenames_R %in% eff_names

pitchers$idMatches <- as.numeric(ind) + as.numeric(ind_L) + as.numeric(ind_R)


#At this point, we select pitchers that have one of the codenames
ind_tot <- ind | ind_L | ind_R 
sum(ind_tot)

pitchers_m <- pitchers[pitchers$idMatches == 1,]
codenames

both <- ind_L & ind
codenames[both]
pitchers[both,]
#For pitchers in both, we distinguish them based on team

ind_L <- ind_L & !ind
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