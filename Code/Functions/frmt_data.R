#A function for formattig the sportsbook archive data for modeling and 
#Validation testing
options(stringsAsFactors = FALSE)

frmt_data <- function(file, ssn){
  games <- read.csv(file)
  names(games)
  vars <- names(games)[c(1,3:21)]
  games <- games[,vars]
  #Assign year to each game 
  games$year <- ssn
  #Assign id number for each game
  N <- length(games$Date)
  gameID <- rep(1:(N/2), each = 2)
  games$gameID <- gameID
  #tail(games)
  #Get opposing pitcher
  indx <- 0:(N-1)
  odds <- seq(from = 1, to = N , by = 2)
  indx[odds] = indx[odds] + 2
  indx
  games$OppPitcher <- games$Pitcher[indx]
  #Get opposing team
  games$Opp <- games$Team[indx]
  #Get Opposing team score
  games$OppScore <- games$Final[indx]
  #Get win or lose
  games$Win <- ifelse(games$Final > games$OppScore, 1, 0)
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
  modelvars <- c("obsID", "gameID", "Team", "Pitcher", "Opp", "OppPitcher", "Venue", "year",
                 "Final")
  games$home <- ifelse(games$Team == games$Venue, 1,0)
  return(games)
}