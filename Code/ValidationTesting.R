#Lee Przybylski 
#11/16/2021

#Perform validation testing for favorite models by training model for an initial 
#interval then testing predictions on the next two weeks

library(lme4)

source("DataFormating.R")

unique(games$Date)
head(games)
modelvars <- c("obsID", "gameID", "Team", "Pitcher", "Opp", "OppPitcher", "Venue", "year",
               "Final")
df <- games[,modelvars]
df$gameID <- as.factor(df$gameID)
df$home <- ifelse(df$Team == df$Venue, 1, 0)
head(df)
games$home <- ifelse(games$Team == games$Venue, 1,0)
head(games)

#Train on all games before date0
#Model did not converge well until 5/24

step <- 0.05
cuts <- seq(from = 0.15, to = 1, by = step)
cuts <- quantile(games$Date, cuts)
J <- length(cuts) -1
for (j in 1:J){
  #Start with 15% of the season and increase trainings at 5-10% intervals
  date0 <- cuts[j]
  datef <- cuts[j+1]

  #Train on all games before date0
  dftrn <- games[games$Date <= date0,]
  indx_val <- (games$Date > date0) & (games$Date <= datef)
  dfval <- games[indx_val,]
  #pct of season observed
  obsrate <- max(dftrn$gameID)/max(games$gameID)
  #Train basic glm
  poiss1_ <- glm(Final ~ Team + Opp + Venue + home, data = dftrn, family = poisson(link= "log"))
  #Make team and venue random
  poiss2_ <- glmer(Final ~ home + (1| Team) + (1| Opp) + (1|Venue), data = dftrn, 
                   family = poisson(link= "log"))
  #Train glmem
  poiss3_ <- glmer(Final ~ home + (1| Team) + (1| Opp) + (1|Venue) + (1|OppPitcher) 
                   + (1|gameID) + (1| obsID), data = dftrn, family = poisson(), 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  #Predict on all games between date0 and datef
  yhat1 <- predict(poiss1_, newdata = dfval, type = "response")
  yhat2 <- predict(poiss3_, re.form = ~ (1| Team) + (1| Opp) + (1|Venue), 
                   newdata = dfval, allow.new.levels = TRUE,
                   type = "response")
  yhat3 <- predict(poiss3_, re.form = ~ (1| Team) + (1| Opp) + (1|Venue)
                  + (1|OppPitcher), newdata = dfval, allow.new.levels = TRUE,
                  type = "response")
  dfval$pred1 <- yhat1
  dfval$pred2 <- yhat2
  dfval$pred3 <- yhat3
  #Collect residuals
  y <- dfval$Final
  res1 <- y - yhat1
  res2 <- y - yhat2
  res3 <- y - yhat3
  #Compute accuracy scores
  N <- length(dfval$Date)
  #Get opposing pitcher
  indx <- 0:(N-1)
  odds <- seq(from = 1, to = N , by = 2)
  indx[odds] = indx[odds] + 2
  #indx
  #Compare Opposing team predicted score to predicted score
  wpred1 <- ifelse(dfval$pred1 > dfval$pred1[indx], 1, 0) 
  acc1 <- sum(wpred1 == dfval$Win)/length(dfval$Win)
  #Compare Opposing team predicted score to predicted score
  wpred2 <- ifelse(dfval$pred2 > dfval$pred2[indx], 1, 0) 
  acc2 <- sum(wpred2 == dfval$Win)/length(dfval$Win)
  #Compare Opposing team predicted score to predicted score
  wpred3 <- ifelse(dfval$pred3 > dfval$pred3[indx], 1, 0) 
  acc3 <- sum(wpred3 == dfval$Win)/length(dfval$Win)
  #Performance of the moneyline
  accOpen <- sum((dfval$Win > 0) & (dfval$Open < 0))/N
  accClose <- sum((dfval$Win > 0) & (dfval$Close < 0))/N

  #Get win or lose
  games$Win <- ifelse(games$Final > games$OppScore, 1, 0)
  head(games)
  scores <- c(N, obsrate, mean(abs(res1)), mean(abs(res2)), mean(abs(res3)),
              sqrt(mean(res1^2)), sqrt(mean(res2^2)), sqrt(mean(res3^2)),
              acc1, acc2, acc3, accOpen, accClose)
}