#Lee Przybylski 
#11-11-2021

#Fit our 3 favorite glmer models
#Demonstrate prediction functions
source("Code/DataFormatting.R")
library(lme4)

head(games)
modelvars <- c("obsID", "gameID", "Team", "Pitcher", "Opp", "OppPitcher", "Venue", "year",
               "Final")
df <- games[,modelvars]
df$gameID <- as.factor(df$gameID)
df$home <- ifelse(df$Team == df$Venue, 1, 0)
head(df)

#Naive Model
t0 <- Sys.time()
poiss1 <- glm(Final ~ Team + Opp + Venue, data = df, family = poisson(link= "log"))
tf <- Sys.time()
print(tf - t0)
#Fits in less than a second

#Fixed effects for teams and venue
t0 <- Sys.time()
poiss7.5 <- glmer(Final ~ Team + Opp + Venue + home + (1|OppPitcher) + (1|gameID),
                  data = df, family = poisson(link= "log"), 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf - t0)
#Takes about 26 minutes to fit

#All random except home
t0 <- Sys.time()
poiss3 <- glmer(Final ~ home + (1| Team) + (1| Opp) + (1|Venue) + (1|OppPitcher) 
                + (1|gameID) + (1| obsID), data = df, family = poisson())
tf <- Sys.time()
print(tf - t0) #fits in 25 seconds

summary(poiss3)


head(df)

#Choose a game to predicton
sampledf <- df[df$gameID == 150,]

#Try to understand how predict works with poiss3
sampledf$pred1 <- predict(poiss3, newdata = sampledf, type = "response")
sampledf
#intercept
mu <- unlist(fixef(poiss3)[1])
chi <- unlist(fixef(poiss3)[2])
geff <- ranef(poiss3)$gameID[150,1]
v <- ranef(poiss3)$Venue["LAD", 1]

#For COL 
eff <- ranef(poiss3)$obsID["COL150", 1]
b <- ranef(poiss3)$Team["COL", 1]
f <- ranef(poiss3)$Opp["LAD", 1]
p <- ranef(poiss3)$OppPitcher["TBAUER", 1]
lambda <- mu  + geff + v + eff + b + f + p
exp(lambda)
#For LAD 
eff <- ranef(poiss3)$obsID["LAD150", 1]
b <- ranef(poiss3)$Team["LAD", 1]
f <- ranef(poiss3)$Opp["COL", 1]
p <- ranef(poiss3)$OppPitcher["ASENZATEL", 1]
lambda <- mu + chi + geff + v + eff + b + f + p
exp(lambda)

#If we want to predict without gameID and obsID
#Predictions with conditioning only on the pitcher
yhat2 <- predict(poiss3, re.form = ~ (1| Team) + (1| Opp) + (1|Venue)
                 + (1|OppPitcher), newdata = sampledf, type = "response")
sampledf$pred2 <- yhat2
sampledf
#Predictions by hand
#For COL 
b <- ranef(poiss3)$Team["COL", 1]
f <- ranef(poiss3)$Opp["LAD", 1]
p <- ranef(poiss3)$OppPitcher["TBAUER", 1]
lambda <- mu  + v + b + f + p
exp(lambda)
#For LAD 
b <- ranef(poiss3)$Team["LAD", 1]
f <- ranef(poiss3)$Opp["COL", 1]
p <- ranef(poiss3)$OppPitcher["ASENZATEL", 1]
lambda <- mu + chi + v + b + f + p
exp(lambda)
#Everything matches, Colorado got better when we removed the obsID effect, 
#Which suggests they had a significantly below average game

#If we add a new pitcher, then we still want to get a prediction
sampledf$OppPitcher[1] <- "LPRZYBYLSKI"
sampledf
pred3 <- predict(poiss3, re.form = ~ (1| Team) + (1| Opp) + (1|Venue)
                 + (1|OppPitcher), newdata = sampledf, allow.new.levels = TRUE,
                 type = "response")
sampledf$pred3 <- pred3
#How would COL do with no conditioning on pitcher
b <- ranef(poiss3)$Team["COL", 1]
f <- ranef(poiss3)$Opp["LAD", 1]
lambda <- mu  + v + b + f 
exp(lambda)
