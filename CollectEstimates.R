#Lee Przybylski 
#11-11-2021

#Fit our favorite glmer model and extract estimates for the pitcher
source("Code/DataFormatting.R")
library(lme4)

head(games)
modelvars <- c("obsID", "gameID", "Team", "Pitcher", "Opp", "OppPitcher", "Venue", "year",
               "Final")
df <- games[,modelvars]
df$gameID <- as.factor(df$gameID)
df$home <- ifelse(df$Team == df$Venue, 1, 0)
head(df)

#Fit the model, takes about 23 minutes
t0 <- Sys.time()
poiss7 <- glmer(Final ~ Team + Opp + Venue + (1|OppPitcher) + (1|gameID),
                data = df, family = poisson(link= "log"), 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf - t0)

#Same thing with homefield advantage
t0 <- Sys.time()
poiss7.5 <- glmer(Final ~ Team + Opp + Venue + home + (1|OppPitcher) + (1|gameID),
                  data = df, family = poisson(link= "log"), 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf - t0)
#Takes about 26 minutes to fit

anova(poiss7,poiss7.5)
#using a likelihood ratio test, gives a p-value of 0.006, home effects seem significant.
fixef(poiss7.5)

head(df)
#add predictions with all info
df1 <- df
yhat1 <- predict(poiss7.5, type = "response")
df1$pred1 <- yhat1
head(df1)

#Drop game number and make predictions
df2 <- df
keep <- names(df2)[names(df2) != "OppPitcher"]
df2 <- df2[,keep] 
#Predictions with conditioning only on the pitcher
yhat2 <- predict(poiss7.5, re.form = ~ (1|OppPitcher), type = "response")
df1$pred2 <- yhat2
head(df1)
#predictions with no conditioning
yhat3 <- predict(poiss7.5, re.form = ~ 0, type = "response")
df1$pred3 <- yhat3
head(df1)

#When we bet, we must disregard effects from gameID and 
