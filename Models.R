#Lee Przybylski 
#11-11-2021

#Fit a generalized linear model or generalized linear mixed effects model
source("DataFormatting.R")
library(lme4)

head(games)
modelvars <- c("obsID", "gameID", "Team", "Pitcher", "Opp", "OppPitcher", "Venue", "year",
               "Final")
df <- games[,modelvars]
df$gameID <- as.factor(df$gameID)
head(df)

#First we consider team effects
poiss1 <- glm(Final ~ Team -1, data = df, family = poisson(link = "log"))
summary(poiss1)
length(poiss1$coefficients)
exp(poiss1$coefficients)
sort(unique(df$Team))
#Boston, Tampa, LA, and Toronto had the highest means

#Check for overdispersion
1-pchisq(deviance(poiss1), df.residual(poiss1))
#Not a good fit according to deviance residuals

#Add effects for the defense and the venue
poiss2 <- glm(Final ~ Team + Opp + Venue, data = df, family = poisson(link= "log"))
summary(poiss2)
#Check for overdispersion
1-pchisq(deviance(poiss2), df.residual(poiss1))
#Not a good fit according to deviance residuals

#Add effects for the opposing starting pitcher
poiss3 <- glm(Final ~ Team + Opp + Venue + OppPitcher,
              data = df, family = poisson(link= "log"))
summary(poiss3)
#Check for overdispersion
1-pchisq(deviance(poiss3), df.residual(poiss3))
#Not a good fit according to deviance residuals

#Make each starting pitcher have a random effect and add a random effect for each
#game.  First as a prereq, do a simple model with team and random effect
t0 <- Sys.time()
poiss4 <- glmer(Final ~ Team + (1|obsID), data = df, 
                family = poisson(link= "log"),
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf-t0)
#summary(poiss4)
#Variance of obsID random effects is 0.2531

#This will take a minute
t0 <- Sys.time()
poiss5 <- glmer(Final ~ Team + Opp + Venue + (1|OppPitcher) + (1|gameID)+ (1|obsID),
              data = df, family = poisson(link= "log"))
summary(poiss5)
tf <- Sys.time()
print(tf - t0)
#Ran for 23 minutes, got 3 warnings


#Go with all random effects
t0 <- Sys.time()
poiss5.5 <- glmer(Final ~ (1|Team) + (1|Opp) + (1|OppPitcher) + (1|Venue)+ 
                  (1|gameID)+ (1|obsID),
                data = df, family = poisson(link= "log"), 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf - t0)
#summary(poiss5.5)
#Fit 6 seconds

#Go with all random effects, including per game
t0 <- Sys.time()
poiss6 <- glmer(Final ~ (1|Team) + (1|Opp) + (1|OppPitcher) + (1|Venue) + (1|gameID),
                data = df, family = poisson(link= "log"), 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf - t0)
#summary(poiss5)
#Fit 6 seconds


#Try a different optimizer to avoid warnings
t0 <- Sys.time()
poiss7 <- glmer(Final ~ Team + Opp + Venue + (1|OppPitcher) + (1|gameID),
                data = df, family = poisson(link= "log"), 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
tf <- Sys.time()
print(tf - t0)
#summary(poiss6)
#Gave a singular fit 23 minutes





