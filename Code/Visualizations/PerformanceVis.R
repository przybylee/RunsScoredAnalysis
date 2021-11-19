#Lee Przybylski
#11/18/2021   

#Plot model performance based on Validation Testing for a given season
library(ggplot2)
library(tidyr)

ssn <- 2021
file <- paste("Data/ModelPerformance/performance", ssn, ".csv", sep = "")
dfperf <- read.csv(file, sep = ",")
head(dfperf)

#Infer the total number of games
ng_samp <- dfperf$Ngames[1]
prop_samp <- dfperf$obsrate[2] - dfperf$obsrate[1]
ng_samp/(2*prop_samp)

gamefile <- paste("Data/RawData/mlbodds", ssn, ".csv", sep = "")
source("Code/Functions/frmt_data.R")
dfgames <- frmt_data(gamefile, ssn)
head(dfgames, n=20)
max(dfgames$gameID)

#Axis breaks
xbreaks <- seq(0.1, to = 1, by = 0.1)
#Model Names
mnames <- c("GLM", "R. GLME", "F. GLME")

#Plot RMSE of the three different models
vars <- c("obsrate", "RMSE1", "RMSE2", "RMSE3")
dfRMSE <- dfperf[,vars]
head(dfRMSE)

dfRMSE<- pivot_longer(data = dfRMSE, cols = vars[2:4], names_to = "Model")
dfRMSE <- dfRMSE[order(dfRMSE$Model),]
ybreaks <- 

plot <- ggplot(data = dfRMSE, aes(x= obsrate, y = value, group = Model)) + 
  geom_line(aes(color = Model))+ 
  geom_point(aes(color = Model))+
  scale_color_hue(labels = mnames)+
  xlab("Proportion of Season Observed")+
  ylab("RMSE")+labs(title = "2021 Season")+
  scale_x_continuous(breaks = xbreaks)+
  scale_y_continuous(breaks = seq(2.4, 4, by = 0.1))
plot
#R GLME performs basically the same as model F GLME

head(dfperf)

#Plot accuracy of the three different models
vars <- c("obsrate", "Acc1", "Acc2", "Acc3")
dfACC <- dfperf[,vars]
head(dfACC)

dfACC<- pivot_longer(data = dfACC, cols = vars[2:4], names_to = "Model")
dfACC <- dfACC[order(dfACC$Model),]
ggplot(data = dfACC, aes(x= obsrate, y = value, group = Model)) + 
  geom_line(aes(color = Model))+ 
  geom_point(aes(color = Model))+
  scale_color_hue(labels = mnames)+
  xlab("Proportion of Season Observed")+
  ylab("Prediction Accuracy")+
  labs(title = paste(ssn, "Season", sep = " "))+
  scale_x_continuous(breaks = xbreaks)+
  scale_y_continuous(breaks = seq(0.4, 1, by = 0.05))

#Add opening line accuracy
ggplot(data = dfACC, aes(x= obsrate, y = value, group = Model)) + 
  geom_line(aes(color = Model))+ 
  geom_point(aes(color = Model))+
  scale_color_hue(labels = mnames)+
  xlab("Proportion of Season Observedd")+
  ylab("Prediction Accuracy")+
  labs(title = paste(ssn, "Season", sep = " "))+
  scale_x_continuous(breaks = xbreaks)+
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.05))+
  geom_hline(yintercept = mean(dfperf$AccOpen), linetype = "dashed", 
             color = "green", size =2)+
  geom_hline(yintercept = mean(dfperf$AccClose), linetype = "dashed", 
             color = "red", size = 2)

head(dfgames)
sum((dfgames$RunLine >= 0)&(dfgames$Win == 1))/length(dfgames$RunLine)
summary(dfgames$RunLine)
