#This is an anlayis of the data created by PitcherMetrics.R
#We examine the top rated pitchers under 4 metrics and 
#compute the correlation between 5 metrics.  Includes vizualizations.
library(tidyr)
library(ggplot2)
#library(reshape2)

ssn <- 2021

file <- paste("Data/SPR_Data/Starters", ssn, ".csv", sep = "")
keeps <- read.csv(file, header = TRUE, sep = ",")

head(keeps)
vars <- c("Name", "Team", "SPR", "ERA", "FIP", "WARP", "DRA")
dfmetrics <- keeps[,vars]
head(dfmetrics)
L <- length(dfmetrics$Name)

N <- 10
rank <- 1:N
#Top10 in ERA
honERA <- cbind(rank,dfmetrics[order(dfmetrics$ERA)[1:N],c("Name", "Team", "ERA", "SPR")])
honERA
xtable(honERA, digits = 3)

#Top10 in FIP
honFIP <- cbind(rank,dfmetrics[order(dfmetrics$FIP)[1:10],c("Name", "Team", "FIP", "SPR")])
honFIP
xtable(honFIP, digits = 3)

#Top10 in DRA
honDRA <- cbind(rank, dfmetrics[order(dfmetrics$DRA)[1:10],c("Name", "Team", "DRA", "SPR")])
xtable(honDRA, digits = 3)

#Top10 in SPR
honSPR <- cbind(rank, dfmetrics[order(dfmetrics$SPR)[1:10],vars])
xtable(honSPR, digits = 3)

#Correlation of the 5 metrics
vars <- c("WARP", "ERA", "FIP", "DRA", "SPR")
dfcorr <- keeps[,vars]
head(dfcorr)
cormat <- round(cor(dfcorr), 4)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
#Make a triangular matrix
upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_fixed()
