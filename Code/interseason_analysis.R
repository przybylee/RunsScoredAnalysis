#Lee Przybylski
#11/18/2021

library(tidyr)
library(ggplot2)
#library(reshape2)


#Take 2 consecutive seasons and 1 pitching metric and check the correlation

metrics <- c("WARP","DRA", "FIP", "ERA", "SPR")
cross_ssn_cor <- matrix(data = 0, nrow = length(metrics)+1, ncol = 0)

for(ssn in 2015:2018){
  ssn1 <- ssn
  ssn2 <- ssn1+1
  #Read data from the 2 seasons
  file1 <- paste("Data/SPR_Data/Starters", ssn1, ".csv", sep = "")
  file2 <- paste("Data/SPR_Data/Starters", ssn2, ".csv", sep = "")
  df1 <- read.csv(file1, header = TRUE, sep = ",")
  df2 <- read.csv(file2, header = TRUE, sep = ",")
  df1 <- df1[,c("bpid", metrics)]
  df2 <- df2[,c("bpid", metrics)]
  #merge the two seasons by player id
  dftot <- merge(df1,df2, by = "bpid")
  col <- rep(0, length(metrics))
  for (j in 1:length(metrics)){
    metric <- metrics[j]
    N <- length(dftot$bpid)
    xname <- paste(metrics[j], "x", sep = ".")
    xval <- dftot[,xname]
    yname <- paste(metrics[j], "y", sep = ".")
    yval <- dftot[,yname]
    col[j] <- cor(xval, yval, use = "pairwise.complete.obs")
  }
  ssncol <- c(N, col)
  cross_ssn_cor <- cbind(cross_ssn_cor, ssncol)
}
#There was 1 missing value in WARP for 2019
cross_ssn_cor
xtable(cross_ssn_cor, digits = 3)
#SPR is not consistent between seasons