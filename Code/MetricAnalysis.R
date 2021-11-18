

#This is an anlayis of the data created by PitcherMetrics.R

head(keeps)
vars <- c("Name", "Team", "ERA", "FIP", "WARP", "DRA", "SPR")
dfmetrics <- keeps[,vars]
head(dfmetrics)
L <- length(dfmetrics$Name)

#Top10 in ERA
honERA <- dfmetrics[order(dfmetrics$ERA)[1:10],]
honERA
xtable(honERA, digits = 3)

#Top10 in FIP
dfmetrics[order(dfmetrics$FIP)[1:10],]


#Top10 in WARP
dfmetrics[order(dfmetrics$WARP)[rev((L-9):L)],]

#Top10 in SPR
dfmetrics[order(dfmetrics$SPR)[1:10],]


