setwd('E:\\第二篇\\原始数据\\五站分位数')
rm(list = ls())
library(xlsx)
source("mkTrend.R")
Stations<-c("花园口", "龙门", "唐乃亥", "兰州", "头道拐")
Flood_trend <- matrix(NA, 21, length(Stations))
colnames(Flood_trend) <- c("花园口", "龙门", "唐乃亥", "兰州", "头道拐")
rownames(Flood_trend) <- c("0", "0.05","0.10","0.15","0.20", "0.25","0.30","0.35","0.40","0.45", 
                       "0.50","0.55","0.60","0.65","0.70","0.75","0.80","0.85","0.90","0.95","1.00")
for (i in 1:length(Stations)) {
  filename_Qmax<-paste("Qmax分位数_",sep="",Stations[i],".xlsx")
  Qmax<-read.xlsx2(filename_Qmax,sheetIndex=1,header=TRUE,colClasses=rep("numeric",22))
  for (j in 1:21) {
    z1 <- mkTrend(Qmax[, j + 1])
    Flood_trend[j, i] <- z1$Z
  }
}
write.xlsx2(Flood_trend, "分位数指标趋势.xlsx",col.names = TRUE,row.names = TRUE)