setwd('E:\\第二篇\\原始数据\\五站分位数')
rm(list = ls())
library(changepoint)
library(xlsx)
Stations<-c("花园口", "龙门", "唐乃亥", "兰州", "头道拐")
cp.year <- matrix(NA, 21, length(Stations))  #
colnames(cp.year) <- c("花园口", "龙门", "唐乃亥", "兰州", "头道拐")
rownames(cp.year) <- c("0", "0.05","0.10","0.15","0.20", "0.25","0.30","0.35","0.40","0.45", 
                       "0.50","0.55","0.60","0.65","0.70","0.75","0.80","0.85","0.90","0.95","1.00")
cp.pvalue <- matrix(NA, 21, length(Stations))  #
colnames(cp.pvalue) <- c("花园口", "龙门", "唐乃亥", "兰州", "头道拐")
rownames(cp.pvalue) <- c("0", "0.05","0.10","0.15","0.20", "0.25","0.30","0.35","0.40","0.45", 
                         "0.50","0.55","0.60","0.65","0.70","0.75","0.80","0.85","0.90","0.95","1.00")
for (i in 1:length(Stations)) {
  filename_Qmax<-paste("Qmax分位数_",sep="",Stations[i],".xlsx")
  Qmax<-read.xlsx2(filename_Qmax,sheetIndex=1,header=TRUE,colClasses=rep("numeric",22))
  for (j in 1:21) {
    cpt1 <- cpt.mean(Qmax[, j + 1], class = FALSE)
    cp.year[j, i] <- Qmax[cpt1[1], "Year"]
    cp.pvalue[j, i] <- cpt1[2]
  }
}
write.xlsx2(cp.year, "cp.year.xlsx",col.names = TRUE,row.names = TRUE)
write.xlsx2(cp.pvalue, "cp.pvalue.xlsx",col.names = TRUE,row.names = TRUE)