setwd("E:\\project\\黄河")
load("SPEI_InputClipDaily_Monthly.rda")
Station<- read.csv("中国气候日志数据V3station839.csv")
field1<- read.csv("./黄河流域基本数据/field.csv",header = F)
DailyData<-list(station = list())
Year<-matrix(data=NA,nrow = 94,ncol = 2)
for(i in 1:94){
  t=field1[[1]][i]
  Data<-data_in[[1]][[t]]
  len<-length(Data[,1])
  temp=0
  for(j in 1:len){
    rowName<-row.names(Data)[j]
    YearX<-as.numeric(gsub("(-)","",rowName))
    if(YearX - 19600101 == 0) temp=j
  }
  if(temp ==0){
    DailyData$station<-c(DailyData$station,list(Data))
  }
  else {
    Data<-Data[temp:len,]
    DailyData$station<-c(DailyData$station,list(Data))
  }
  
  rowName<-row.names(Data)[1]
  YearF<-as.numeric(gsub("(-)","",rowName))
  Year[i,1]<-YearF
  len<-length(Data[,1])
  rowName<-row.names(Data)[len]
  YearE<-as.numeric(gsub("(-)","",rowName))
  Year[i,2]<-YearE
}

save(DailyData,file = "MeteorologyDailyData.rda")
list.files(pattern = ".rda")