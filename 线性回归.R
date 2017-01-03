#线性趋势
rm(list = ls(all = TRUE))
setwd('E:\\project\\黄河') #设置工作空间
#load("PET_冬.rda") #载入需要求流域平均的数值
Datas<- read.csv("黄河年季月径流.csv",header = T)

# a1<-a
# a<-matrix(NA,nrow = 94,ncol = 1) #℃/10a"
# colnames(a)<-"℃/10a"
# b<-matrix(NA,nrow = 56,ncol = 2)
# colnames(b)<-c("year","PET")
# rownames(b)<-row.names(a1)
# b[,1]<-as.numeric(row.names(a1))
# 
# for(i in 1:94){
#   b[,2]<-a1[,i]
#   b<-data.frame(b)
#   Y<-b$PET
#   x<-b$year
#   model<-lm(Y~x,data = b)
#   a[i,1]<-data.frame(model$coefficients)[2,1]*10
# }
# write.csv(a, file = "PET_黄河_冬线性趋势.csv")

Flow<-Datas[,3:19]
# Flow<-matrix(NA,nrow = 1517,ncol = 17)
# Flow标准化
# for(i in 1:37){
#   n1<-41*i-40
#   n2<-41*i
#   for(j in 1:17){
#     Flow[n1:n2,j]<-FlowNew[n1:n2,j]/FlowNew[n1,j]
#   }
# }



k<-matrix(NA,nrow = 37,ncol = 17)
colnames(k)<-colnames(Flow)
temp<-matrix(NA,nrow = 41,ncol = 2)
colnames(temp)<-c("year","FLOW")
temp[,1]<-as.numeric(Datas[1:41,2])

for(i in 1:37){
  n1<-41*i-40
  n2<-41*i
  for(j in 1:17){
    temp[,2]<-Flow[n1:n2,j]
    temp<-data.frame(temp)
    Y<-temp$FLOW
    x<-temp$year
    model<-lm(Y~x,data = temp)
    k[i,j]<-data.frame(model$coefficients)[2,1]*10/10000 #亿m3
  }
}

write.csv(k, file = "径流线性趋势10a.csv")

