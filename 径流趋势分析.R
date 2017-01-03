# 黄河站点径流数据MK趋势分析
rm(list = ls(all = TRUE))
setwd('E:\\project\\黄河') #设置工作空间
source("mkTrend.R") #导入mkTrend函数
flowData<-read.csv("黄河流域37水文站月径流量数据_万立方米每月.csv",header = TRUE) #读入径流数据
nStation<-length(unique(flowData[,2])) # 第二列为水文站，获取水文站的个数赋值为n；
nYear<-max(flowData[,3])-min(flowData[,3])+1 #计算年份即1960-2000年数，41年；

# 1.径流月变化趋势
MonthlyTrend<-matrix(NA,nrow = nStation,ncol = 12) #生成空矩阵MonthlyTrend储存mk趋势值，行为站点，列为月份；
colnames(MonthlyTrend) <-colnames(flowData)[4:15]
rownames(MonthlyTrend) <-unique(flowData[,2])
for(i in 1:nStation){           #外层循环站点
  for(j in 1:12){               #内层月份循环
    n1<-1+(i-1)*nYear
    n2<-i*nYear
    xData<-flowData[n1:n2,3+j]  #行表示第i个站点的1960-2000年的流量，列为第j个月份的流量，故xData为第i个站点在j月的流量时间序列
    zt<-mkTrend(xData)
    MonthlyTrend[i,j]<-zt$Z 
  }
}
write.csv(MonthlyTrend, file = "黄河径流月变化趋势.csv",col.names = TRUE,row.names = TRUE)

# 2.径流季变化趋势 春季3-5月，夏季6-8月，秋季9-11月，冬季为1、2、12月；
SeasonTrend<-matrix(NA,nrow = nStation,ncol = 4) #生成空矩阵SeasonTrend储存mk趋势值，行为站点，列为季节；
SeasonFlow<-matrix(NA,nrow = nStation*nYear,ncol = 4) #储存季节径流值
colnames(SeasonTrend)<-c("春季", "夏季", "秋季", "冬季")
rownames(SeasonTrend) <-unique(flowData[,2])
for(i in 1:3){
  n<-3*i+3
  SeasonFlow[,i]<-rowSums(flowData[,n:(n+2)])
}
SeasonFlow[,4]<-rowSums(flowData[,c(4,5,15)])

for(i in 1:nStation){           #外层循环站点
  for(j in 1:4){               #内层月份循环
    n1<-1+(i-1)*nYear
    n2<-i*nYear
    xData<-SeasonFlow[n1:n2,j]  #行表示第i个站点的1960-2000年的流量，列为第j个季节的流量，故xData为第i个站点在j季的流量时间序列
    zt<-mkTrend(xData)
    SeasonTrend[i,j]<-zt$Z 
  }
}
write.csv(SeasonTrend, file = "黄河径流季变化趋势.csv",col.names = TRUE,row.names = TRUE)

# 3.径流年变化趋势
YearlyTrend<-matrix(NA,nrow = nStation,ncol = 1) #生成空矩阵YearlyTrend储存mk趋势值，行为站点，列为年份；
YearFlow<-matrix(NA,nrow = nStation*nYear,ncol = 1) #储存年径流值
YearFlow<-rowSums(flowData[,4:15])
colnames(YearlyTrend)<-"年"
rownames(YearlyTrend) <-unique(flowData[,2])
for(i in 1:nStation){           #外层循环站点
  n1<-1+(i-1)*nYear
  n2<-i*nYear
  xData<-YearFlow[n1:n2]  #行表示第i个站点的1960-2000年的流量，列为年份的流量，故xData为第i个站点的流量时间序列
  zt<-mkTrend(xData)
  YearlyTrend[i,1]<-zt$Z 
}
#write.csv(YearlyTrend, file = "黄河径流年变化趋势.csv",col.names = TRUE,row.names = TRUE)