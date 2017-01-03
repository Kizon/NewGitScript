# 黄河站点径流数据MK突变检测 ufk/ubk
rm(list = ls(all = TRUE))
setwd('E:\\project\\黄河') #设置工作空间
source("Mann_Kendall.R") #导入mkTrend函数
flowData<-read.csv("黄河流域37水文站月径流量数据_万立方米每月.csv",header = TRUE) #读入径流数据
nStation<-length(unique(flowData[,2])) # 第二列为水文站，获取水文站的个数赋值为n=37；
nYear<-max(flowData[,3])-min(flowData[,3])+1 #计算年份即1960-2000年数，41年；

YearFlow<-rowSums(flowData[,4:15]) #12个月份相加=>年径流量，为一维数组;
YearFlow<-array(YearFlow,dim = c(41,37)) #数组重组为41*37的二维数组；第一维表示时间，二维表示站点；
YearFlow<-as.data.frame(YearFlow)
colnames(YearFlow) <-unique(flowData[,2]) #设置列名为站点代号328-366；
rownames(YearFlow) <-unique(flowData[,3]) #设置行名为年份1960-2000；
#以上处理得到黄河流域37个水文站1960-2000年41年的年径流数据YearFlow。
# View(flowData)
Flow<-read.csv("黄河年季月径流.csv",header = TRUE) #读取站点各年年季月的径流

# 典型站点提取
Ulz334<-Flow[247:287,] #上游流域兰州站334数据
n1=c(1,12,22,32)
n2=c(11,21,31,41)
averSs<-matrix(NA,nrow = 4,ncol = 4)
for(i in 1:4){
    averSs[i,]<-apply(Ulz334[n1[i]:n2[i],4:7],MARGIN=2,mean)
}


Ugd331<-Flow[124:164,] #上游流域贵德站331数据
for(i in 1:4){
  averSs[i,]<-apply(Ugd331[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Msm338<-Flow[411:451,] #中游流域三门峡站338数据
for(i in 1:4){
  averSs[i,]<-apply(Msm338[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Mlm337<-Flow[370:410,] #中游流域龙门峡站337数据
for(i in 1:4){
  averSs[i,]<-apply(Mlm337[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Dlj333<-Flow[206:246,] #下游流域利津站333数据
for(i in 1:4){
  averSs[i,]<-apply(Dlj333[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Dhyk340<-Flow[493:533,] #下游流域花园口站340数据
for(i in 1:4){
  averSs[i,]<-apply(Dhyk340[n1[i]:n2[i],4:7],MARGIN=2,mean)
}



UMD_Yr<-YearFlow[,c(4,7,10,11,13,6)] # 上中下游典型站年流量
write.csv(UMD_Yr,file ="UMD_Yr.csv")
UMD_YrUF<-matrix(NA,nrow=41,ncol = 6)
UMD_YrUB<-matrix(NA,nrow=41,ncol = 6)
for(i in 1:6){
  Temp<-UMD_Yr[,i]
  UMDYrLST<-Mann_Kendall(Temp)# 原函数的返回值为：LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubk同理；
  yUF <- as.data.frame(UMDYrLST$UF[3])$U #ufk
  yUB <- as.data.frame(UMDYrLST$UB[3])$U #ubk
  UMD_YrUF[,i]<-t(yUF)
  UMD_YrUB[,i]<-t(yUB)
}
write.csv(UMD_YrUF,file ="UMD_YrUF.csv")
write.csv(UMD_YrUB,file ="UMD_YrUB.csv")
rm(Temp)

#==========================================年径流突变绘图=========================================================#
#以下开始年径流mk突变检验
YearTol<-rowSums(YearFlow) #每行的各列间互相相加得到41年每年所有37个水文站的径流总和；
YearLST<-Mann_Kendall(YearTol) # 原函数的返回值为：LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubk同理；
Year_ufk<-YearLST$UF$U #ufk
Year_ubk<-YearLST$UB$U #ubk
#以上为年径流的mk突变检验

# 作图
yUF <- as.data.frame(YearLST$UF[3])$U #ufk
yUB <- as.data.frame(YearLST$UB[3])$U #ubk

DYear<-as.numeric(rownames(YearFlow))
x<-c(DYear)
# 
# plot(x,y=yUF, type="l", xlim=c(1961,1999),ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=2, lty=1,xaxt="n", xlab="",
#      ylab="", cex=0.5,mgp=c(1,0.1,0),tck=0.01)
# points(x,y=yUB,type="l",lty=2,col=1,lwd=2)
# abline(h=1.96,lty=4,lwd=0.5)# 1.96是a=0.05的显著性水平
# abline(h=-1.96,lty=4,lwd=0.5)
# abline(h=0,col="gray",lwd=0.5)
# legend("topright",c("UFk", "UBk"),lty = c(1,3),lwd = 2,bty="n", y.intersp = 2, ncol = 1)
# axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=0.01,mgp=c(1,0.1,0),las=1) # at表示在x轴值该处画标签，标签值为labels
# #box()

#
F_UB<-matrix(NA,nrow=41,ncol=5)
F_UF<-matrix(NA,nrow=41,ncol=5)
F_UF[,1]<-t(yUF)
F_UB[,1]<-t(yUB)

#==========================================四季径流突变绘图=========================================================#

Season<-array(0,dim = c(41,37,4)) #三维数组储存季节径流，第一维为年份，第二维为站点，第三维为季节

for(i in 1:4){
  for(j in 1:37){
    n1<-41*j-40
    n2<-41*j
    Season[,j,i]<-Flow[n1:n2,3+i]
  }
}

n<-c(4,7,10,11,13,6)
nameSta<-c("u331.csv","u334.csv","m337.csv","m338.csv","d340.csv","d333.csv")
UMD_UF<-matrix(NA,nrow = 41,ncol = 24)
UMD_UB<-matrix(NA,nrow = 41,ncol = 24)
UMDSsJP<-matrix(NA,nrow = 41,ncol = 24)
UMDSsLJP<-matrix(NA,nrow = 41,ncol = 24)
nn=0
for (i in 1:6){
  t<-n[i]
  Temp<-Season[,t,]
  # write.csv(Temp,file = nameSta[i])
  for (j in 1:4){
    nn=nn+1
    UMDSs<-Temp[,j]
    meanTemp<-mean(UMDSs)
    UMDSsJP[,nn]<-t(UMDSs-meanTemp)
    UMDSsLJP[,nn]<-cumsum(UMDSsJP[,nn])
    
    UMDSsLST<-Mann_Kendall(UMDSs)# 原函数的返回值为：LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubk同理；
    yUF <- as.data.frame(UMDSsLST$UF[3])$U #ufk
    yUB <- as.data.frame(UMDSsLST$UB[3])$U #ubk
    
    UMD_UF[,nn]<-t(yUF)
    UMD_UB[,nn]<-t(yUB)
  }
}
rm(Temp)
write.csv(UMD_UF,file = "UMD_UF.csv")
write.csv(UMD_UB,file = "UMD_UB.csv")
write.csv(UMDSsJP,file = "UMDSsJP.csv")
write.csv(UMDSsLJP,file = "UMDSsLJP.csv")

SeasonF<-matrix(NA,nrow = 41,ncol = 4)
for(i in 1:4){
  SeasonTol<-rowSums(Season[,,i]) #每行的各列间互相相加得到41年每年所有37个水文站的径流总和；
  SeasonF[,i]<-t(SeasonTol)
  SeasonLST<-Mann_Kendall(SeasonTol) # 原函数的返回值为：LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubk同理；
  Season_ufk<-SeasonLST$UF$U #ufk
  Season_ubk<-SeasonLST$UB$U #ubk
  t<-i+1
 
  #以上为年径流的mk突变检验
  
  # 作图
  yUF <- as.data.frame(SeasonLST$UF[3])$U #ufk
  yUB <- as.data.frame(SeasonLST$UB[3])$U #ubk
  F_UF[,t]<-t(yUF)
  F_UB[,t]<-t(yUB)
  DYear<-as.numeric(rownames(YearFlow))
  x<-c(DYear)
  
#   plot(x,y=yUF, type="l", xlim=c(1961,1999),ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=2, lty=1,xaxt="n", xlab="",
#        ylab="",cex=0.5,mgp=c(1,0.1,0),tck=0.01)
#   points(x,y=yUB,type="l",lty=2,col=1,lwd=2)
#   abline(h=1.96,lty=4,lwd=0.5)# 1.96是a=0.05的显著性水平
#   abline(h=-1.96,lty=4,lwd=0.5)
#   abline(h=0,col="gray",lwd=0.5)
#   #legend("topright",c("UFk", "UBk"),lty = c(1,3),lwd = 2,bty="n", y.intersp = 2, ncol = 1)
#   axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=0.01,mgp=c(1,0.1,0),las=1) # at表示在x轴值该处画标签，标签值为labels
  }

SeasonF<-as.data.frame(SeasonF)
rownames(F_UF)<-unique(flowData[,3])
rownames(F_UB)<-unique(flowData[,3])
F_UB<-as.data.frame(F_UB)
F_UF<-as.data.frame(F_UF)
colnames(F_UF)<-c("year","spring","summer","autumn","winter")
colnames(F_UB)<-c("year","spring","summer","autumn","winter")
# write.csv(F_UF, file = "F_UF.csv",col.names = TRUE,row.names = TRUE)
# write.csv(F_UB, file = "F_UB.csv",col.names = TRUE,row.names = TRUE)
# rm(list = ls(all = TRUE))
# setwd("G:/黄河/0")
# Fvalue<-read.csv("Fvalue.csv",header = TRUE)
# YearA<-cbind(Fvalue[,2],Fvalue[,3])
# rownames(YearA)<-Fvalue[,1]
# colnames(YearA)<-c("year","年")
# YearA<-data.frame(YearA)
# plot(YearA$year,YearA$年,type = "l",lwd = 2,lty = 1, xaxt = "n",xlab = "",ylab = "",cex = 0.5, tck=0.01,mgp=c(1,0.1,0))
# axis(1,labels=c(1960,1970,1980,1990,2000,2014),at=c(1960,1970,1980,1990,2000,2014),tck=0.01,las=1,mgp=c(1,0.1,0))
# abline(h=mean(YearA$年),lty = 6,lwd = 2,col = "black") #均值
# h1=mean(YearA$年[1:44])
# segments(1960,h1,2002,h1,lty = 6,lwd = 2,col = "black")
# h2=mean(YearA$年[45:55])
# segments(2003,h2,2014,h2,lty = 6,lwd = 2,col = "black")
# Y<-YearA$年
# x<-YearA$year
# model<-lm(Y~x,data = YearA)
# abline(model,col = "black",lty=3,lwd=2)#线性回归
YrF<-matrix(NA,nrow = 41,ncol = 2)
YrF[,1]<-t(DYear)
YrF[,2]<-t(YearTol)
YrF<-data.frame(YrF) #年径流总量
colnames(YrF)<-c("year","nian")
# plot(YrF$year,YrF$nian,type = "l",lwd = 2,lty = 1, xaxt = "n",xlab = "",ylab = "",cex = 0.5, tck=0.01,mgp=c(1,0.1,0))
# axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=0.01,mgp=c(1,0.1,0),las=1) # at表示在x轴值该处画标签，标签值为labels
# abline(h=mean(YrF$nian),lty = 6,lwd = 2,col = "black")
# h1=mean(YrF$nian[1:34])
# segments(1960,h1,1993,h1,lty = 6,lwd = 2,col = "black")
# h2=mean(YrF$nian[34:41])
# segments(1993,h2,2000,h2,lty = 6,lwd = 2,col = "black")
# Y<-YrF$nian
# x<-YrF$year
# model<-lm(Y~x,data = YrF)
# abline(model,col = "black",lty=3,lwd=2) #线性回归
# summary(model)
# dev.off()
# ======================================== 距 平 值 ================================================= #
# m_Yr<-mean(YrF[,2])
# JP_Yr<-YrF[,2]-m_Yr
# JP_Yr<-t(t(JP_Yr))
# LJP_Yr<-cumsum(JP_Yr)
# LJP_Yr<-t(t(LJP_Yr))
# JP_Ss<-matrix(NA,nrow = 41,ncol = 4)
# LJP_Ss<-matrix(NA,nrow = 41,ncol = 4)
# m_Ss<-matrix(NA,nrow = 1,ncol = 4)
# for(i in 1:4){
#   m_Ss[i]<-mean(SeasonF[,i])
#   JP_Ss[,i]<-SeasonF[,i]-m_Ss[i]
#   LJP_Ss[,i]<-cumsum(JP_Ss[,i])
# }
# JP<-matrix(NA,nrow = 41,ncol = 5)
# LJP<-matrix(NA,nrow = 41,ncol = 5)
# JP[,1]<-JP_Yr
# JP[,2:5]<-JP_Ss
# LJP[,1]<-LJP_Yr
# LJP[,2:5]<-LJP_Ss
# JP<-as.data.frame(JP)
# LJP<-as.data.frame(LJP)
# write.csv(JP,file = "JP.csv")
# write.csv(LJP,file ="LJP.csv")
# write.csv(YrF,file ="YrF.csv")
# write.csv(SeasonF,file ="SeasonF.csv")

UMDJP_Yr<-matrix(NA,nrow = 41,ncol = 6)
UMDLJP_Yr<-matrix(NA,nrow = 41,ncol = 6)

for(i in 1:6){
  UMDJP_Yr[,i]<-UMD_Yr[,i]-mean(UMD_Yr[,i])
  UMDLJP_Yr[,i]<-cumsum(UMDJP_Yr[,i])
}
write.csv(UMDJP_Yr,file ="UMDJP_Yr.csv")
write.csv(UMDLJP_Yr,file ="UMDLJP_Yr.csv")

