# �ƺ�վ�㾶������MKͻ���� ufk/ubk
rm(list = ls(all = TRUE))
setwd('E:\\project\\�ƺ�') #���ù����ռ�
source("Mann_Kendall.R") #����mkTrend����
flowData<-read.csv("�ƺ�����37ˮ��վ�¾���������_��������ÿ��.csv",header = TRUE) #���뾶������
nStation<-length(unique(flowData[,2])) # �ڶ���Ϊˮ��վ����ȡˮ��վ�ĸ�����ֵΪn=37��
nYear<-max(flowData[,3])-min(flowData[,3])+1 #������ݼ�1960-2000������41�ꣻ

YearFlow<-rowSums(flowData[,4:15]) #12���·����=>�꾶������Ϊһά����;
YearFlow<-array(YearFlow,dim = c(41,37)) #��������Ϊ41*37�Ķ�ά���飻��һά��ʾʱ�䣬��ά��ʾվ�㣻
YearFlow<-as.data.frame(YearFlow)
colnames(YearFlow) <-unique(flowData[,2]) #��������Ϊվ�����328-366��
rownames(YearFlow) <-unique(flowData[,3]) #��������Ϊ���1960-2000��
#���ϴ����õ��ƺ�����37��ˮ��վ1960-2000��41����꾶������YearFlow��
# View(flowData)
Flow<-read.csv("�ƺ��꼾�¾���.csv",header = TRUE) #��ȡվ������꼾�µľ���

# ����վ����ȡ
Ulz334<-Flow[247:287,] #������������վ334����
n1=c(1,12,22,32)
n2=c(11,21,31,41)
averSs<-matrix(NA,nrow = 4,ncol = 4)
for(i in 1:4){
    averSs[i,]<-apply(Ulz334[n1[i]:n2[i],4:7],MARGIN=2,mean)
}


Ugd331<-Flow[124:164,] #����������վ331����
for(i in 1:4){
  averSs[i,]<-apply(Ugd331[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Msm338<-Flow[411:451,] #������������Ͽվ338����
for(i in 1:4){
  averSs[i,]<-apply(Msm338[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Mlm337<-Flow[370:410,] #������������Ͽվ337����
for(i in 1:4){
  averSs[i,]<-apply(Mlm337[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Dlj333<-Flow[206:246,] #������������վ333����
for(i in 1:4){
  averSs[i,]<-apply(Dlj333[n1[i]:n2[i],4:7],MARGIN=2,mean)
}
Dhyk340<-Flow[493:533,] #��������԰��վ340����
for(i in 1:4){
  averSs[i,]<-apply(Dhyk340[n1[i]:n2[i],4:7],MARGIN=2,mean)
}



UMD_Yr<-YearFlow[,c(4,7,10,11,13,6)] # �������ε���վ������
write.csv(UMD_Yr,file ="UMD_Yr.csv")
UMD_YrUF<-matrix(NA,nrow=41,ncol = 6)
UMD_YrUB<-matrix(NA,nrow=41,ncol = 6)
for(i in 1:6){
  Temp<-UMD_Yr[,i]
  UMDYrLST<-Mann_Kendall(Temp)# ԭ�����ķ���ֵΪ��LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubkͬ����
  yUF <- as.data.frame(UMDYrLST$UF[3])$U #ufk
  yUB <- as.data.frame(UMDYrLST$UB[3])$U #ubk
  UMD_YrUF[,i]<-t(yUF)
  UMD_YrUB[,i]<-t(yUB)
}
write.csv(UMD_YrUF,file ="UMD_YrUF.csv")
write.csv(UMD_YrUB,file ="UMD_YrUB.csv")
rm(Temp)

#==========================================�꾶��ͻ���ͼ=========================================================#
#���¿�ʼ�꾶��mkͻ�����
YearTol<-rowSums(YearFlow) #ÿ�еĸ��м以����ӵõ�41��ÿ������37��ˮ��վ�ľ����ܺͣ�
YearLST<-Mann_Kendall(YearTol) # ԭ�����ķ���ֵΪ��LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubkͬ����
Year_ufk<-YearLST$UF$U #ufk
Year_ubk<-YearLST$UB$U #ubk
#����Ϊ�꾶����mkͻ�����

# ��ͼ
yUF <- as.data.frame(YearLST$UF[3])$U #ufk
yUB <- as.data.frame(YearLST$UB[3])$U #ubk

DYear<-as.numeric(rownames(YearFlow))
x<-c(DYear)
# 
# plot(x,y=yUF, type="l", xlim=c(1961,1999),ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=2, lty=1,xaxt="n", xlab="",
#      ylab="", cex=0.5,mgp=c(1,0.1,0),tck=0.01)
# points(x,y=yUB,type="l",lty=2,col=1,lwd=2)
# abline(h=1.96,lty=4,lwd=0.5)# 1.96��a=0.05��������ˮƽ
# abline(h=-1.96,lty=4,lwd=0.5)
# abline(h=0,col="gray",lwd=0.5)
# legend("topright",c("UFk", "UBk"),lty = c(1,3),lwd = 2,bty="n", y.intersp = 2, ncol = 1)
# axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=0.01,mgp=c(1,0.1,0),las=1) # at��ʾ��x��ֵ�ô�����ǩ����ǩֵΪlabels
# #box()

#
F_UB<-matrix(NA,nrow=41,ncol=5)
F_UF<-matrix(NA,nrow=41,ncol=5)
F_UF[,1]<-t(yUF)
F_UB[,1]<-t(yUB)

#==========================================�ļ�����ͻ���ͼ=========================================================#

Season<-array(0,dim = c(41,37,4)) #��ά���鴢�漾�ھ�������һάΪ��ݣ��ڶ�άΪվ�㣬����άΪ����

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
    
    UMDSsLST<-Mann_Kendall(UMDSs)# ԭ�����ķ���ֵΪ��LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubkͬ����
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
  SeasonTol<-rowSums(Season[,,i]) #ÿ�еĸ��м以����ӵõ�41��ÿ������37��ˮ��վ�ľ����ܺͣ�
  SeasonF[,i]<-t(SeasonTol)
  SeasonLST<-Mann_Kendall(SeasonTol) # ԭ�����ķ���ֵΪ��LST <- list(UF=y1,UB=y2),ufk<-YearLST$UF$U,ubkͬ����
  Season_ufk<-SeasonLST$UF$U #ufk
  Season_ubk<-SeasonLST$UB$U #ubk
  t<-i+1
 
  #����Ϊ�꾶����mkͻ�����
  
  # ��ͼ
  yUF <- as.data.frame(SeasonLST$UF[3])$U #ufk
  yUB <- as.data.frame(SeasonLST$UB[3])$U #ubk
  F_UF[,t]<-t(yUF)
  F_UB[,t]<-t(yUB)
  DYear<-as.numeric(rownames(YearFlow))
  x<-c(DYear)
  
#   plot(x,y=yUF, type="l", xlim=c(1961,1999),ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=2, lty=1,xaxt="n", xlab="",
#        ylab="",cex=0.5,mgp=c(1,0.1,0),tck=0.01)
#   points(x,y=yUB,type="l",lty=2,col=1,lwd=2)
#   abline(h=1.96,lty=4,lwd=0.5)# 1.96��a=0.05��������ˮƽ
#   abline(h=-1.96,lty=4,lwd=0.5)
#   abline(h=0,col="gray",lwd=0.5)
#   #legend("topright",c("UFk", "UBk"),lty = c(1,3),lwd = 2,bty="n", y.intersp = 2, ncol = 1)
#   axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=0.01,mgp=c(1,0.1,0),las=1) # at��ʾ��x��ֵ�ô�����ǩ����ǩֵΪlabels
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
# setwd("G:/�ƺ�/0")
# Fvalue<-read.csv("Fvalue.csv",header = TRUE)
# YearA<-cbind(Fvalue[,2],Fvalue[,3])
# rownames(YearA)<-Fvalue[,1]
# colnames(YearA)<-c("year","��")
# YearA<-data.frame(YearA)
# plot(YearA$year,YearA$��,type = "l",lwd = 2,lty = 1, xaxt = "n",xlab = "",ylab = "",cex = 0.5, tck=0.01,mgp=c(1,0.1,0))
# axis(1,labels=c(1960,1970,1980,1990,2000,2014),at=c(1960,1970,1980,1990,2000,2014),tck=0.01,las=1,mgp=c(1,0.1,0))
# abline(h=mean(YearA$��),lty = 6,lwd = 2,col = "black") #��ֵ
# h1=mean(YearA$��[1:44])
# segments(1960,h1,2002,h1,lty = 6,lwd = 2,col = "black")
# h2=mean(YearA$��[45:55])
# segments(2003,h2,2014,h2,lty = 6,lwd = 2,col = "black")
# Y<-YearA$��
# x<-YearA$year
# model<-lm(Y~x,data = YearA)
# abline(model,col = "black",lty=3,lwd=2)#���Իع�
YrF<-matrix(NA,nrow = 41,ncol = 2)
YrF[,1]<-t(DYear)
YrF[,2]<-t(YearTol)
YrF<-data.frame(YrF) #�꾶������
colnames(YrF)<-c("year","nian")
# plot(YrF$year,YrF$nian,type = "l",lwd = 2,lty = 1, xaxt = "n",xlab = "",ylab = "",cex = 0.5, tck=0.01,mgp=c(1,0.1,0))
# axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=0.01,mgp=c(1,0.1,0),las=1) # at��ʾ��x��ֵ�ô�����ǩ����ǩֵΪlabels
# abline(h=mean(YrF$nian),lty = 6,lwd = 2,col = "black")
# h1=mean(YrF$nian[1:34])
# segments(1960,h1,1993,h1,lty = 6,lwd = 2,col = "black")
# h2=mean(YrF$nian[34:41])
# segments(1993,h2,2000,h2,lty = 6,lwd = 2,col = "black")
# Y<-YrF$nian
# x<-YrF$year
# model<-lm(Y~x,data = YrF)
# abline(model,col = "black",lty=3,lwd=2) #���Իع�
# summary(model)
# dev.off()
# ======================================== �� ƽ ֵ ================================================= #
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
