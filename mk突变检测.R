# 黄河站点径流数据MK突变检测 ufk/ubk
rm(list = ls(all = TRUE))
setwd('E:\\project\\黄河') #设置工作空间
source("Mann_Kendall.R") #导入mkTrend函数
flowData<-read.csv("黄河流域37水文站月径流量数据_万立方米每月.csv",header = TRUE) #读入径流数据
nStation<-length(unique(flowData[,2])) # 第二列为水文站，获取水文站的个数赋值为n=37；
nYear<-max(flowData[,3])-min(flowData[,3])+1 #计算年份即1960-2000年数，41年；

YearFlow<-rowSums(flowData[,4:15]) #12个月份相加=>年径流量，为一维数组;
YearFlow<-array(YearFlow,dim = c(41,37)) #数组重组为41*37的二维数组；第一维表示时间，二维表示站点；
colnames(YearFlow) <-unique(flowData[,2]) #设置列名为站点代号328-366；
rownames(YearFlow) <-unique(flowData[,3]) #设置行名为年份1960-2000；
#以上处理得到黄河流域37个水文站1960-2000年41年的年径流数据YearFlow。
# View(flowData)

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
                
plot(x,y=yUF, type="l", ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=1, lty=5,xaxt="n", ylab="", cex=0.5,mgp=c(1,0.1,0),tck=0.01)
points(x,y=yUB,type="l",lty=3,col=6,lwd=1)
abline(h=1.969,lty=4,lwd=0.5)# 1.969是a=0.05的显著性水平
abline(h=-1.96,lty=4,lwd=0.5)
abline(h=0,col="gray",lwd=0.5)
axis(1,labels=c(1960,1970,1980,1990,2000),at=c(1960,1970,1980,1990,2000),tck=-0.01,las=1) # at表示在x轴值该处画标签，标签值为labels

# dev.off()