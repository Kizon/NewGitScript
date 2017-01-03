Mann_Kendall <- function(timeserial){#Mann Kendall 突变检验，传递参数
  Mann_Kendall_sub <- function(timeserial){#需要进行两次秩的分析，因此在函数中嵌套了一个函数
    r <- c()#分析的三个变量，具体含义可以参照魏凤英老师的书
    s <- c()#秩序列。
    U <- c()
    
    for(i in 2:length(timeserial))#进行大小比较，从第二个开始与以前的数据进行大小比较
    {r[i] <- 0
    
    for(j in 1:i)
    {
      if(timeserial[i]>timeserial[j]){r[i]=r[i]+1}#如果后面的数大于前面的数，则秩加1
    }
    
    
    s[i] <- 0
    for (ii in 2:i){ 
      s[i] <- s[i]+r[ii]#秩序列。Sk是第i时刻数值大于ii时刻数值个数的累计数
    }
    
    
    U[i] <- 0
    U[i] <- (s[i]- (i*(i-1)/4))/sqrt(i*(i-1)*(2*i+5)/72)
    
    }
    
    r[1] <- 0
    s[1] <- 0
    U[1] <- 0
    
    LST <- list(r = r, s = s, U = U)
    
    return (LST)
  }
  timeserial_rev <- rev(timeserial)#生成逆序列
  
  y1 <- Mann_Kendall_sub(timeserial)#计算正序列
  y2 <- Mann_Kendall_sub(timeserial_rev)#计算逆序列
  
  y2$U <- -(rev(y2$U))#转换符号与顺序
  
  LST <- list(UF=y1,UB=y2)
  return(LST)  
}
# 
# 
# 
# #这里是你要修改的地方
# setwd("d:/")
# od <- read.table("1.txt", header=T)
# Variable <- c("Jan","Feb","Mar","Apr","May","Jun")
# #修改上面代码
# 
# #可以自己定义，或者根据数据自动生成
# rows <- length(Variable)
# startyear <- as.numeric(od[1,1])
# years <- od$Year
# 
# #如果要生成图片，请执行相应代码
# tiff("filename.tif", width=14.6, height=16, units="cm", res=300, family = 'serif')
# par(mfrow=c(rows,2),oma=c(3,0,0,0), mar=c(0,2,0,0),cex=0.7)
# 
# for(i in 1:length(Variable)){
#   
#   name <- paste("od[        DISCUZ_CODE_0        ]quot;,Variable[i], sep="")
#                 value <- eval(parse(text=name))
#                 
#                 plot(value,type="l", ylab=Variable[i], cex.axis=0.6,xaxt="n",mgp=c(1,0.1,0),tck=-0.02)
#                 if(i==length(Variable)){
#                 axis(side=1, at=years, tck=-0.04, hadj=0.4, labels=years,mgp=c(1,0.4,0), cex.axis=1) # add x-axis to the last figure
#                 axis(side=1, at=1:length(od$Year), tck=-0.01, hadj=0.4, labels=NA, cex.axis=1) # add month labels to the x-axis
#                 mtext("年份",side=1,line=1.5)
#                 }
#                 
#                 
#                 d<-Mann_Kendall(value)#进行突变检验
#                 yUF <- as.data.frame(d$UF[3])$U
#                 yUB <- as.data.frame(d$UB[3])$U
#                 
#                 plot(x=c(1:length(od$Year)),y=yUF, type="l", ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=1, lty=5, ylab="", cex=0.5,xaxt="n",mgp=c(1,0.1,0),tck=-0.02)
#                 points(x=c(1:length(od$Year)),y=yUB,type="l",lty=3,col=6,lwd=1)
#                 abline(h=1.969,lty=4,lwd=0.5)# 1.969是a=0.05的显著性水平
#                 abline(h=-1.96,lty=4,lwd=0.5)
#                 abline(h=0,col="gray",lwd=0.5)
# }
# 
# axis(side=1, at=years, tck=-0.04, hadj=0.4, labels=years,mgp=c(1,0.4,0), cex.axis=1) # add x-axis to the last figure
# axis(side=1, at=1:length(od$Year), tck=-0.01, hadj=0.4, labels=NA, cex.axis=1) # add month labels to the x-axis
# mtext("年份",side=1,line=1.5)
# dev.off()