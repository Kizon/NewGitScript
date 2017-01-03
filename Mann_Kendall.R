Mann_Kendall <- function(timeserial){#Mann Kendall ͻ����飬���ݲ���
  Mann_Kendall_sub <- function(timeserial){#��Ҫ���������ȵķ���������ں�����Ƕ����һ������
    r <- c()#�������������������庬����Բ���κ��Ӣ��ʦ����
    s <- c()#�����С�
    U <- c()
    
    for(i in 2:length(timeserial))#���д�С�Ƚϣ��ӵڶ�����ʼ����ǰ�����ݽ��д�С�Ƚ�
    {r[i] <- 0
    
    for(j in 1:i)
    {
      if(timeserial[i]>timeserial[j]){r[i]=r[i]+1}#��������������ǰ����������ȼ�1
    }
    
    
    s[i] <- 0
    for (ii in 2:i){ 
      s[i] <- s[i]+r[ii]#�����С�Sk�ǵ�iʱ����ֵ����iiʱ����ֵ�������ۼ���
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
  timeserial_rev <- rev(timeserial)#����������
  
  y1 <- Mann_Kendall_sub(timeserial)#����������
  y2 <- Mann_Kendall_sub(timeserial_rev)#����������
  
  y2$U <- -(rev(y2$U))#ת��������˳��
  
  LST <- list(UF=y1,UB=y2)
  return(LST)  
}
# 
# 
# 
# #��������Ҫ�޸ĵĵط�
# setwd("d:/")
# od <- read.table("1.txt", header=T)
# Variable <- c("Jan","Feb","Mar","Apr","May","Jun")
# #�޸��������
# 
# #�����Լ����壬���߸��������Զ�����
# rows <- length(Variable)
# startyear <- as.numeric(od[1,1])
# years <- od$Year
# 
# #���Ҫ����ͼƬ����ִ����Ӧ����
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
#                 mtext("���",side=1,line=1.5)
#                 }
#                 
#                 
#                 d<-Mann_Kendall(value)#����ͻ�����
#                 yUF <- as.data.frame(d$UF[3])$U
#                 yUB <- as.data.frame(d$UB[3])$U
#                 
#                 plot(x=c(1:length(od$Year)),y=yUF, type="l", ylim=c(min(yUF,yUB,-1.96),max(yUF,yUB,1.96)),lwd=1, lty=5, ylab="", cex=0.5,xaxt="n",mgp=c(1,0.1,0),tck=-0.02)
#                 points(x=c(1:length(od$Year)),y=yUB,type="l",lty=3,col=6,lwd=1)
#                 abline(h=1.969,lty=4,lwd=0.5)# 1.969��a=0.05��������ˮƽ
#                 abline(h=-1.96,lty=4,lwd=0.5)
#                 abline(h=0,col="gray",lwd=0.5)
# }
# 
# axis(side=1, at=years, tck=-0.04, hadj=0.4, labels=years,mgp=c(1,0.4,0), cex.axis=1) # add x-axis to the last figure
# axis(side=1, at=1:length(od$Year), tck=-0.01, hadj=0.4, labels=NA, cex.axis=1) # add month labels to the x-axis
# mtext("���",side=1,line=1.5)
# dev.off()