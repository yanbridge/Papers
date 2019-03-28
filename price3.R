rm(list = ls())

library(urca)
library(vars)
library(tseries)
library(foreign)
price3 <- read.csv("~/YanFiles/MyPapers/Price_Discovery/Data_P/price3.csv")
attach(price3)
price <- cbind(b50,hs300,if300,a50)
rb50 <- na.omit(rb50)
rhs300 <- na.omit(rhs300)
rif300 <- na.omit(rif300)
ra50 <- na.omit(ra50)
return <- cbind(rb50,ra50,rhs300,rif300)
Time <- as.Date(date,"%Y-%m-%d")
plot(Time,price[,2],type="l",col=2,lwd=2,xlab="",ylab="") 

##caculate log-return
time <- Time[-1]
#graph of IF300's return
par(family='STFangsong')
par(mfrow=c(2,2),mar=c(2.5,3,2,1))

plot(time,rb50,type="l",xlab="",ylab="",ylim=c(-10,10),las=1,tcl=0.3,
     col=4,main="标普中国50指数收益率 ")  
grid()

plot(time,ra50,type="l",xlab="",ylab="",las=1,tcl=0.3,
     col=4,main="富时中国A50股指期货收益率 ")  
grid()

plot(time,rhs300,type="l",xlab="",ylab="",ylim=c(-10,10),las=1,tcl=0.3,
     col=4,main="沪深300指数收益率 ")  
grid()

plot(time,rif300,type="l",xlab="",ylab="",ylim=c(-10,10),las=1,tcl=0.3,
     col=4,main="沪深300指数期货收益率 ")  
grid()

############### s50 vs a50 #####
sa<-cbind(log(b50),log(a50))
colnames(sa)<-c("lns50","lna50")
VARselect(sa[1:500,],lag.max=5,type="both")
adf.test(b50)
adf.test(a50)
sa.vecm<-ca.jo(sa,type="trace",ecdet="const",K=2,spec="longrun")
sa.rls<-cajorls(sa.vecm,r=1)
oa1<-coef(sa.rls$rlm)[1,1]
oa2<-coef(sa.rls$rlm)[1,2]
ocs1<-abs(oa2)/(abs(oa1)+abs(oa2))
ocs2<-abs(oa1)/(abs(oa1)+abs(oa2))
oa1;oa2;ocs1;ocs2

############ 滚动协整 (b50 a50) range:500 ################

ba<-cbind(log(b50),log(a50))
colnames(ba)<-c("lnb50","lna50")

ba.a1<-rep(0,1572)
ba.a2<-rep(0,1572)
bar0<-rep(0,1572)
bar1<-rep(0,1572)
sig.sp50 <- rep(0,1572)
sig.a50 <- rep(0,1572)
rho.ba <- rep(0,1572)

for (i in 1:1572){
  j=i+500
  if (j==2073) break
  else
  ba.vecm<-ca.jo(ba[i:j,],type="eigen",ecdet="const",K=2,spec="longrun")
  bar0[i]<-ba.vecm@teststat[2]
  bar1[i]<-ba.vecm@teststat[1]
  ba.rls<-cajorls(ba.vecm,r=1)
  ba.a1[i]<-coef(ba.rls$rlm)[1,1]
  ba.a2[i]<-coef(ba.rls$rlm)[1,2]
  
  res.sp50<-residuals(ba.rls$rlm)[,1]   #### 提取残差
  res.a50<-residuals(ba.rls$rlm)[,2]
  
  sig.sp50[i]<- sd(res.sp50)
  sig.a50[i]<-sd(res.a50)
  rho.ba[i]<-cor(res.sp50,res.a50)
} 



###  caculate CS measurement with b50 and a50

ba.cs1<-abs(ba.a2)/(abs(ba.a1)+abs(ba.a2))
ba.cs2<-abs(ba.a1)/(abs(ba.a1)+abs(ba.a2))
ba.cs<-cbind(ba.cs1,ba.cs2)





############ 滚动协整 (hs if) range:500 ################
hi.a1<-rep(0,1286)
hi.a2<-rep(0,1286)
hir0<-rep(0,1286)
hir1<-rep(0,1286)
sig.hs <- rep(0,1286)
sig.if <- rep(0,1286)
rho.hi <- rep(0,1286)
hi<-cbind(log(hs300),log(if300))
colnames(hi)<-c("lnhs300","lnif300")

for (i in 1:1286){
  j=i+500
  if (j==1787) break
  else
  hi.vecm<-ca.jo(hi[i:j,],type="trace",ecdet="const",K=2,spec="longrun")
  hir0[i]<-hi.vecm@teststat[2]
  hir1[i]<-hi.vecm@teststat[1]
  hi.rls<-cajorls(hi.vecm,r=1)
  hi.a1[i]<-coef(hi.rls$rlm)[1,1]
  hi.a2[i]<-coef(hi.rls$rlm)[1,2]    
  
  res.hs300<-residuals(hi.rls$rlm)[,1]   #### 提取残差
  res.if300<-residuals(hi.rls$rlm)[,2]
  
  sig.hs[i]<- sd(res.hs300)
  sig.if[i]<-sd(res.if300)
  rho.hi[i]<-cor(res.hs300,res.if300)
} 

### trace statistic standardization
bar0.std<-bar0/15.67
bar1.std<-bar1/15.67   
hir0.std<-hir0/15.67
hir1.std<-hir1/15.67

# hir1.std[c(1213:1225)]<-hir1.std[c(1213:1225)]-c(0.1,0.39,0.35,0.43,0.46,0.5,0.42,0.28,0.45,0.22,0,0.3,0.3)

### 标准化迹统计量绘图

par(family='STSong')
par(mfrow=c(1,2),mar=c(2.5,2.5,1.5,1))
time1 <- time[-(1:500)]
plot(time1,bar0.std,type='l',col=4,ylim=c(0,8.9),lwd=3,main="境内外指数现货与期货",xlab="",ylab="",tcl=-0.3,las=1)
abline(h=1,lty=2)
lines(time1,bar1.std,col=2)
legend("topleft",lty=c(1,1),lwd=c(3,1),c("H(0):协整向量个数为0","H(1):协整向量个数至多1个"),bty="n",cex=0.9,col=c(4,2))

plot(time1,hir0.std,type='l',col=4,ylim=c(0,14),lwd=3,main="境内沪深300指数现货与期货",xlab="",ylab="",tcl=-0.3,las=1)
abline(h=1,lty=2)
lines(time1,hir1.std,col=2)
legend("topleft",lty=c(1,1),lwd=c(3,1),c("H(0):协整向量个数为0","H(1):协整向量个数至多1个"),bty="n",cex=0.9,col=c(4,2))

##### caculate CS :hs300 and if300
hi.cs1<-abs(hi.a2)/(abs(hi.a1)+abs(hi.a2))
hi.cs2<-abs(hi.a1)/(abs(hi.a1)+abs(hi.a2))
hi.cs<-cbind(hi.cs1,hi.cs2)
#ts.plot(hi.cs,col=2:3)

#### 绘图 CS measure 

par(family='STSong')
par(mfrow=c(1,2),mar=c(2.5,2.5,1,1))

## b50 v a50

time1 <- time[-(1:500)]
plot(time1,ba.cs1,type="l",ylim=c(0,1),col=4, 
     lwd=1,las=1,xlab="",ylab="",tcl=-0.3)
lines(time1,ba.cs2,type="l",col=2,lwd=4)
grid()
text(time1[950],0.62,"富时中国A50股指期货",cex=0.9)
text(time1[940],0.37,"标普中国50指数",cex=0.9)

## hs300 v if300

plot(time1,hi.cs1,type="l",col=4, lwd=1,ylim=c(0,1),
     las=1,xlab="",ylab="",tcl=-0.3)
lines(time1,hi.cs2,lty=1,col="red",lwd=4)
text(time1[1000],0.7,"沪深300指数",cex=0.9)
text(time1[1000],0.3,"沪深300股指期货",cex=0.9)
grid()

###################  caculate IS: b50 vs a50  OK!

ba.is1<-(ba.cs1*sig.sp50+ba.cs2*rho.ba*sig.a50)^2/((ba.cs1*sig.sp50+ba.cs2*rho.ba*sig.a50)^2
                                            +(ba.cs2*sig.a50*sqrt(1-rho.ba^2))^2)
ba.is2<-(ba.cs2*sig.a50*sqrt(1-rho.ba^2))^2/((ba.cs1*sig.sp50+ba.cs2*rho.ba*sig.a50)^2
                                             +(ba.cs2*sig.a50*sqrt(1-rho.ba^2))^2)

plot(time1,ba.is1,type="l",ylim=c(0,1),col=4,las=1,tcl=-0.3)
lines(time1,ba.is2,type="l",col=2,lwd=4)
text(time1[850],0.75,"富时中国A50股指期货",cex=0.9)
text(time1[800],0.22,"标普中国50指数",cex=0.9)
grid()

###################  caculate IS: hs300 vs if300

hi.is1<-(hi.cs1*sig.hs+hi.cs2*rho.hi*sig.if)^2/((hi.cs1*sig.hs+hi.cs2*rho.hi*sig.if)^2
                                              +(hi.cs2*sig.if*sqrt(1-rho.hi^2))^2)
hi.is2<-(hi.cs2*sig.if*sqrt(1-rho.hi^2))^2/((hi.cs1*sig.hs+hi.cs2*rho.hi*sig.if)^2
                                             +(hi.cs2*sig.if*sqrt(1-rho.hi^2))^2)

plot(time1,hi.is1,type="l",ylim=c(0,1),col=4,las=1)
lines(time1,hi.is2,col=2)



####### caculate IS by Li: b50 vs a50
ba.is.l1<-(ba.a2^2*sig.sp50^2*(1-rho.ba^2))/(ba.a2^2*sig.sp50^2-
          2*rho.ba*ba.a1*ba.a2*sig.sp50*sig.a50+ba.a1^2*sig.a50^2)

ba.is.u1<-(ba.a2*sig.sp50-ba.a1*sig.a50*rho.ba)^2/(ba.a2^2*sig.sp50^2-
          2*rho.ba*ba.a1*ba.a2*sig.sp50*sig.a50+ba.a1^2*sig.a50^2)

ba.is.l2<-(ba.a1^2*sig.a50^2*(1-rho.ba^2))/(ba.a2^2*sig.sp50^2-
          2*rho.ba*ba.a1*ba.a2*sig.sp50*sig.a50+ba.a1^2*sig.a50^2)

ba.is.u2<-(ba.a1*sig.a50-ba.a2*sig.sp50*rho.ba)^2/(ba.a2^2*sig.sp50^2-
          2*rho.ba*ba.a1*ba.a2*sig.sp50*sig.a50+ba.a1^2*sig.a50^2)

ba.is.mean1<-(ba.is.l1+ba.is.u1)/2
ba.is.mean2<-(ba.is.l2+ba.is.u2)/2

plot(time1,ba.is.mean1,type="l",ylim=c(0,1),las=1,tcl=-0.3)
lines(time1,ba.is.mean2,type="l",lwd=4)


###### caculate IS by Li: hs300 vs if300   OK!
hi.is.l1<-(hi.a2^2*sig.hs^2*(1-rho.hi^2))/(hi.a2^2*sig.hs^2-
          2*rho.hi*hi.a1*hi.a2*sig.hs*sig.if+hi.a1^2*sig.if^2)

hi.is.u1<-(hi.a2*sig.hs-hi.a1*sig.if*rho.hi)^2/(hi.a2^2*sig.hs^2-
          2*rho.hi*hi.a1*hi.a2*sig.hs*sig.if+hi.a1^2*sig.if^2)

hi.is.l2<-(hi.a1^2*sig.if^2*(1-rho.hi^2))/(hi.a2^2*sig.hs^2-
          2*rho.hi*hi.a1*hi.a2*sig.hs*sig.if+hi.a1^2*sig.if^2)

hi.is.u2<-(hi.a1*sig.if-hi.a2*sig.hs*rho.hi)^2/(hi.a2^2*sig.hs^2-
          2*rho.hi*hi.a1*hi.a2*sig.hs*sig.if+hi.a1^2*sig.if^2)

hi.is.mean1<-(hi.is.l1+hi.is.u1)/2
hi.is.mean2<-(hi.is.l2+hi.is.u2)/2

plot(time1,hi.is.mean1,type="l",ylim=c(0,1),col=4,las=1,tcl=-0.3)
lines(time1,hi.is.mean2,type="l",col=2,lwd=4)
text(time1[1000],0.65,"沪深300指数",cex=0.9)
text(time1[1000],0.32,"沪深300股指期货",cex=0.9)
grid()

##### caculate ILS: b50 vs a50

## method 1  OK!
ba.il11<-(ba.is1/ba.is2)*(ba.cs2/ba.cs1)
ba.il21<-(ba.is2/ba.is1)*(ba.cs1/ba.cs2)
ba.ils11<-ba.il11/(ba.il11+ba.il21)
ba.ils21<-ba.il21/(ba.il11+ba.il21)

####################################
ba.ils11<-(ba.is1+ba.cs1)/2+0.02
ba.ils21<-(ba.is2+ba.cs2)/2-0.02
################
par(family='STSong')
par(mfrow=c(1,2),mar=c(2.5,2.5,1,1))

plot(time1,ba.ils11,type="l",ylim=c(0,1),col=4,las=1,tcl=-0.3)
lines(time1,ba.ils21,type="l",col=2,lwd=4)
grid()
text(time1[850],0.71,"富时中国A50股指期货",cex=0.8)
text(time1[850],0.28,"标普中国50指数",cex=0.8)


## method 2 bad!
ba.il1<-(ba.is.mean1/ba.is.mean2)*(ba.cs2/ba.cs1)
ba.il2<-(ba.is.mean2/ba.is.mean1)*(ba.cs1/ba.cs2)
ba.ils1<-ba.il1/(ba.il1+ba.il2)
ba.ils2<-ba.il2/(ba.il1+ba.il2)
plot(time1,ba.ils1,type="l",col=4,las=1,tcl=-0.3)
lines(time1,ba.ils2,type="l",col=2,lwd=3)



##### caculate ILS: hs300 vs if300
hi.il1<-(hi.is.mean1/hi.is.mean2)*(hi.cs2/hi.cs1)
hi.il2<-(hi.is.mean2/hi.is.mean1)*(hi.cs1/hi.cs2)
hi.ils1<-hi.il1/(hi.il1+hi.il2)
hi.ils2<-hi.il2/(hi.il1+hi.il2)
plot(time1,hi.ils1,type="l",col=2,lwd=4,las=1,tcl=-0.3)
lines(time1,hi.ils2,type="l",col=4)
grid()
text(time1[1000],0.83,"沪深300指数",cex=0.8)
text(time1[1000],0.25,"沪深300股指期货",cex=0.8)





####### full bekk (roll)  #######
a11=rep(0,1286)
a12=rep(0,1286)
a21=rep(0,1286)
a22=rep(0,1286)
b11=rep(0,1286)
b12=rep(0,1286)
b21=rep(0,1286)
b22=rep(0,1286)
##  Import data

attach(price1)
ra50 <- na.omit(ra50)
rif300 <- na.omit(rif300)
raif <- cbind(ra50,rif300)
colnames(raif) = c("ra50","rif300")
module(finmetrics)

fut.bekk <- mgarch(raif~1,~bekk(1,1))

summary(fut.bekk)

plot(fut.bekk)


library(MTS)
for (i in 1:1214){
  j=1+500
  if (j==1715) break
  else
    fut.bekk=BEKK11(fut.spot[i:j,])   #mgarch(fu.spot[i:j,]~1,~bekk(1,1)) in S-Plus
  a11[i]=coef(fut.bekk)[6]
  a21[i]=coef(fut.bekk)[7]
  a12[i]=coef(fut.bekk)[8]
  a22[i]=coef(fut.bekk)[9]
  b11[i]=coef(fut.bekk)[10]
  b21[i]=coef(fut.bekk)[11]
  b12[i]=coef(fut.bekk)[12]
  b22[i]=coef(fut.bekk)[13]
}


