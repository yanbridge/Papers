library(urca)
library(vars)
library(tseries)
library(foreign)
price2 <- read.csv("~/YanFiles/MyPapers/Price_Discovery/Data_P/price2.csv")
attach(price2)
price <- cbind(open.b50,b50,hs300,if300,a50)
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
par(family='STSong')
par(mfrow=c(2,2),mar=c(2.5,3,2,1))

plot(time,rb50,type="l",xlab="",ylab="",ylim=c(-10,10),las=1,tcl=0.3,
     col=4,main="标普中国50指数收益率 ")  
grid()

plot(time,ra50,type="l",xlab="",ylab="",las=1,tcl=0.3,
     col=4,main="新华富时A50股指期货收益率 ")  
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

ba<-cbind(log(open.b50),log(a50))
colnames(ba)<-c("lnb50","lna50")

ba.a1<-rep(0,1287)
ba.a2<-rep(0,1287)
bar0<-rep(0,1287)
bar1<-rep(0,1287)
sig.sp50 <- rep(0,1287)
sig.a50 <- rep(0,1287)
rho.ba <- rep(0,1287)

for (i in 1:1287){
  j=i+499
  if (j==1787) break
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
hi.a1<-rep(0,1287)
hi.a2<-rep(0,1287)
hir0<-rep(0,1287)
hir1<-rep(0,1287)
sig.hs <- rep(0,1287)
sig.if <- rep(0,1287)
rho.hi <- rep(0,1287)
hi<-cbind(log(hs300),log(if300))
colnames(hi)<-c("lnhs300","lnif300")

for (i in 1:1287){
  j=i+499
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
time1 <- Time[-(1:500)]
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

time1 <- Time[-(1:500)]
plot(time1,ba.cs1,type="l",ylim=c(0,1),col=4, 
     lwd=1,las=1,xlab="",ylab="",tcl=-0.3)
lines(time1,ba.cs2,type="l",col=2,lwd=3)
grid()
text(time1[950],0.62,"新华富时A50股指期货",cex=0.9)
text(time1[940],0.37,"标普中国50指数",cex=0.9)

## hs300 v if300

plot(time1,hi.cs1,type="l",col=4, lwd=1,ylim=c(0,1),
     las=1,xlab="",ylab="",tcl=-0.3)
lines(time1,hi.cs2,lty=1,col="red",lwd=3)
text(time1[1000],0.7,"沪深300指数",cex=0.9)
text(time1[1000],0.3,"沪深300股指期货",cex=0.9)
grid()

###################  caculate IS: b50 vs a50  OK!

ba.is1<-(ba.cs1*sig.sp50+ba.cs2*rho.ba*sig.a50)^2/((ba.cs1*sig.sp50+ba.cs2*rho.ba*sig.a50)^2
                                            +(ba.cs2*sig.a50*sqrt(1-rho.ba^2))^2)
ba.is2<-(ba.cs2*sig.a50*sqrt(1-rho.ba^2))^2/((ba.cs1*sig.sp50+ba.cs2*rho.ba*sig.a50)^2
                                             +(ba.cs2*sig.a50*sqrt(1-rho.ba^2))^2)

plot(time1,ba.is1,type="l",ylim=c(0,1),col=4,las=1,tcl=-0.3)
lines(time1,ba.is2,type="l",col=2,lwd=3)
text(time1[850],0.75,"新华富时A50股指期货",cex=0.9)
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
lines(time1,ba.is.mean2,type="l",lwd=3)


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
lines(time1,hi.is.mean2,type="l",col=2,lwd=2)
text(time1[1000],0.65,"沪深300指数",cex=0.9)
text(time1[1000],0.32,"沪深300股指期货",cex=0.9)
grid()

##### caculate ILS: b50 vs a50

## method 1  OK!
ba.il11<-(ba.is1/ba.is2)*(ba.cs2/ba.cs1)
ba.il21<-(ba.is2/ba.is1)*(ba.cs1/ba.cs2)
ba.ils11<-ba.il11/(ba.il11+ba.il21)
ba.ils21<-ba.il21/(ba.il11+ba.il21)


plot(time1,ba.ils11,type="l",col=4,las=1,tcl=-0.3)
lines(time1,ba.ils21,type="l",col=2,lwd=3)
grid()
text(time1[850],0.75,"新华富时A50股指期货",cex=0.8)
text(time1[850],0.25,"标普中国50指数",cex=0.8)


## method 2
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


####################### Plot ils ###########################
par(family='STSong')
par(mfrow=c(1,2),mar=c(2.5,2.5,1,1))

ba.ils31<-(ba.cs1+ba.is1)/2+0.02
ba.ils32<-(ba.cs2+ba.is2)/2-0.02
plot(time1,ba.ils31,type="l",ylim=c(0,1),col=4,las=1,tcl=-0.3)
lines(time1,ba.ils32,type="l",col=2,lwd=4)
grid()
text(time1[850],0.71,"新华富时A50股指期货",cex=0.8)
text(time1[850],0.28,"标普中国50指数",cex=0.8)

plot(time1,hi.ils1,type="l",col=2,lwd=4,las=1,tcl=-0.3)
lines(time1,hi.ils2,type="l",col=4)
grid()
text(time1[1000],0.83,"沪深300指数",cex=0.8)
text(time1[1000],0.25,"沪深300股指期货",cex=0.8)

######## statistic for all ###
data<-cbind(ba.cs1,ba.cs2,hi.cs1,hi.cs2,ba.is1,ba.is2,hi.is.mean1,hi.is.mean2,ba.ils31,ba.ils32,hi.ils1,hi.ils2)
summary(data)
library(fBasics)
basicStats(data)
####### full bekk (roll)  S-Plus #######

module(finmetrics)

# import data in S-Plus
# import data

### a50 vs b50

rab50<-price2[c("ra50","rb50")]
rab50<-rab50[-1,]


ab.a11<-rep(0,1286)
ab.a21<-rep(0,1286)
ab.a12<-rep(0,1286)
ab.a22<-rep(0,1286)
ab.b11<-rep(0,1286)
ab.b21<-rep(0,1286)
ab.b12<-rep(0,1286)
ab.b22<-rep(0,1286)

p.ab.a11<-rep(0,1286)
p.ab.a21<-rep(0,1286)
p.ab.a12<-rep(0,1286)
p.ab.a22<-rep(0,1286)
p.ab.b11<-rep(0,1286)
p.ab.b21<-rep(0,1286)
p.ab.b12<-rep(0,1286)
p.ab.b22<-rep(0,1286)

for (i in 1:1287){
  j= i+499
  if (j==1787) break
  else
    ab.bekk<-mgarch(rab50[i:j,]~1,~bekk(1,1)) 
  
  ab.a11[i]=coef(ab.bekk)[6]
  ab.a21[i]=coef(ab.bekk)[7]
  ab.a12[i]=coef(ab.bekk)[8]
  ab.a22[i]=coef(ab.bekk)[9]
  ab.b11[i]=coef(ab.bekk)[10]
  ab.b21[i]=coef(ab.bekk)[11]
  ab.b12[i]=coef(ab.bekk)[12]
  ab.b22[i]=coef(ab.bekk)[13]
  
  p.ab.a11<-summary(ab.bekk)$coef[6,4]
  p.ab.a21<-summary(ab.bekk)$coef[7,4]
  p.ab.a12<-summary(ab.bekk)$coef[8,4]
  p.ab.a22<-summary(ab.bekk)$coef[9,4]
  p.ab.b11<-summary(ab.bekk)$coef[10,4]
  p.ab.b21<-summary(ab.bekk)$coef[11,4]
  p.ab.b12<-summary(ab.bekk)$coef[12,4]
  p.ab.b22<-summary(ab.bekk)$coef[13,4]
}

data.ab50<-cbind(ab.a11,ab.a21,ab.a12,ab.a22,ab.b11,ab.b21,ab.b12,ab.b22,
                 p.ab.a11,p.ab.a21,p.ab.a12,p.ab.a22,p.ab.b11,p.ab.b21,p.ab.b12,p.ab.b22)

### if300 vs hs300
rih300<-price2[c("rif300","rhs300")]
rih300<-rih300[-1,]


ih.a11<-rep(0,1286)
ih.a21<-rep(0,1286)
ih.a12<-rep(0,1286)
ih.a22<-rep(0,1286)
ih.b11<-rep(0,1286)
ih.b21<-rep(0,1286)
ih.b12<-rep(0,1286)
ih.b22<-rep(0,1286)

p.ih.a11<-rep(0,1286)
p.ih.a21<-rep(0,1286)
p.ih.a12<-rep(0,1286)
p.ih.a22<-rep(0,1286)
p.ih.b11<-rep(0,1286)
p.ih.b21<-rep(0,1286)
p.ih.b12<-rep(0,1286)
p.ih.b22<-rep(0,1286)


for (i in 1:1287){
  j= i+499
  if (j==1787) break
  else
    ab.bekk<-mgarch(rih300[i:j,]~1,~bekk(1,1)) 
  
  ih.a11[i]=coef(ab.bekk)[6]
  ih.a21[i]=coef(ab.bekk)[7]
  ih.a12[i]=coef(ab.bekk)[8]
  ih.a22[i]=coef(ab.bekk)[9]
  ih.b11[i]=coef(ab.bekk)[10]
  ih.b21[i]=coef(ab.bekk)[11]
  ih.b12[i]=coef(ab.bekk)[12]
  ih.b22[i]=coef(ab.bekk)[13]
  
  p.ih.a11[i]<-summary(ab.bekk)$coef[6,4]
  p.ih.a21[i]<-summary(ab.bekk)$coef[7,4]
  p.ih.a12[i]<-summary(ab.bekk)$coef[8,4]
  p.ih.a22[i]<-summary(ab.bekk)$coef[9,4]
  p.ih.b11[i]<-summary(ab.bekk)$coef[10,4]
  p.ih.b21[i]<-summary(ab.bekk)$coef[11,4]
  p.ih.b12[i]<-summary(ab.bekk)$coef[12,4]
  p.ih.b22[i]<-summary(ab.bekk)$coef[13,4]
}

data.ih<-cbind(ih.a11,ih.a21,ih.a12,ih.a22,ih.b11,ih.b21,ih.b12,ih.b22,
               p.ih.a11,p.ih.a21,p.ih.a12,p.ih.a22,p.ih.b11,p.ih.b21,p.ih.b12,p.ih.b22)

###  a50 vs if300
rai<-price2[c("ra50","rif300")]
rai<-rai[-1,]


ai.a11<-rep(0,1286)
ai.a21<-rep(0,1286)
ai.a12<-rep(0,1286)
ai.a22<-rep(0,1286)
ai.b11<-rep(0,1286)
ai.b21<-rep(0,1286)
ai.b12<-rep(0,1286)
ai.b22<-rep(0,1286)

p.ai.a11<-rep(0,1286)
p.ai.a21<-rep(0,1286)
p.ai.a12<-rep(0,1286)
p.ai.a22<-rep(0,1286)
p.ai.b11<-rep(0,1286)
p.ai.b21<-rep(0,1286)
p.ai.b12<-rep(0,1286)
p.ai.b22<-rep(0,1286)


for (i in 1:1287){
  j= i+499
  if (j==1787) break
  else
    ab.bekk<-mgarch(rai[i:j,]~1,~bekk(1,1)) 
  
  ai.a11[i]=coef(ab.bekk)[6]
  ai.a21[i]=coef(ab.bekk)[7]
  ai.a12[i]=coef(ab.bekk)[8]
  ai.a22[i]=coef(ab.bekk)[9]
  ai.b11[i]=coef(ab.bekk)[10]
  ai.b21[i]=coef(ab.bekk)[11]
  ai.b12[i]=coef(ab.bekk)[12]
  ai.b22[i]=coef(ab.bekk)[13]
  
  p.ai.a11[i]<-summary(ab.bekk)$coef[6,4]
  p.ai.a21[i]<-summary(ab.bekk)$coef[7,4]
  p.ai.a12[i]<-summary(ab.bekk)$coef[8,4]
  p.ai.a22[i]<-summary(ab.bekk)$coef[9,4]
  p.ai.b11[i]<-summary(ab.bekk)$coef[10,4]
  p.ai.b21[i]<-summary(ab.bekk)$coef[11,4]
  p.ai.b12[i]<-summary(ab.bekk)$coef[12,4]
  p.ai.b22[i]<-summary(ab.bekk)$coef[13,4]
}

data.ai<-cbind(ai.a11,ai.a21,ai.a12,ai.a22,ai.b11,ai.b21,ai.b12,ai.b22,
               p.ai.a11,p.ai.a21,p.ai.a12,p.ai.a22,p.ai.b11,p.ai.b21,p.ai.b12,p.ai.b22)






####
library(MTS)
rab<-cbind(price2[c("ra50","rb50")])
rab<-rab[-1,]

ab.a11<-rep(0,1286)
ab.a21<-rep(0,1286)
ab.a12<-rep(0,1286)
ab.a22<-rep(0,1286)
ab.b11<-rep(0,1286)
ab.b21<-rep(0,1286)
ab.b12<-rep(0,1286)
ab.b22<-rep(0,1286)

for (i in 1:1287){
  j=i+499
  if (j==1787) break
  else
    fut.bekk=BEKK11(rab[i:j,])   #mgarch(fu.spot[i:j,]~1,~bekk(1,1)) in S-Plus
  ab.a11[i]=coef(fut.bekk)[6]
  ab.a21[i]=coef(fut.bekk)[7]
  ab.a12[i]=coef(fut.bekk)[8]
  ab.a22[i]=coef(fut.bekk)[9]
  ab.b11[i]=coef(fut.bekk)[10]
  ab.b21[i]=coef(fut.bekk)[11]
  ab.b12[i]=coef(fut.bekk)[12]
  ab.b22[i]=coef(fut.bekk)[13]
}

##################
vol.ai <- read.csv("~/YanFiles/MyPapers/Price_Discovery/Data_P/data_ai.csv")
vol.ab <- read.csv("~/YanFiles/MyPapers/Price_Discovery/Data_P/data_ab.csv")
vol.ih <- read.csv("~/YanFiles/MyPapers/Price_Discovery/Data_P/data_ih.csv")

time2 <- Time[-(1:500)]

par(family='STSong')
par(mfrow=c(1,3),mar=c(2.5,2.5,1.5,1))
plot(time2,vol.ab[,6],type='l',ylim=c(-0.3,0.9),las=1,tcl=-0.3,xlab="",ylab="",col=4,main="(a)",cex.axis=1.2)
lines(time2,vol.ab[,7],col=2,lwd=3)
legend("topleft",lty=c(1,1),lwd=c(1,3),c("新华富时A50股指期货 → 标普中国50指数","标普中国50指数 → 新华富时A50股指期货"),bty="n",cex=1.1,col=c(4,2))
abline(h=0,lty=2)

par(family='STSong')
par(mar=c(2.5,2.5,1,1))
plot(time2,vol.ih[,6],type='l',ylim=c(-0.3,0.9),las=1,tcl=-0.3,xlab="",ylab="",col=4,main="(b)",cex.axis=1.2)
lines(time2,vol.ih[,7],col=2,lwd=3)
legend("topleft",lty=c(1,1),lwd=c(1,3),c("沪深300股指期货 → 沪深300指数","沪深300指数 → 沪深300股指期货"),bty="n",cex=1.1,col=c(4,2))
abline(h=0,lty=2)

par(family='STSong')
par(mar=c(2.5,2.5,1,1))
plot(time2,vol.ai[,6],type='l',ylim=c(-0.3,0.9),las=1,tcl=-0.3,xlab="",ylab="",col=4,main="(c)",cex.axis=1.2)
lines(time2,vol.ai[,7],col=2,lwd=3)
legend("topleft",lty=c(1,1),lwd=c(1,3),c("沪深300股指期货 → 新华富时A50股指期货","新华富时A50股指期货 → 沪深300股指期货"),bty="n",cex=1.1,col=c(4,2))
abline(h=0,lty=2)

####
par(family='STSong')
par(mfrow=c(1,3),mar=c(2.5,2.5,1.5,1))
plot(time2,vol.ab[,3],type='l',ylim=c(-1,0.9),las=1,tcl=-0.3,xlab="",ylab="",col=4,main="(a)",cex.axis=1.2)
lines(time2,vol.ab[,2],col=2,lwd=3)
legend("topleft",lty=c(1,1),lwd=c(1,3),c("新华富时A50股指期货 → 标普中国50指数","标普中国50指数 → 新华富时A50股指期货"),bty="n",cex=1.1,col=c(4,2))
abline(h=0,lty=2)

par(family='STSong')
par(mar=c(2.5,2.5,1,1))
plot(time2,vol.ih[,3],type='l',ylim=c(-1.,0.9),las=1,tcl=-0.3,xlab="",ylab="",col=4,main="(b)",cex.axis=1.2)
lines(time2,vol.ih[,2],col=2,lwd=3)
legend("topleft",lty=c(1,1),lwd=c(1,3),c("沪深300股指期货 → 沪深300指数","沪深300指数 → 沪深300股指期货"),bty="n",cex=1.1,col=c(4,2))
abline(h=0,lty=2)

par(family='STSong')
par(mar=c(2.5,2.5,1,1))
plot(time2,vol.ai[,3],type='l',ylim=c(-1,0.9),las=1,tcl=-0.3,xlab="",ylab="",col=4,main="(c)",cex.axis=1.2)
lines(time2,vol.ai[,2],col=2,lwd=3)
legend("topleft",lty=c(1,1),lwd=c(1,3),c("沪深300股指期货 → 新华富时A50股指期货","新华富时A50股指期货 → 沪深300股指期货"),bty="n",cex=1.1,col=c(4,2))
abline(h=0,lty=2)
