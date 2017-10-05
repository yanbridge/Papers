library(rmgarch)
library(copula)

# 读取数据
fin<-read.table("financial.txt",header=T)
head(fin)
Time=as.Date(fin$date,"%Y-%m-%d")
attach(fin)
summary(fin)
sd(stock)
sd(bond)
sd(ex)
sd(money)

###plot graphic
par(family = 'STFangsong')
#stock
plot(Time,stock,type="l",col=4,xlab="",ylab="",main="沪深300指数")
grid()
#bond
dev.new()
plot(Time,bond,type="l",col=4,xlab="",ylab="",main="中国债券总净价指数")
grid()
dev.new()
plot(Time,money,type="l",col=4,xlab="",ylab="",main="银行间抵押回购利率（一个月)")
grid()
dev.new()
plot(Time,ex,type="l",col=4,xlab="",ylab="",main="人民币兑美元汇率")
grid()

dev.off()  #关闭图形和设置参数
 
library(tseries)
library(timeDate)
skewness(ex)    #bond,ex,money
kurtosis(bond)    #bond,ex,money
acf(stock)         #bond,ex,money
acf(stock^2)       #bond,ex,money
jarque.bera.test(money)
Box.test(stock, lag = 20, type = c("Ljung-Box"))
Box.test(stock^2, lag = 20, type = c("Ljung-Box"))

library(car)
qqPlot(stock, envelope=F) 
qqPlot(stock,distribution="t", df=5, envelope=F) 
x=seq(-3,3,by=0.1)
plot(x,pnorm(x),type="l")

adf.test(stock)    #unit test
library(vars)
stock.adf<-ur.df(stock, type = "none", lags = 5,
      selectlags = c("Fixed", "AIC", "BIC"))    #drift, trend
summary(stock.adf)


library(FinTS) ##arch.lm test
ArchTest(ex,lag=5) #p-value值小于0.05，表示拒绝没有ARCH效应，即存在ARCH效应

#####
spec = ugarchspec(mean.model = list(armaOrder = c(0,0)),
       variance.model = list(garchOrder = c(1,1),
       model = "sGARCH"), distribution.model = "sged")    #distribution:norm,sstd,ged,sged,nig,ghyp,jsu

spec = ugarchspec(mean.model = list(armaOrder = c(1,0)),
       variance.model = list(garchOrder = c(1,1),
       model = "gjrGARCH",submodel=NULL), distribution.model = "sstd")    
#distribution:snorm,std,sstd,ged,sged,nig,ghyp,sju 
#model = eGARCH,fGARCH,gjrGARCH,apARCH,iGARCH and csGARCH
#if model = "fGARCH", then submodel can change to TGARCH, AVGARCH, 
#NGARCH,APARCH,GJRGARCH and ALLGARCH
###stock
fit.stock <- ugarchfit(data = stock, spec = spec)
show(fit.stock)
res.stock<-residuals(fit.stock)
sigma.stock<-sigma(fit.stock)
stock.sres<-res.stock/sigma.stock
stock.sres<-as.vector(stock.sres)   

###bond 
fit.bond <- ugarchfit(data = bond, spec = spec)
show(fit.bond)
res.bond<-residuals(fit.bond)
sigma.bond<-sigma(fit.bond)
bond.sres<-res.bond/sigma.bond
bond.sres<-as.vector(bond.sres) 

##money
fit.money <- ugarchfit(data = money, spec = spec)
show(fit.money)
res.money<-residuals(fit.money)
sigma.money<-sigma(fit.money)
money.sres<-res.money/sigma.money
money.sres<-as.vector(money.sres) 

##ex
spec = ugarchspec(mean.model = list(armaOrder = c(1,0)),
        variance.model = list(garchOrder = c(1,1),
        model = "eGARCH",submodel=NULL), distribution.model = "sstd") 
fit.ex <- ugarchfit(data = ex, spec = spec)
show(fit.ex)
res.ex<-residuals(fit.ex)
sigma.ex<-sigma(fit.ex)
ex.sres<-res.ex/sigma.ex
ex.sres<-as.vector(ex.sres)

fin.sres<-cbind(Time,stock.sres,bond.sres,ex.sres,money.sres)

library(VineCopula)
u <- pobs(as.matrix(cbind(res.stock,res.bond)))[,1]
v <- pobs(as.matrix(cbind(res.stock,res.bond)))[,2]

selectedCopula <- BiCopSelect(u,v,familyset=NA)
selectedCopula
t.cop <- tCopula(dim=2)
set.seed(500)
m <- pobs(as.matrix(cbind(res.stock1,res.bond1)))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)

clayton.cop <- claytonCopula( dim = 2)
fit.1 <- fitCopula(clayton.cop,m,method='ml')

##caculate VaR
stock.var<-mean(stock)-1.95*sigma.stock
bond.var<-mean(bond)-1.95*sigma.bond
ex.var<-mean(ex)-1.95*sigma.ex
money.var<-mean(money)-1.95*sigma.money

var.fin<-cbind(stock.var,bond.var,money.var,ex.var)
colnames(var.fin)<-c("stock","bond","money","fx")

##unit root test, use urca package
library(vars)
library(urca)
summary(ur.df(stock.var, type = "trend", lags = 2))  #type="none","drift","trend"

##select VAR lag
VARselect(var.fin, lag.max = 5, type="const")
var.fin.result <- VAR(var.fin, p = 2, type = "both")
summary(var.fin.result)
fin.irf <- irf(var.fin.result, impulse="bond",response = "stock", n.ahead = 48, boot = TRUE)
plot(fin.irf,main="bond to stock")

###SVAR example
data(Canada) 
var.2c <- VAR(Canada, p = 2, type = "const") 
amat <- diag(4) 
diag(amat) <- NA 
amat[2, 1] <- NA 
amat[4, 1] <- NA ## Estimation method scoring 
svar.1<-SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL, max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) ## Estimation method direct 
svar.2<-SVAR(x = var.2c, estmethod = "direct", Amat = amat, Bmat = NULL, hessian = TRUE, method="BFGS")
plot(irf(svar.a, impulse = "e", response =  "U", ortho = TRUE, cumulative = FALSE, boot = TRUE, ci = 0.99,runs = 100, seed = NULL))

##SVAR for financial market
amat <- diag(4) 
diag(amat) <- NA 
svar.1<-SVAR(x = var.fin.result, estmethod = "scoring", Amat = amat, Bmat = NULL, max.iter = 100, maxls = 1000, conv.crit = 1.0e-8) 




####
library(MSBVAR)
var.lag.specification(var.fin, lagmax=12)

m1 <- msbvar(ts(var.fin), p=1, h=2, lambda0=0.6,
            lambda1=0.1, lambda3=1, lambda4=0.5, lambda5=0,
            mu5=0, mu6=0, qm=12, alpha.prior=matrix(10, 2, 2),
            prior=0, max.iter=20)
m1$hreg[1]
m1$hreg[2]
m2p <- gibbs.msbvar(m1, N1=1000, N2=10000, permute=FALSE, Sigma.idx=1)
irf2 <- mc.irf(m2p, nsteps=12)
plot.ms.irf(irf2)
plot(ts(m1$fp))
print(m1$Q)
plotregimeid(m2p, type="all")

###
xm <- msbvar(ts(var.fin), p=1, h=2,
            lambda0=0.8, lambda1=0.15,
            lambda3=2, lambda4=1, lambda5=0, mu5=0,
            mu6=0, qm=12,
            alpha.prior=matrix(c(100,40,30,50), 2, 2))

plot(ts(xm$fp))
print(xm$Q)
# Now sample the posterior
N1 <- 2000
N2 <- 10000
# First, so this with random permutation sampling
x1 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=TRUE)
# Identify the regimes using clustering in plotregimeid()
plotregimeid(x1, type="all")
x2 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=FALSE, Sigma.idx=1)
# Plot the variances. Note the strict hyperplane between the variances
# for the first equation versus the others.
plotregimeid(xm, x2, type="Sigma")

regimeSummary(m1)

###szbvar
fit.BVAR <- szbvar(ts(var.fin), p=1, z=NULL, lambda0=0.6,
                   lambda1=0.1, lambda3=2, lambda4=0.5, lambda5=0,
                   mu5=0, mu6=0, nu=3, qm=4, prior=0,
                   posterior.fit=FALSE)
summary(fit.BVAR)                   
# Generate unconditional forecasts for both models
forecast.BVAR <- uc.forecast(fit.BVAR, nsteps=12,
                             burnin=1000, gibbs=10000)
# Draw from the posterior pdf of the impulse responses.
posterior.impulses <- mc.irf(fit.BVAR, nsteps=10, draws=5000)
# Plot the responses
varnames=c("stock","bond","ex","money")
plot(posterior.impulses, method=c("Sims-Zha2"), component=1,
probs=c(0.16,0.84), varnames=varnames)

# Plot the forecasts
par(mfrow=c(2,1))
plot(forecast.BVAR$forecast[,,1], probs=c(0.16,0.84),
        main="I2P Forecast")
abline(h=0)
plot(forecast.BVAR$forecast[,,2], probs=c(0.16,0.84),
           main="P2I Forecast")
abline(h=0)

m2 <- msbvar(ts(var.fin), p=1, h=2, lambda0=0.7,
             lambda1=0.1, lambda3=1.1, lambda4=0.6, lambda5=0.2,
             mu5=0, mu6=0, qm=12, alpha.prior=matrix(10, 2, 2),
             prior=0, max.iter=20)
m2$hreg[1]
m2$hreg[2]
m2p <- gibbs.msbvar(m2, N1=1000, N2=10000, permute=FALSE, Sigma.idx=1)
irf2 <- mc.irf(m2p, nsteps=12)
plot.ms.irf(irf2)
plot(ts(m1$fp))
print(m1$Q)
plotregimeid(m2p, type="all")



####example
data(dji30ret)
dat=dji30ret[,1:3,drop=FALSE]
#Copula-Student(non time varying -max Likelihood Estimation of rho)
uspec = spec
spec.1 = cgarchspec(uspec = multispec(replicate(3,uspec)),VAR = T,VAR.opt = list(lag=1,lag.max = 4, lag.criterion =c("AIC","HQ","SC","FPE"),external.regressors = NULL),dccOrder = c(1,1),distribution.model =list(copula=c("mvt"),method=c("ML"),time.varying=FALSE,transformation ="parametric"),start.pars =list(),fixed.pars=list())
fit.c<-cgarchfit(spec=spec.1,data=Dat,spd.control=list(lower=0.1,upper=0.9,type="pwm",kernel="epanech"),fit.control=list(eval.se=TRUE),solver="solnp")
fit.c
