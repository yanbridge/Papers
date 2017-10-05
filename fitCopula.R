library(VineCopula)

t.cop <- tCopula(dim=2)
m.1 <- pobs(as.matrix(cbind(stock.sres,bond.sres)))
m.2 <- pobs(as.matrix(cbind(stock.sres,money.sres)))
m.3 <- pobs(as.matrix(cbind(stock.sres,ex.sres)))
m.4 <- pobs(as.matrix(cbind(money.sres,bond.sres)))
m.5 <- pobs(as.matrix(cbind(ex.sres,bond.sres)))
m.6 <- pobs(as.matrix(cbind(money.sres,ex.sres)))

# t-Copula
t.stock.bond <- fitCopula(t.cop,m.1,method='ml')
summary(t.stock.bond)
t.sb.1<-tCopula(param = 0.0187,dim = 2,df = 92)   #param and df can change
lambda(t.sb.1)

t.stock.money <- fitCopula(t.cop,m.2,method='ml')
summary(t.stock.money)
lambda()

t.stock.ex <- fitCopula(t.cop,m.3,method='ml')
summary(t.stock.ex)

t.bond.money <- fitCopula(t.cop,m.4,method='ml')
summary(t.bond.money)

t.bond.ex <- fitCopula(t.cop,m.5,method='ml')
summary(t.bond.ex)

t.money.ex <- fitCopula(t.cop,m.6,method='ml')
summary(t.money.ex)

# Gumbel Copula
gumbel.cop <- gumbelCopula(3,dim=2)
m <- pobs(as.matrix(cbind(stock.sres,bond.sres)))
g.fit.stock.bond <- fitCopula(gumbel.cop,m,method='irho')
g.fit.stock.bond

# Clayton Copula
clayton.cop <- claytonCopula( dim=2)
m <- pobs(as.matrix(cbind(stock.sres,ex.sres)))
c.fit.stock.bond <- fitCopula(clayton.cop,m,method='ml')
c.fit.stock.bond

# Frank Copula
frank.cop <- frankCopula(3, dim=2)
m <- pobs(as.matrix(cbind(stock.sres,ex.sres)))
f.stock.bond <- fitCopula(frank.cop,m.1,method='ml')
summary(f.stock.bond)

f.stock.money <- fitCopula(frank.cop,m.2,method='ml')
summary(f.stock.money)
lambda()

f.stock.ex <- fitCopula(frank.cop,m.3,method='ml')
summary(f.stock.ex)

f.bond.money <- fitCopula(frank.cop,m.4,method='ml')
summary(f.bond.money)

f.bond.ex <- fitCopula(frank.cop,m.5,method='ml')
summary(f.bond.ex)

f.money.ex <- fitCopula(frank.cop,m.6,method='ml')
summary(f.money.ex)

fra.cop.1 = archmCopula(family="frank", dim=2, param=0.02158)
lambda(fra.cop.1)

frank.cop1 = archmCopula(family="frank", dim=2, param=-5.07387)
persp(frank.cop1, pCopula, main="CDF",
      xlab="u", ylab="v", zlab="C(u,v)")
persp(frank.cop1, dCopula, main="pdf", 
      xlab="u", ylab="v", zlab="c(u,v)")
contour(frank.cop1, pCopula, main="CDF", 
        xlab="u", ylab="v")    
contour(frank.cop1, dCopula, main="pdf", 
        xlab="u", ylab="v")