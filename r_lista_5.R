setwd("/home/rafael/Área de Trabalho/arquivos/sociais/10º Semestre/ASTF/R scripts")

########### Exercicio 1

dados = read.table("m-mrk2vw.txt",header=T) # Log returns
head(dados)

library(psych)
describe(dados)

mean(dados)

plot(x = dados[,1], y = dados[,2])
mrk.ts <-ts(dados[,2], start=c(1960,1), end=c(2008,12),frequency=12)
plot(mrk.ts)

library(MTS)
ccm(dados[, -1], lags=6, level=TRUE)

library(portes)

# IBM and SP500 data
# The Modified Multivariate Portmanteau Test, Hosking (1980)
Hosking(cbind(r.sp.ts,r.ibm.ts), lags=c(1,5,10), order=0, squared.residuals = F)

Hosking(as.matrix(dados[, -1]), lags=1:6, order=0, squared.residuals=FALSE)


########### Exercicio 2


bonds1_10 = read.table("m-gs1n10.txt",header=T)

bonds.one.ts  = ts(diff(100*bonds1_10$gs1), start=c(1953,4), end=c(2009,10),frequency=12)
bonds.ten.ts = ts(diff(100*bonds1_10$gs10), start=c(1953,4), end=c(2009,10),frequency=12)

library(MTS)

VARorder(cbind(bonds.one.ts, bonds.ten.ts), maxp = 60, output = T)

# Full model using package 'vars'
bonds.var <- VAR(cbind(bonds.one.ts, bonds.ten.ts), p = 2)
summary(bonds.var)

mq(resid(bonds.var), lag = 12, adj=3) # , adj=20

ccm(cbind(bonds.one.ts, bonds.ten.ts), lags=2, level=TRUE)


############# VMA
bonds.vma <- VMA(cbind(bonds.one.ts, bonds.ten.ts), q = 2, include.mean = T, fixed = NULL,
                  beta=NULL, sebeta=NULL, prelim = F,
                  details = F, thres = 2)

# Refines a fitted VMA model by setting insignificant parameters to zero
bonds.vma.ref = refVMA(bonds.vma, thres = 1.5)

############# EX 3 VARMA
Eccm(cbind(bonds.one.ts, bonds.ten.ts), maxp = 6, maxq = 6, include.mean = T, rev = T)

VARMA(cbind(bonds.one.ts, bonds.ten.ts), p = 2, q = 2, include.mean = T,
      fixed = NULL, beta=NULL, sebeta=NULL,
      prelim = F, details = F, thres = 2)

varma16 <-VARMA(cbind(bonds.one.ts, bonds.ten.ts), p = 1, q = 6, include.mean = T,
               fixed = NULL, beta=NULL, sebeta=NULL,
               prelim = F, details = F, thres = 2)

varma33 <-VARMA(cbind(bonds.one.ts, bonds.ten.ts), p = 3, q = 3, include.mean = T,
                fixed = NULL, beta=NULL, sebeta=NULL,
                prelim = F, details = F, thres = 2)

varma16.ref = refVARMA(varma16, thres = 1)
varma33.ref = refVARMA(varma33, thres = 1)

# Analysis of the residuals
res.varma16<-resid(varma16.ref)
ccm(res.varma16, level=TRUE)

res.varma33<-resid(varma33.ref)
ccm(res.varma33, level=TRUE)

# Correlation
cor(res.varma16)
cor(res.varma33)
