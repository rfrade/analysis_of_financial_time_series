setwd("/home/rafael/Área de Trabalho/arquivos/sociais/10º Semestre/ASTF/listas/lista_6")

## Single-Factor Model, illustration

############################## A

# Import data
c10sp <- read.table("m-excess-c10sp-9003.txt",header=T)
head(c10sp)
dim(c10sp)

market_index <- cbind(rep(1,168), c10sp[,11])
returns <- as.matrix(c10sp[,1:10])

mean.returns <- apply(returns, 2, mean)
sd.returns <- apply(returns, 2, sd)

coeficients <- solve(t(market_index) %*% market_index) %*% t(market_index) %*% returns

row.names(coeficients) <- c("Alpha","Beta")
round(coeficients,2) # show results
# Estimated alphas
alpha <- coeficients[1,]
# Estimated betas
beta <- coeficients[2,]

# Calculate residuals
residuos <- returns - market_index %*% coeficients

# Covariance matrix of residuals
D.coef <- diag(crossprod(residuos)/(168-2))

# R-squared
r.quadrado <- 1 - diag(crossprod(residuos))/diag(crossprod(returns))

# Summary
round(cbind(mean.returns, sd.returns, alpha, beta,sigma.i=D.coef,r.quadrado),3)

# Plots
par(mfrow=c(2,1))
barplot(beta, ylab="betas")
abline(h=1)
barplot(r.quadrado, ylab="R-Quadrado")
par(mfrow=c(1,1))

# Covariance and correlation estimates under the market model
cov.r <- var(c10sp[,11])* outer(beta, beta) + diag(D.coef)
round(cov.r,1)
sd.r <- sqrt(diag(cov.r))
corr.r <- cov.r/outer(sd.r,sd.r)
round(corr.r,1)

# Sample correlation
round(cor(returns),1)

library(xtable)
xtable(round(t(100*w.gmin.data),2))
xtable(round(t(100*w.gmin.model),2))

############################## C

# Global minimum variance portfolio
# Using the market model covariance matrix
w.gmin.model <- solve(cov.r) %*% rep(1,nrow(cov.r))
w.gmin.model <- w.gmin.model/sum(w.gmin.model)
round(t(100*w.gmin.model),2)
# Using the sample covariance matrix
w.gmin.data <- solve(var(returns))%*%rep(1,nrow(cov.r))
w.gmin.data <- w.gmin.data/sum(w.gmin.data)
round(t(100*w.gmin.data),2)

# Residual covariance and correlation matrices
resi.cov <- t(E.hat) %*% E.hat/(168-2)
resi.sd <- sqrt(diag(resi.cov))
resi.cor <- resi.cov/outer(resi.sd,resi.sd)
round(resi.cor[,1:10],2)

############################## D


# Calculate means
rm <- matrix(apply(returns, 2, mean),1)
# Calculate demeaned returns
rtn <- returns - matrix(1,168,1) %*% rm
head(rtn)
# The means of the demeaned returns are zero
apply(rtn, 2, mean)

# Create dummies
drugs <- c(rep(1,4), rep(0,6))
auto <- c(rep(0,4), rep(1,2), rep(0,4))
oil <- c(rep(0,6), rep(1,4))
ind.dum = cbind(drugs, auto, oil)
rownames(ind.dum) <- c(colnames( rtn))
ind.dum

# Sample correlation matrix of the returns
cov.rtn <- var(rtn)
sd.rtn <- sqrt(diag(cov.rtn))
corr.rtn <- cov.rtn/outer(sd.rtn,sd.rtn)
round(corr.rtn, 2)

# Step 1
# OLS estimate of F.hat.o:
F.hat.o <- solve(crossprod(ind.dum)) %*% t(ind.dum) %*% t(rtn)
# OLS residuals:
E.hat.o <- t(rtn) - ind.dum %*% F.hat.o
# Residual variances, matrix D:
diagD.hat.o <- diag((1/167) * (E.hat.o %*% t(E.hat.o)))
round(diagD.hat.o, 2)
# Alternative way to calculate D:
library(matrixStats)
round(rowVars(E.hat.o), 2)
# Matrix D
round(diag(rowVars(E.hat.o)),2)

# Step 2
# Inverse of D:
Dinv.hat <- solve(diag(diagD.hat.o))
# Weights for the factor mimicking portfolios
w <- solve(t(ind.dum) %*% Dinv.hat %*% ind.dum) %*% t(ind.dum) %*% Dinv.hat
colnames(w) <- c(colnames(rtn))
round(t(w),2)
# Refined estimate of factor realizations
F.hat.g <- solve(t(ind.dum) %*% Dinv.hat %*% ind.dum
) %*% t(ind.dum) %*% Dinv.hat %*% t(rtn)
round(cor(t(F.hat.g)),3) # factor correlation matrix

# Residuals of the refined regression:
E.hat.g <- t(rtn) - ind.dum %*% F.hat.g
# Residual variance matrix
diagD.hat.g <- rowVars(E.hat.g)
# Covariance matrix of the estimated factor realizations
cov.ind <- ind.dum %*% var(t(F.hat.g)) %*% t(ind.dum) + diag(diagD.hat.g)
round(cov.ind, 2)
# Correlation matrix
sd.ind <- sqrt(diag(cov.ind))
corr.ind <- cov.ind/outer(sd.ind,sd.ind)
round(corr.ind, 2)

# Plot
# Declare time series object
F.hat.g.ts <- ts(t(F.hat.g), frequency = 12, start = c(1990, 1))
plot(F.hat.g.ts)

# A better plot
library(ggplot2)
library(reshape2)
F.hat.g.ts2 <- data.frame(date = seq(as.Date("1990/1/1"), by = "month",
                                     length.out = length(F.hat.g.ts[,1])), F.hat.g.ts)
F.hat.g.ts2 <- melt(F.hat.g.ts2, 'date')
ggplot(F.hat.g.ts2, aes(x=date, y=value, group=variable, color=variable)) +
  geom_line() + scale_x_date() + facet_grid(variable ~ .) + 
  xlab("Years") + ylab("Factor returns")

# Industry factor model global minimum variance portfolio
w.gmin.ind <- solve(cov.ind) %*% rep(1, nrow(cov.ind))
w.gmin.ind <- w.gmin.ind/sum(w.gmin.ind)
rownames(w.gmin.ind) <- c(colnames(rtn))
colnames(w.gmin.ind) <- "weights"
round(t(100*w.gmin.ind), 1) # weights
sum(t(w.gmin.ind)) # Weights sum to one

xtable(round(t(100*w.gmin.ind), 1))


############################## F

library(stats)
pca.cov <- prcomp(returns)
summary(pca.cov)
round(pca.cov$rotation,3)
xtable(summary(pca.cov))

pca.cor <- prcomp(returns, scale = TRUE)
summary(pca.cor)
round(pca.cor$rotation,3)

# Scree plot
par(mfrow=c(1,2))
screeplot(pca.cov, type = "l", main = "Screeplot (cov matrix)")
screeplot(pca.cor, type = "l", main = "Screeplot (cor matrix)")
par(mfrow=c(1,1))


############################## G

# Factor analysis
fa.ret.none <- factanal(returns, factors=2, rotation = "none")
fa.ret.varimax <- factanal(returns, factors=4, rotation = "varimax")

# Objects in output
names(fa.ret.varimax)

# Loadings
fa.ret.none$loadings
fa.ret.varimax$loadings

# Uniqueness
round(fa.ret.none$uniqueness,3)
round(fa.ret.varimax$uniqueness,3)

# Communalities
round(1-fa.ret.none$uniqueness,3)
round(1-fa.ret.varimax$uniqueness,3)

# Likelihood ratio test
# Test of the hypothesis that 2 factors are sufficient
round(fa.ret.varimax$STATISTIC,3)
fa.ret.varimax$dof
round(fa.ret.varimax$PVAL,3)

# Graphical representation
# plot factor 1 by factor 2
load <- fa.ret.varimax$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(returns),cex=.7) # add variable names 

library(portes)
round(Hosking(as.matrix(returns), lags=c(1,5,10), order=0, squared.residuals = F),3)


library(MTS)
VARorder(as.matrix(log(returns + 10)), maxp = 12, output = T)

############################## H