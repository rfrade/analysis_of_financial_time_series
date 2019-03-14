## Rafael Frade - Lista 3

caminho = "/home/rafael/Área de Trabalho/arquivos/sociais/10º Semestre/ASTF/listas/lista_4/HW04/d-pg-0111.txt"

## Exercicio 1.a - Testar se a media e zero
data = read.table(caminho,header=T) # Load data with header
head(data) # the second column brings the simple returns
tail(data)

sp.log = log(data$rtn + 1)

mean(sp.log)
t.test(sp.log, mu=0)

## Exercicio 1.b - Correlaçoes no log retorno
Box.test(sp.log, lag=10, type="Ljung")
acf(sp.log)
pacf(sp.log)

## Exercicio 1.c - ARCH Effect
choques = sp.log - mean(sp.log)
Box.test(choques^2, lag=10, type="Ljung")

library(rugarch)
sp.ar2 = autoarfima(data = sp.log, criterion = "BIC", method="partial", arfima = F)
# ar_1:-0.120789 ar_2: -0.081555

## Exercicio 1.d - Model FIT
spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(2,1)),
                  mean.model=list(armaOrder=c(2,0), include.mean=TRUE),
                  distribution.model="norm")
fit = ugarchfit(spec = spec, data = sp.log)

spec_11 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
                  distribution.model="norm")
fit_11 = ugarchfit(spec = spec_11, data = sp.log)

## Create residuals series after garchFit
r_11<-residuals(fit_11, standardize=F)

## Plots for residuals analysis
acf(r_11, main = "ACF of Standardized Residuals")
pacf(r_11, main = "PACF of Standardized Residuals")
acf(r_11^2, main = "ACF of Standardized Residuals")
pacf(r_11^2, main = "PACF of Standardized Residuals")

length(r_11)

Box.test(x = r_11^2, lag = 10)

