# Criado por Rafael Frade - 20/08/2018
# Disciplina: Análise de Séries Temporais Financeiras
# Resolução do Exercício 1 - Lista 1

library(quantmod)
library(dygraphs) # Carrega a API dygraphs usada para geraçao de graficos: https://plot.ly/r/
library(PerformanceAnalytics)
library(fBasics)

getSymbols("AXP",from="2003-01-01",to="2014-12-31")
getSymbols("CAT",from="2003-01-01",to="2014-12-31") 
getSymbols("SBUX",from="2003-01-01",to="2014-12-31")

# Calcula o retorno simples
simpleReturnsAXP = CalculateReturns(AXP[,6], method = "simple") 
simpleReturnsCAT = CalculateReturns(CAT[,6], method = "simple") 
simpleReturnsSBUX = CalculateReturns(SBUX[,6], method = "simple") 

# Calcula o retorno multiplicado por 100
simpleReturnsAXP = ajustarPercentual(simpleReturnsAXP)
simpleReturnsCAT = ajustarPercentual(simpleReturnsCAT)
simpleReturnsSBUX = ajustarPercentual(simpleReturnsSBUX)

#Calcula as medias
mean(simpleReturnsAXP, na.rm=TRUE)
mean(simpleReturnsCAT, na.rm=TRUE)
mean(simpleReturnsSBUX, na.rm=TRUE)

#Calcula o desvio padrao
sd(simpleReturnsAXP, na.rm=TRUE)
sd(simpleReturnsCAT, na.rm=TRUE)
sd(simpleReturnsSBUX, na.rm=TRUE)

#Calcula a assimetria
skewness(simpleReturnsAXP, na.rm=TRUE)
skewness(simpleReturnsCAT, na.rm=TRUE)
skewness(simpleReturnsSBUX, na.rm=TRUE)

#Calcula a curtose
kurtosis(simpleReturnsAXP, na.rm=TRUE)
kurtosis(simpleReturnsCAT, na.rm=TRUE)
kurtosis(simpleReturnsSBUX, na.rm=TRUE)

#Calcula o minimo
min(simpleReturnsAXP, na.rm=TRUE)
min(simpleReturnsCAT, na.rm=TRUE)
min(simpleReturnsSBUX, na.rm=TRUE)

#Calcula o maximo
max(simpleReturnsAXP, na.rm=TRUE)
max(simpleReturnsCAT, na.rm=TRUE)
max(simpleReturnsSBUX, na.rm=TRUE)

#Calcula os log-retornos
logAXP = log(simpleReturnsAXP/100 + 1) * 100
logCAT = log(simpleReturnsCAT/100 + 1) * 100
logSBUX = log(simpleReturnsSBUX/100 + 1) * 100

#Calcula as estatisticas para os log retornos
basicStats(logAXP)
basicStats(logCAT)
basicStats(logSBUX)

#Realiza os testes de hipotese

t.test(logAXP, mu=0 , conf.level = 0.95)
t.test(logCAT, mu=0 , conf.level = 0.95)
t.test(logSBUX, mu=0 , conf.level = 0.95)

# Multiplica os retornos por 100
ajustarPercentual <- function(simpleReturns) {
  
  for (i in 1:length(simpleReturns)) {
    simpleReturns[i] = simpleReturns[i] * 100;
  }
  return(simpleReturns)

}