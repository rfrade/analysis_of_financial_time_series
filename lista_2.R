# Criado por Rafael Frade - 11/09/2018
# Disciplina: Análise de Séries Temporais Financeiras
# Lista 2

############################# Exercicio 1 ############################# 

# cria o vetor de choques com distribuiçao normal, media zero e desvio padrao de 0.07
set.seed(1)
choques = rnorm(499, mean=0, sd=0.07)

# cria o vetor que contera a serie
processoAR = numeric(500)

#O primeiro elemento eh zero
processoAR[1] = 0

# Cria uma sequencia de 500 numeros simulando um AR(1)
for (i in 2:500) {
  # Rt =          0.02 + 0.7 * (Rt-1)            + At
  processoAR[i] = 0.02 + 0.7 * processoAR[i - 1] + choques[i - 1]
}

library(ggplot2)

ggplot(data = data.frame(1:500, processoAR), aes(x=1:500, y=processoAR, group=1)) + geom_line()+ geom_point()

# Como esperado, o ACF de um AR fracamente estacionario decai exponencialmente
acf(processoAR, lag.max=30, main = "ACF de AR(1) simulado")
pacf(processoAR, lag.max=30, main = "PACF de AR(1) simulado")


############################# Exercicio 2 ############################# 

# cria o vetor de choques com distribuiçao normal, media zero e desvio padrao de 0.08
set.seed(2) # valor 2 usado para criar choques diferentes do exercicio anterior
choques = rnorm(500, mean=0, sd=0.08)

# cria o vetor que contera a serie
processoMA = numeric(500)

# o primeiro elemento eh teta_zero + at
processoMA[1] = 0.02 + choques[1]

# Cria uma sequencia de 500 numeros simulando um MA(1)
for (i in 2:500) {
  # Rt =          0.02 + At         + 0.7 * (At-1)
  processoMA[i] = 0.02 + choques[i] + 0.7 * choques[i - 1]
}

plot(processoMA)

ggplot(data = data.frame(1:500, processoMA), aes(x=1:500, y=processoMA, group=1)) + geom_line()+ geom_point()

acf(processoMA, lag.max=30, main = "ACF de MA(1) simulado")
pacf(processoMA, lag.max=30, main = "PACF de MA(1) simulado")


############################# Exercicio 3 ############################# 

# cria o vetor de choques com distribuiçao normal, media zero e desvio padrao de 0.08
set.seed(3) # valor 3 usado para criar choques diferentes dos exercicios anteriores
choques = rnorm(500, mean=0, sd=0.08)

# cria o vetor que contera a serie
processoARMA = numeric(500)

# o primeiro elemento eh zero
processoARMA[1] = 0

# Cria uma sequencia de 500 numeros simulando um ARMA(1, 1)
for (i in 2:500) {
  # Rt =            0.02 + phi1(0.7) * Rt-1        + At         + theta1 (-0.1) * At-1
  processoARMA[i] = 0.02 + 0.7 * processoARMA[i-1] + choques[i] + (-0.1) * choques[i - 1]
}

ggplot(data = data.frame(1:500, processoARMA), aes(x=1:500, y=processoARMA, group=1)) + geom_line()+ geom_point()

# Thus, the ACF of an ARMA(1; 1) model behaves very much like that of an AR(1)
# model except that the exponential decay starts with lag 2 (p. 29)

# The ACF and PACF are not informative in determining the order of an ARMA model (p. 30)

acf(processoARMA, lag.max=30, main = "ACF de ARMA(1, 1) simulado")
pacf(processoARMA, lag.max=30, main = "PACF de ARMA(1, 1) simulado")


############################# Exercicio 4 #############################

# cria o vetor de choques com distribuiçao normal, media zero e desvio padrao de 0.08
set.seed(4) # valor 4 usado para criar choques diferentes dos exercicios anteriores
choques = rnorm(500, mean=0, sd=0.08)

# cria o vetor que contera a serie
passeioAleatorio = numeric(500)

# o primeiro elemento eh 10
passeioAleatorio[1] = 10

# Cria uma sequencia de 500 numeros simulando um AR(1)
for (i in 2:500) {
  # Pt =              = Pt-1                  + At
  passeioAleatorio[i] = passeioAleatorio[i-1] + choques[i]
}

ggplot(data = data.frame(1:500, passeioAleatorio), aes(x=1:500, y=passeioAleatorio, group=1)) + geom_line()+ geom_point()

# Consequently, the series has a strong memory as it remembers all of the past shocks (p. 32)
# Testing for a unit root in the index is relevant if one wishes to verify empirically 
#that the Index follows a random walk with drift
acf(passeioAleatorio, lag.max=30, main = "ACF de Passeio Aleatorio simulado")
pacf(passeioAleatorio, lag.max=30, main = "PACF de Passeio Aleatorio simulado")
