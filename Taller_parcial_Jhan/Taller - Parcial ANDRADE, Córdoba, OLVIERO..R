##____________________________Universidad Nacional de Colombia ___________________________## 
##___________________________  Facultad de Ciencias Econ?micas ___________________________##
##____________________________  T?picos de Econometria II  ________________________________##
##____________________________Taller - Parcial Primer Corte ______________________________##
##_____________________ Jhan J. Andres, Dino Córdoba & Luis Olivero ________________________##
##_________________________________________________________________________________________##

##########################
###     PREGUNTA 4     ###
##########################

#_______________                       
# Punto a              
#_______________

## Se carga el paquete time series, stargazer y dynlm

library(tseries)
library(stargazer)
library(dynlm)
library(urca)
library(uroot)

## Se crea la semilla de datos aleatorios y a partir de ella se simulan unos residuales gaussianos.
set.seed(18933)
obs <- 100
et = ts(rnorm(n = obs, mean = 0, sd = 1))


## Se procede a simular una caminata aleatoria sin deriva y sin tendencia.

x11()
yt = ts(arima.sim(model= list(order = c(0,1,0)), n = obs, innov = et))

## Se grafica la serie simulada.
plot.ts(yt, main = "Gr?fica caminata aleatoria sin deriva", ylab= "Valores", xlab = "Periodo", col = "blue", lwd = 2)
 
## Una vez obtenida la serie yt, se procede a estimar por MCO el modelo #1. 

dyt = diff(yt)
yt_1 = lag(yt)[1:100]
Modelo.1 = lm(dyt~ yt_1, data = yt)
stargazer(Modelo.1, type = "text", title="Modelo #1")

#______________
#Punto b
#______________



## Se procede a simular 10000 veces una caminata aleatoria sin deriva y sin tendencia. 
N <- 10000
set.seed(2782)
obs <- 100
zt = ts(replicate(n=N,arima.sim(model= list(order = c(0,1,0)), n = obs, innov = rnorm(obs, mean = 0, sd =1))))
 

## Una vez se han simulado las 10000 muestras, se estima 10000 veces el modelo #1 por MCO y se obtiene el Rho estimado. 
Rho <- numeric(N)
for(i in 1:ncol(zt)) {
  Rho[i] <- summary(
    dynlm(diff(zt[, i],1) ~ L(zt[, i],1)))$coef[2, 1]
}

Rho

##Una vez se han simulado las 10000 muestras, se estima 10000 veces el modelo #1 por MCO y se obtiene el estad?stico DF estimado. 
DF <- numeric(N)
for(i in 1:ncol(zt)) {
  DF[i] <- summary(
    dynlm(diff(zt[, i],1) ~ L(zt[, i],1)))$coef[2, 3]
}

DF

## Ahora se realizar?n los histogramas para la variable Rho y el estad?stico DF + Drift

x11()

hist(Rho, main="Rho estimado prueba Dickey-Fuller + Drift con N=10000 y n = 100", 
     xlab = "Valor", ylab = "Frecuencia",col = "blue", border="black",xlim =c(-0.4,0.1),ylim=c(0,2500))
 
hist(DF, main="Estad?stico Dickey-Fuller + Drift con N=10000 y n = 100",
     xlab = "Valor", ylab = "Frecuencia",col="red", border = "black",xlim=c(-5,2))


## Ahora se comparar? la densidad del estad?stico DF + Drift con la densidad de una distribuci?n normal

curve(dnorm(x), 
      from = -6, to = 4, 
      ylim = c(0, 0.6), 
      lty = 2,
      ylab = "Densidad",
      xlab = "t-Statistic",
      main = "Funci?n de densidad de la prueba estad?stica DF + Drift",
      col = "red", 
      lwd = 2)

lines(density(DF), lwd = 2, col = "black")
    
## Ahora se determinar?n los cuantiles de cola izquierda para el estad?stico DF

round(quantile(DF, c(0.05, 0.01)),3)

#______________
#Punto c
#______________


## Se procede a simular 10000 veces una caminata aleatoria con deriva y sin tendencia. 
 
N <- 10000
set.seed(27819)
obs <- 100
drift <- 0.5

xt = ts(replicate(n=N,arima.sim(model= list(order = c(0,1,0)), mean = drift,
                                n = obs, innov = rnorm(obs, mean = 0, sd =1))))

## ## Una vez se han simulado las 10000 muestras, se estima 10000 veces por MCO el modelo #1 m?s una tendencia determin?stica lineal y se obtiene el Rho estimado. 
Rho1 <- numeric(N)
for(i in 1:ncol(xt)) {
  Rho1[i] <- summary(
    dynlm(diff(xt[, i],1) ~ L(xt[, i],1) + seq(1:100)))$coef[2, 1]
}

Rho1

##Una vez se han simulado las 10000 muestras, se estima 10000 veces por MCO el modelo #1 y se obtiene el estad?stico DF estimado. 
DF.drift <- numeric(N)
for(i in 1:ncol(zt)) {
  DF.drift[i] <- summary(
    dynlm(diff(zt[, i],1) ~ L(zt[, i],1) + seq(1:100)))$coef[2, 3]
}

DF.drift

## Ahora se realizar?n los histogramas para la variable Rho y el estad?stico DF

x11()
hist(Rho1, main="Rho estimado prueba Dickey-Fuller + Drift + Linear Trend con N=10000 y n = 100", 
     xlab = "Valor", ylab = "Frecuencia",col = "green", border="black", ylim = c(0,4200),xlim = c(-0.4, 0.1))

hist(DF.drift, main="Estad?stico Dickey-Fuller + Drift + Linear Trend con N=10000 y n = 100",
     xlab = "Valor", ylab = "Frecuencia",col="yellow", border = "black",xlim=c(-5,1))


## Ahora se comparar? la densidad del estad?stico DF + Drift + Linear Trend con la densidad de una distribuci?n normal y la densidad del estad?stico DF + Drift

curve(dnorm(x), 
      from = -6, to = 3.5, 
      ylim = c(0, 0.6), 
      lty = 2,
      ylab = "Densidad",
      xlab = "t-Statistic",
      main = "Funci?n de densidad de la prueba estad?stica DF + Drift + Linear Trend",
      col = "red", 
      lwd = 2)

lines(density(DF.drift), lwd = 2, col = "blue")
lines(density(DF), lwd = 2, col = "black")

## Ahora se determinar?n los cuantiles de cola izquierda para el estad?stico DF

round(quantile(DF.drift, c(0.05, 0.01)),3)

#______________
#Punto d
#______________

 ## Una vez se han simulado las 10000 muestras, se estima 10000 veces por MCO un modelo sin deriva y sin tendencia y se obtiene el Rho estimado. 
Rho2 <- numeric(N)
for(i in 1:ncol(xt)) {
  Rho2[i] <- summary(
    dynlm(diff(xt[, i],1) ~ L(xt[, i],1) + 0 ))$coef[1, 1]
}

Rho2

##Una vez se han simulado las 10000 muestras, se estima 10000 veces por MCO un modelo sin deriva ni tendencia y se obtiene el estad?stico DF estimado. 
DF.drift2 <- numeric(N)
for(i in 1:ncol(zt)) {
  DF.drift2[i] <- summary(
    dynlm(diff(zt[, i],1) ~ L(zt[, i],1) + 0 ))$coef[1, 3]
}

DF.drift2

## Ahora se realizar?n los histogramas para la variable Rho y el estad?stico ADF

x11()
hist(Rho2, main="Rho estimado prueba Dickey-Fuller  con N=10000 y n = 100", 
     xlab = "Valor", ylab = "Frecuencia",col = "purple", border="black", ylim = c(0,6000),xlim = c(-0.3, 0.1))

hist(DF.drift2, main="Estad?stico Dickey-Fuller con N=10000 y n = 100",
     xlab = "Valor", ylab = "Frecuencia",col="orange", border = "black",xlim=c(-4,4))


## Ahora se comparar? la densidad del estad?stico DF  con la densidad de una distribuci?n normal y la densidad del estad?stico DF + Drift + Linear Trend 
 
curve(dnorm(x), 
      from = -6, to = 3.5, 
      ylim = c(0, 0.6), 
      lty = 2,
      ylab = "Densidad",
      xlab = "t-Statistic",
      main = "Funci?n de densidad de la prueba estad?stica DF",
      col = "red", 
      lwd = 2)

lines(density(DF.drift2), lwd = 2, col = "green")
lines(density(DF.drift), lwd = 2, col = "blue")
 
## Ahora se determinar?n los cuantiles de cola izquierda para el estad?stico DF

round(quantile(DF.drift2, c(0.05, 0.01)),3)


#______________
#Punto e
#______________

## Partimos de las 10000 seriew simuladas en el literal b

N <- 10000
set.seed(2782)
obs <- 100
zt = ts(replicate(n=N,arima.sim(model= list(order = c(0,1,0)), n = obs, innov = rnorm(obs, mean = 0, sd =1))))

## Ahora procederemos a contaminar cada una de las series con un outlier igual a 20, en el momento t = 50.

Outlier = c(rep(0,49), 20,rep(0,51))
zt.out = zt + Outlier

 
##Una vez se han contaminado las muestras en zt, se estima 10000 veces el modelo #1 por MCO y se obtiene el estad?stico DF estimado. 
DF.out <- numeric(N)
for(i in 1:ncol(zt.out)) {
  DF.out[i] <- summary(
    dynlm(diff(zt.out[, i],1) ~ L(zt.out[, i],1)))$coef[2, 3]
}

DF.out

## Ahora se realizar? el histograma para el estad?stico DF obtenido de las muestras contaminadas en zt. 

x11()
hist(DF.out, main="Estad?stico Dickey-Fuller + Drift para las muestras contaminadas con N=10000 y n = 100",
     xlab = "Valor", ylab = "Frecuencia",col="gray", border = "black",xlim=c(-10,0), ylim = c(0,1500))


## Ahora se comparar? la densidad del estad?stico DF + Drift para las muestras contaminadas con la densidad de una distribuci?n normal y la densidad del estad?stico DF + Drift para las muestras sin contaminar.

curve(dnorm(x), 
      from = -10, to = 3.5, 
      ylim = c(0, 0.6), 
      lty = 2,
      ylab = "Densidad",
      xlab = "t-Statistic",
      main = "Funci?n de densidad de la prueba estad?stica DF + Drift para las muestras contaminadas",
      col = "red", 
      lwd = 2)

lines(density(DF.out), lwd = 2, col = "yellow")
lines(density(DF), lwd = 2, col = "black")

## Ahora se determinar?n los cuantiles de cola izquierda para el estad?stico DF

round(quantile(DF.out, c(0.05, 0.01)),3)

## ?Cu?ntas veces el test D-F es ahora rechazado

a =length(DF.out[DF.out<quantile(DF,c(0.05))])
b = a - 500
b


##########################
###     PREGUNTA 5     ###
##########################


## Se cargan las bases de datos con la serie del PIB real desestacionalizado y de periodicidad trimestral para Brasil
pib = PIB_real_trimestral_desestacionalizado_BRASIL

## Se extrean los valores del PIB como matriz
serie = as.matrix(pib$`PIB real trimestral desestacionalizado`)

#Se convierten en un objeto de serie de tiempo.
serie = ts(serie)

## Se grafica la serie 
plot(pib$Fecha, serie, xlab='Fecha', ylab= 'Valor del PIB', type = 'l', main='PIB real trimestral de la República Federal de Brasil', col = "red")

## Se carga la base de datos de la tasa de cambio
tasa112 = Tasa_de_Cambio_Brasil_U_ltimo_valor_del_mes_enero_2000_a_marzo_2020

## Se extraen los valores como matriz
tasa = as.matrix(tasa112$`Exchange Rate`)

#Se convierten en un objeto de serie de tiempo.
tasa = ts(tasa)

## Se grafica la serie 
plot(tasa112$Date, tasa, xlab='Fecha', ylab= 'Valor de la tasa de Cambio Nominal', type = 'l', main='Tasa de Cambio Nominal para la República Federal de Brasil', col = "red")


#_______________                       
# Punto a              
#_______________

## Para seleccionar el tipo de modelo ARIMA que se va a testear aplicamos los criterios de información AIC dentro del mismo código

#Primero ejecutamos la prueba DF aumentada para el modelo con tendencia y con intercepto
DF.aumentada.tendencia = ur.df(serie,type = "trend", selectlags = "AIC")
summary(DF.aumentada.tendencia)
plot(DF.aumentada.tendencia)


#Posteriormente se aplica la misma prueba pero para un modelo solo con intercepto
DF.aumentada.intercepto = ur.df(serie,type = "drift", selectlags = "AIC")
summary(DF.aumentada.intercepto)

#Se aplica la prueba para el modelo sin componentes determin?sticos
DF.aumentada = ur.df(serie,type = "none", selectlags = "AIC")
summary(DF.aumentada)

#### Tasa de cambio 

## Para seleccionar el tipo de modelo ARIMA que se va a testear aplicamos los criterios de información AIC dentro del mismo código

#Primero ejecutamos la prueba DF aumentada para el modelo con tendencia y con intercepto
DF.aumentada.tendencia.tasa = ur.df(tasa,type = "trend", selectlags = "AIC")
summary(DF.aumentada.tendencia.tasa)
plot(DF.aumentada.tendencia.tasa)


#Posteriormente se aplica la misma prueba pero para un modelo solo con intercepto
DF.aumentada.intercepto.tasa = ur.df(tasa,type = "drift", selectlags = "AIC")
summary(DF.aumentada.intercepto.tasa)

#Se aplica la prueba para el modelo sin componentes determin?sticos
DF.aumentada.tasa = ur.df(tasa,type = "none", selectlags = "AIC")
summary(DF.aumentada.tasa)



#_______________                       
# Punto b             
#_______________

## Se corre la KPSS para
kpss.tau = ur.kpss(serie,type = "tau", lags = "short") ## Dado los resultados de la ADF esta prueba no se incluyó en el análisis
summary(kpss.tau)
kpss.mu = ur.kpss(serie,type = "mu", lags = "short")
summary(kpss.mu)

## Se corre la ERS
y1.ers= ur.ers(serie,type = "DF-GLS", model="trend") ## Dado los resultados de la ADF esta prueba no se incluyó en el análisis
summary(y1.ers)

cy2.ers= ur.ers(serie,type = "DF-GLS", model="constant")
summary(y2.ers)


## Tasa de cambio 

## Se corre la KPSS para
kpss.tau.tasa = ur.kpss(tasa,type = "tau", lags = "short") ##Dado Dado los resultados de la ADF se decidió que esta prueba se decidió no se incluyía en el análisis
summary(kpss.tau.tasa)
kpss.mu.tasa= ur.kpss(tasa,type = "mu", lags = "short")
summary(kpss.mu.tasa)

## Se corre la ERS
y1.ers.tasa= ur.ers(tasa,type = "DF-GLS", model="trend") ##Dado Dado los resultados de la ADF se decidió que esta prueba se decidió no se incluyía en el análisis
summary(y1.ers.tasa)

cy2.ers.tasa= ur.ers(tasa,type = "DF-GLS", model="constant")
summary(cy2.ers.tasa)


#_______________                       
# Punto d             
#_______________

## Orden de integraci?n

serie1 = diff(serie)


## Se grafica la serie 
plot( serie1, ylab= 'Valor del PIB', type = 'l', main='PIB real trimestral de la República Federal de Brasil primera diferencia', col = "red")


## Para seleccionar el tipo de modelo ARIMA que se va a testear aplicamos los criterios de información AIC dentro del mismo código

#Primero ejecutamos la prueba DF aumentada para el modelo con tendencia y con intercepto para la diferencia de la serie
DF.aumentada.tendencia1 = ur.df(serie1,type = "trend", selectlags = "AIC") ## Dado los resultados de la ADF no se incluyó esta prueba para el análisis
summary(DF.aumentada.tendencia1)
plot(DF.aumentada.tendencia1)


#Posteriormente se aplica la misma prueba pero para un modelo solo con intercepto (que representa la pendiente de la tendencia) para la diferencia de la serie
DF.aumentada.intercepto1 = ur.df(serie1,type = "drift", selectlags = "AIC")
summary(DF.aumentada.intercepto1)

#Se aplica la prueba para el modelo sin componentes determin?sticos para la diferencia de la serie
DF.aumentada1 = ur.df(serie1,type = "none", selectlags = "AIC")
summary(DF.aumentada1)

## Tasa de cambio 

tasa1 = diff(tasa)


## Se grafica la serie 
plot( tasa1, ylab= 'Valor nominal de la tasa de cambio', type = 'l', main='Tasa de Cambio Nominal de Brasil 1era diferencia', col = "red")


## Para seleccionar el tipo de modelo ARIMA que se va a testear aplicamos los criterios de información AIC dentro del mismo código

#Primero ejecutamos la prueba DF aumentada para el modelo con tendencia y con intercepto para la diferencia de la serie
DF.aumentada.tendencia1.tasa= ur.df(tasa1,type = "trend", selectlags = "AIC")
summary(DF.aumentada.tendencia1.tasa)
plot(DF.aumentada.tendencia1.tasa)


#Posteriormente se aplica la misma prueba pero para un modelo solo con intercepto (que representa la pendiente de la tendencia)
DF.aumentada.intercepto1.tasa = ur.df(tasa1,type = "drift", selectlags = "AIC")
summary(DF.aumentada.intercepto1.tasa)

#Se aplica la prueba para el modelo sin componentes determin?sticos para la diferencia de la serie
DF.aumentada1.tasa = ur.df(tasa1,type = "none", selectlags = "AIC")
summary(DF.aumentada1.tasa)





#Posteriormente se aplica la misma prueba pero para un modelo solo con intercepto (que representa la pendiente de la tendencia)
DF.aumentada.intercepto1 = ur.df(serie1,type = "drift", selectlags = "AIC")
summary(DF.aumentada.intercepto1)

#Se aplica la prueba para el modelo sin componentes determin?sticos
DF.aumentada1 = ur.df(serie1,type = "none", selectlags = "AIC")
summary(DF.aumentada1)


#_______________                       
# Punto e             
#_______________

#Se ejecuta la prueba Zivot y Andrews 
## PIB en nivel
za = ur.za(serie, model = 'intercept')
summary(za)
plot(za)

za.pib.todo = ur.za(serie, model = 'both')
summary(za.pib.todo)
plot(za.pib.todo)

## Tasa de Cambio en nivel
za.tasa = ur.za(tasa, model = 'intercept')
summary(za.tasa)
plot(za.tasa)

za.tasa.todo = ur.za(tasa, model = 'both')
summary(za.tasa.todo)
plot(za.tasa.todo)

##########################
###     PREGUNTA 6     ###
##########################
## Primero se extreae la base de datos de desempleo
serie.desempleo = ts(desempleo$`Tasa de desempleo`, start = c(2002 , 3), frequency = 12)

## Graficamos 

plot(desempleo$Fecha, serie.desempleo, xlab='Fecha', ylab= 'Tasa de Desempleo', type = 'l', main='Tasa de desempleo mensual de la República Federal de Brasil', col = "blue")

## Para determinar si hay o no componentes deterministicos aplicamos la prueba ADF
DF.aumentada.tendencia1 = ur.df(serie.desempleo ,type = "trend", selectlags = "AIC")
summary(DF.aumentada.tendencia1)
plot(DF.aumentada.tendencia1)


#Posteriormente se aplica la misma prueba pero para un modelo solo con intercepto
DF.aumentada.intercepto1 = ur.df(serie.desempleo,type = "drift", selectlags = "AIC")
summary(DF.aumentada.intercepto1)
#Se aplica la prueba para el modelo sin componentes determinísticos
DF.aumentada1 = ur.df(serie.desempleo,type = "none", selectlags = "AIC")
summary(DF.aumentada1)



## Se aplica la prueba HEGY a la tasa de desempleo
hegy.desempleo = hegy.test(serie.desempleo, deterministic = c(1, 0, 0), lag.method = c('AIC'))
summary(hegy.desempleo)





## En tanto no fue posible conseguir una serie del PIB a precios constantes para el pa?s de Brasil, 
## con frecuencia trimestral y sin desestacionalizar, se utilizar? el Indicador de Actividad Econ?mica (IAE)
## del Banco Central de Brasil para el periodo Enero de 2003 a Febrero de 2020, el cual tiene una frecuencia
## mensual y toma como periodo base el ao 2002 = 100

#Cargamos la base de datos

library(readxl);library(tseries); library(urca)
y = read_excel(file.choose())
IAE = ts(y$IAE,start=c(2003,1),frequency=12)

#Graficamos la serie

x11()
plot.ts(IAE, main="Indicador de Actividad Econ?mica Banco Central de Brasil", xlab="periodo", ylab="valor",lwd = 2 , col = "blue")
plot.ts(diff(IAE), main="Variaci?n Indicador de Actividad Econ?mica Banco Central de Brasil", xlab="periodo", ylab="valor",lwd = 2 , col = "red")

 
#Ahora vamos a realizar una prueba D-F sobre la serie para identificar si la 
#serie tiene componentes determin?sticos.

DF.trend = ur.df(IAE,type = "trend", selectlags = "AIC")
summary(DF.trend) #La serie tiene al menos una ra?z unitaria, pero no tiene tendencia determin?stica lineal

DF.drift = ur.df(IAE,type = "drift", selectlags = "AIC")
summary(DF.drift) #La serie no tiene intercepto, aunque s? al menos una ra?z unitaria


##Realizamos la prueba HEGY

##Con base en los resultados de la prueba ADF, haremos la prueba para una serie sin tendencia. No
##obstante, como la prueba debe tener al menos un componente determin?stico, incluiremos intercepto

library(uroot)
hegy.test(IAE, deterministic = c(1, 0, 0), lag.method = c('AIC'))

