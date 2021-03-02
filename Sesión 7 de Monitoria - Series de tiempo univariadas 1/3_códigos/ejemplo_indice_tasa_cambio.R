##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2020-II
#      SESIÓN MONITORIA : Metodologia Box-Jenkins para la identificación, 
#               estimación y pronóstico de series de tiempo univariadas
##____________________________________________________________________________________
##_____________________________________________________________________________________

#Limpiamos el environment.
remove(list = ls())

#Activamos algunos paquetes que vamos a utilizar en toda la metodolog?a

library(forecast)    # Para hacer pronósticos con modelos arima
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(urca)        # Prueba de raíz unitaria
library(tseries)     # Para estimar modelos de series de tiempo y hacer pruebas de supuestos.
library(readxl)      # Para leer archivos de excel
library(stargazer)   # Para presentar resultados más estéticos.
library(psych)       # Para hacer estadísticas descriptivas
library(seasonal)    # Para desestacionalizar series
library(aTSA)        # Para hacer la prueba de efectos ARCH
library(astsa)       # Para estimar, validar y hacer pronósticos para modelos ARIMA/SARIMA  
library(xts)         # Para utilizar objetos xts 
library(tidyverse)   # Conjunto de paquetes (incluye dplyr y ggplot2)
library(readxl)      # Para leer archivos excel 
library(car)         # Para usar la función qqPlot

################################
# Índice de Tasa de Cambio Real#
################################

#PASO 1: IDENTIFICACIÓN

# En R uno puede trabajar series de tiempo con 3 objetos:
  # ts
  # zoo
  # xts

#Vamos a cargar la serie.
ITCR <- ts(read_excel(file.choose()), start=c(1987,1), frequency=12)

#Analizamos las estadísticas de la serie en nivel.
describe(ITCR) #Si el ITCR está por encima de 100 indica depreciación real. 

# Crear un objeto tipo xts
ITCR_xts = as.xts(ITCR) # Convertir el objeto ts en un objeto xts

# Ver la clase que son cada uno de los ejemplos de series de tiempo
class(ITCR)
class(ITCR_xts)

#Gráficas de las series en nivel
x11() # comando para hacer la gráfica en una ventan externa 
# Grafica para el objeto ts
autoplot(ITCR, col="red", main = "Índice de Tasa de Cambio Real de Colombia- Enero 1987 a Enero 2020",xlab = "Fecha", ylab="", lwd=2)
# Grafica para el objeto xts
plot(ITCR_xts)

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(ITCR,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del ITCR') 
pacf(ITCR,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del ITCR')
# Versíón ggplot:
ggAcf(ITCR,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del ITCR")
ggPacf(ITCR,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del ITCR")
# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(ITCR)
acf2(ITCR_xts)

# Prueba de Augment Dickey Fuller

#Como evidenciamos un proceso altamente persistente, es evidente que la serie no es I(0). No obstante, 
#vamos a hacer la prueba ADF secuencial para validar dicha intuición gráfica.

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf.trend= ur.df(ITCR, type="trend", selectlags = "AIC")
summary(adf.trend) 
#El tau nos dice que la serie tiene al menos una raíz unitaria, mientras el phi3 nos dice que la tendencia 
#no es significativa. Por lo tanto, vamos a mirar únicamente la prueba con intercepto.

#El tau me dice si la serie tiene o no al menos una ra?z unitaria. El phi2 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de ra?z unitaria. 
adf.drift= ur.df(ITCR, type="drift", selectlags = "AIC")
summary(adf.drift) 
#Los resultados indican que la serie tiene al menos una raíz unitaria. El phi2, por su parte, indica que 
#la deriva de la serie no es significativa, por tanto, debe hacerse una prueba sin términnos determinísticos.

#Esta es la correcta especificación de la prueba dado que los términos determinísticos no son significativos. 
adf.none= ur.df(ITCR, type="none", selectlags = "AIC")
summary(adf.none) 
#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto no se rechaza la hipótesis nula. 
#Noten la importancia de determinar si la serie tiene términos detemrminísticos, pues el valor calculado en cada
#especificación de la prueba cambió de forma importante.

#Por consiguiente, procedemos a diferenciar la serie. La inclusión de logaritmos es recomendable
#para estabilizar la varianza, es decir, cuando la serie tiene alta volatilidad. 
d.ITCR= diff(ITCR)             # serie diferenciada
l.ITCR=log(ITCR)               # serie que se le aplica solo el logaritmo 
dl.ITCR= diff(log(ITCR))*100   # diferencia de logaritmos de la serie (tasa de crecimiento)

# creacion del logaritmo y la tasa de crecimiento para la ITCR como objeto xts
l.ITCR_xts = log(ITCR_xts)
dl.ITCR_xts = diff(log(ITCR_xts))

#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
par(mfrow=c(2,2))
plot.ts(ITCR, xlab="",ylab="", main="ITCR en nivel para Colombia 1987-2020",lty=1, lwd=2, col="blue")
plot.ts(l.ITCR, xlab="",ylab="", main="ITCR en logaritmo para Colombia 1987-2020",lty=1, lwd=2, col="black")
plot.ts(d.ITCR, xlab="",ylab="", main="Variación del ITCR para Colombia 1987-2020",lty=1, lwd=2, col="red")
plot.ts(dl.ITCR, xlab="",ylab="", main="Tasa de crecimiento del ITCR para Colombia 1987-2020",lty=1, lwd=2, col="green")

#Vamos a elegir la tasa de crecimiento del ITCR debido a que la varianza es mucho más estable, pese a 
#tener la misma media constante que la serie en primera diferencia.

# Ojo es muy importante a la hora de seleccionar un modelo ARIMA para explicar el proceso generador de datos 
# usar tanto la gráfia de la serie, las ACF y la PACF y los criterios de información 

#Ahora hacemos la ACF y la PACF, en donde evidenciamos un proceso débilmente dependiente. 
lags=30
par(mfrow=c(1,2))
acf(dl.ITCR,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la tasa de crecimiento del ITCR') 
pacf(dl.ITCR,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la tasa de crecimiento del ITCR')
# Usando ggplot
ggAcf(dl.ITCR,lag.max=lags,plot=T,lwd=2) + ggtitle('ACF de la tasa de crecimiento del ITCR')
ggPacf(dl.ITCR,lag.max=lags,plot=T,lwd=2) + ggtitle('PACF de la tasa de crecimiento del ITCR')
#usando el paquete astsa
acf2(dl.ITCR)

# grafica de la tasa de crecimiento del ITCR
par(mfrow=c(1,1))
plot(dl.ITCR_xts)

#Aún así, lo vamos a confirmar con una ADF sin términos deterministicos pues es evidente que la serie no los tiene. 
ADF.dl.ITCR <- ur.df(dl.ITCR, type="none", selectlags = "AIC")
summary(ADF.dl.ITCR) #Rechazamos H0, así que la tasa de crecimiento del ITCR es estacionaria en sentido débil. 


#Ahora procederemos a hacer la identificación del posible PGD subyacente según los criterios de información, puesto 
#que en este caso no es sencillo hacerlo gráficamente. 

#Vamos a analizar las estadísticas descriptivas de la serie en primera diferencia. 
describe(dl.ITCR) #No se debe incluir intercepto, pues la media de la serie es cero. 

# Método manual para identificar el ARIMA usando criterios de información 

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 6 #Supondremos que el rezago autorregresivo máximo es 6 (pmax)
MA.m <- 6 #Supondremos que el rezago de promedio móvil máximo es 6. (qmax)

# función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC
# ojo utlizar method = "ML" dentro de arima(), de lo contrario les arrojará error
arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      #print(c(p, q, AIC(fitp), BIC(fitp)))
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

# Llamo la función arma_selection_df para construir un data frame con todos los posibles modelos
# ARIMA(p, d, q) donde p y q varian entre 0 a 6
# Notese que acá estoy utilizando la serie l.ITCR dado que el parámetro d = 1 
# me diferencia la serie
df_ITCR_1 = arma_seleccion_df(l.ITCR, AR.m, MA.m, d = 1, FALSE, "ML")
view(df_ITCR_1)
# Acá estoy trabajando con la serie en logartimos y diferenciada 
# Es decir, con una tasa de cambio 
df_ITCR_0 = arma_seleccion_df(dl.ITCR, AR.m, MA.m, d = 0, FALSE, "ML")
view(df_ITCR_0)

# Utilizo las función arma_min_AIC para seleccionar el mejor modelo según AIC
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# Utilizo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

# Para cuando uso la serie l.ITCR (d = 1 para la serie solo en logaritmos)
min_aic_1 = arma_min_AIC(df_ITCR_1); min_aic_1
min_bic_1 = arma_min_BIC(df_ITCR_1); min_bic_1

# Para cuando uso la serie dl.ITCR (d = 0 para la serie diferenciada en logaritmos)
min_aic_0 = arma_min_AIC(df_ITCR_0); min_aic_0
min_bic_0 = arma_min_BIC(df_ITCR_0); min_bic_0

#La mejor opción para el AIC es un proceso ARIMA(6,1,3) mientras para el BIC es un ARIMA(0,1,1)

# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast

auto.arima(l.ITCR, method = "ML")

#PASO #2: ESTIMACIÓN.

# Existen 3 diferentes funciones que pueden utilizar para hacer la estimación: 
## arima
## Arima (del paquete forcasts)
## sarima (del paquete astsa) 

# Existen 3 métodos de estimación para la función arima: (por default los 3 comandos utilizan CSS)
## ML: Máxima verosimilitud (más preciso generalmente y usualmente la mejor opción)
## CSS: (más veloz generalmente. se usa como aproximación o para bases de datos grandisimas)
## CSS-ML: Una combinación de ambas

### A veces ML no puede converger
### A veces CSS no puede hacer estimaciones lo suficientemente precisas y arroja error 

# Para más información ver: 
# https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima#:~:text=CSS%20sets%20the%20initial%20observations,an%20estimate%20of%20these%20values.

# Estimación del ARIMA arima6.1.3 para l.ITCR y del ARIMA arima6.0.3 para dl.ITCR (Usando AIC)

## Estimación por maxima verosimilitud (ML por sus siglas en inglés)
arima6.1.3_ML <- arima(l.ITCR, order = c(6,1,3), include.mean = F, method = "ML");summary(arima6.1.3_ML)
arima6.0.3_ML <- Arima(dl.ITCR, order = c(6,0,3), include.mean = F, method = "ML");summary(arima6.0.3_ML)

## Estimación por condidional sum of squares (CSS por sus siglas en inglés)
arima6.1.3_CSS <- Arima(l.ITCR, order = c(6,1,3), include.mean = F);summary(arima6.1.3_CSS)
arima6.0.3_CSS <- arima(dl.ITCR, order = c(6,0,3), include.mean = F);summary(arima6.0.3_CSS)

## Verificar que me dan casi los mismos fitted values por ML o por CSS
fitted(arima6.1.3_ML) # método 1 para calcular los valores ajustados
l.ITCR - arima6.1.3_CSS$residuals # método 2 para calcular los valores ajustados

# Estimación del ARIMA arima0.1.1 para l.ITCR y del ARIMA arima0.0.1 para dl.ITCR (Usando BIC)

## Estimación por maxima verosimilitud (ML por sus siglas en inglés)
arima0.1.1_ML = arima(l.ITCR, order = c(0,1,1), include.mean = F, method = "ML");summary(arima0.1.1_ML)
arima0.0.1_ML = Arima(dl.ITCR, order = c(0,0,1), include.mean = F, method = "ML");summary(arima0.0.1_ML)

## Estimación por condidional sum of squares (CSS por sus siglas en inglés)
arima0.1.1_CSS = Arima(l.ITCR, order = c(0,1,1), include.mean = F);summary(arima0.1.1_CSS)
arima0.0.1_CSS = Arima(dl.ITCR, order = c(0,0,1), include.mean = F);summary(arima0.0.1_CSS)

## Verificar que me dan casi los mismos fitted values por ML o por CSS
fitted(arima0.1.1_ML) # método 1 para calcular los valores ajustados
l.ITCR - arima0.1.1_CSS$residuals # método 2 para calcular los valores ajustados

### Usando el comando sarima para estimar tanto el ARIMA(6.1.3) como el ARIMA(0.1.1)
# El comando sarima solo estima por medios de CSS (no hace ML)
sarima(l.ITCR, p = 6, d = 1, q = 3, no.constant = TRUE) # Me da los mismos valores estimados que arima6.1.3_CSS
sarima(l.ITCR, p = 0, d = 1, q = 1, no.constant = TRUE) # Me da los mismos valores estimados que arima0.1.1_CSS


# De acá en adelante se trabajará con las estimaciones dados por el método de máxima verosimilitud. 
# Es decir, se trabajará con los modelos arima6.1.3_ML y con arima0.1.1_ML

#PASO #3:  VALIDACIÓN. 

# El supuesto más importante que se debe validar es que los residuales estimados se comporten como un ruido blanco. 
# Es decir, que la media de los residuales sea cero, la varianza constante y la covarianza sea cero

# Vamos a realizar el análisis de residuales para cada modelo 

# arima6.1.3_ML

## ACF y PACF para el modelo arima6.1.3_ML

x11()
acf2(residuals(arima6.1.3_ML))

## #Como evidenciamos que la ACF y PACF los residuales del modelo ARIMA(6,3,1) 
# no presentan autocorrelación

## Pruebas formales para ver si existe autocorrelación en los residuales del modelo arima6.1.3_ML

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.ITCR)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima6.1.3_ML),lag=lags.test, type = c("Box-Pierce")) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima6.1.3_ML),type='Box-Pierce',lag=20) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima6.1.3_ML),type='Box-Pierce',lag=30) #No rechazo H0, se cumple el supuesto.
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima6.1.3_ML),lag=lags.test, type = c("Ljung-Box")) #No rechazo H0, se cumple el supuesto.
Box.test(residuals(arima6.1.3_ML),type='Ljung-Box',lag=20) #No rechazo H0, se cumple el supuesto.
Box.test(residuals(arima6.1.3_ML),type='Ljung-Box',lag=30) #No rechazo H0, se cumple el supuesto.

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima6.1.3_ML, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

#Ahora vamos a analizar el supuesto de normalidad. 

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima6.1.3_ML$residuals
qqPlot(Residuales)
#Prueba Jarque-Bera
jarque.bera.test(residuals(arima6.1.3_ML)) #Se rechaza H0, no hay normalidad. 

# arima0.1.1_ML

## ACF y PACF para el modelo arima0.1.1_ML

x11()
acf2(residuals(arima0.1.1_ML))

## #Como evidenciamos que la ACF y PACF los residuales del modelo ARIMA(0,1,1) 
# no presentan autocorrelación

## Pruebas formales para ver si existe autocorrelación en los residuales del modelo arima6.1.3_ML

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.ITCR)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima0.1.1_ML),lag=lags.test, type = c("Box-Pierce")) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima0.1.1_ML),type='Box-Pierce',lag=20) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima0.1.1_ML),type='Box-Pierce',lag=30) #No rechazo H0, se cumple el supuesto.
# Test  Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima0.1.1_ML),lag=lags.test, type = c("Ljung-Box")) #No rechazo H0, se cumple el supuesto.
Box.test(residuals(arima0.1.1_ML),type='Ljung-Box',lag=20) #No rechazo H0, se cumple el supuesto.
Box.test(residuals(arima0.1.1_ML),type='Ljung-Box',lag=30) #No rechazo H0, se cumple el supuesto.

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima0.1.1_ML, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

#Ahora vamos a analizar el supuesto de normalidad. 

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima0.1.1_ML$residuals
qqPlot(Residuales)
#Prueba Jarque-Bera
jarque.bera.test(residuals(arima0.1.1_ML)) #Se rechaza H0, no hay normalidad. 


#PASO 4: PRONÓSTICO. (Supongamos que se satisface el supuesto de normalidad)

#arima6.1.3_ML (AIC)

# Pronóstico pasos adelante 

## Utilizando el comando forecast del paquete forecast 
forecast.l.ITCR <- forecast(arima6.1.3_ML, lead = 12, alpha = 0.05,output = T)   # Pronóstico 12 pasos adelante.
forecast.l.ITCR

## Utilizando el comando sarima.for del paquete astsa
sarima.for(l.ITCR, n.ahead = 12, p = 6, d = 1, q = 3)

#Ahora vamos a ver el ajuste dentro de muestra

## Para la series l.ITCR
fit_1 <- l.ITCR - residuals(arima6.1.3_ML)
## Para la series dl.ITCR
fit_0 <- dl.ITCR - residuals(arima6.0.3_ML)

#Predicción sobre la muestra

x11()
plot.ts(l.ITCR,type="l",main="log(ITCR) ajustada VS log(ITCR) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

x11()
plot.ts(dl.ITCR,type="l",main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#arima0.1.1_ML (BIC)

# Pronóstico pasos adelante 

## Utilizando el comando forecast del paquete forecast 
forecast.l.ITCR <- forecast(arima0.1.1_ML, lead = 12, alpha = 0.05,output = T)   # Pronóstico 12 pasos adelante.
forecast.l.ITCR

## Utilizando el comando sarima.for del paquete astsa
sarima.for(l.ITCR, n.ahead = 12, p = 6, d = 1, q = 3)

#Ahora vamos a ver el ajuste dentro de muestra

## Para la series l.ITCR
fit_1 <- l.ITCR - residuals(arima0.1.1_ML)
## Para la series dl.ITCR
fit_0 <- dl.ITCR - residuals(arima0.0.1_ML)

#Ajuste sobre la muestra

x11()
plot.ts(l.ITCR,type="l",main="log(ITCR) ajustada VS log(ITCR) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

x11()
plot.ts(dl.ITCR,type="l",main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#Nota

# Dado que ambos modelos dan casi un mismo ajuste y 
# son igual de buenos (en términos de validación de supuestos) 
# se prefiere el modelo arima(0,1,1) al ser más parsimonioso que el modelo arima(6,1,3) 


# Notas adicionales 

#### Las series muy volátiles (sobre todo las financieras son difíciles de modelar con métodos ARMA)

#### Existen otros métodos de estimación para series de tiempo muy volatiles o con varianza no estacionaria
#### como son los procesos Arch y Garch
#### Un ejemplo son las series de tiempo Colcap y la Wilshire 5000 price index

#### Dichas series en algunas circusntasancias podrían ser esencialmente díficles de predecir o incluso
#### imposible pero el cambio en su varianza a lo largo de la serie si se podría modela (mediande modelos
#### arch o garch)


#### De igual forma, dado que el ARMA esperar modelar una serie estacionaria en covarianza
#### se observa que la serie pronósticada tiene una varianza más constante y pequeña
#### Por lo que se podría decir que el proceso ARMA suavizo la serie de tiempo original.
