##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2020-II
#      SESIÓN MONITORIA : Metodologia Box-Jenkins para la identificación, 
#               estimación y pronóstico de series de tiempo univariadas
#                   Serie modelada: PIB de los Estados Unidos      
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

##########################################
#       PIB de los Estados Unidos        #
##########################################

# Vamos a trabajar con una serie del PIB real trimestral de Estados Unidos con datos de la FED.
# En este caso usamos datos desestacionalizados para poder aplicar el método.

# Nota: Los datos del PIB real de los estados unidos se pueden encontrar directamente en:
# https://fred.stlouisfed.org/series/GDPC1

#PASO 1- IDENTIFICACIÓN

# En R uno puede trabajar series de tiempo con 3 objetos:
# ts
# zoo
# xts

# Se carga la serie de tiempo
y <- read_excel(file.choose())
PIB <- ts(y[,2], start = 1947, frequency = 4) # Se coloca 1 para indicar que la frecuencia es anual 

# Visualización de los datos
View(PIB)  

# Crear un objeto tipo xts
PIB_xts = as.xts(PIB) # Convertir el objeto ts en un objeto xts

# Ver la clase que son cada uno de los ejemplos de series de tiempo
class(PIB)
class(PIB_xts)

#Vamos a graficar el PIB de Estados Unidos en Nivel
x11()   # Comando para hacer la gráfica en una ventana externa 

# Grafica para el objeto ts
## Usando plot.ts
plot.ts(PIB, xlab="",ylab="", main="PIB real trimestral de Estados Unidos 1947-2018)",lty=1, lwd=2, col="blue")
## Usando autoplot (de la libreria ggplot2)
autoplot(PIB, col="red", main = "PIB real de los Estados Unidos - 1947 a 2012",xlab = "Fecha", ylab="", lwd=2)

# Grafica para el objeto xts
x11()
plot(PIB_xts, main = "PIB real de Estados Unidos - Frecuencia trimestral", ylab  = "PIB real")

#Vamos a graficar la ACF y PACF de la serie en nivel.
lags=24
par(mfrow=c(1,2))
acf(PIB,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PIB real de USA') 
pacf(PIB,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PIB Real de USA')
# Versíón ggplot:
ggAcf(PIB,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA")
ggPacf(PIB,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")
# Versión paquete astsa (sirve tanto para objetos ts como para objetos xts)
acf2(PIB)
acf2(PIB_xts)

# Prueba de Augment Dickey Fuller

#Como evidenciamos por la ACF de la serie en nivel se tiene que el PIB real de los Estados Unidos un proceso altamente persistente, 
# es evidente que la serie no es I(0). No obstante, vamos a hacer la prueba ADF secuencial para validar dicha intuición gráfica.

#Primero vamos a hacer una prueba de ADF con tendencia e intercepto.Los valores críticos de la prueba son sensibles a 
#la inclusión de términos determinísticos, de manera que hay que determinar si la serie tiene tendencia y/o intercepto. 
#Es importante que recuerden que el tau es una prueba de cola izquierda, mientras que phi es una prueba de cola derecha.

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi3 me dice si la tendecia es 
#significativa o no, y por tanto, si el el test de raíz unitaria debería incluir o no tendencia. 
adf.trend= ur.df(PIB, type="trend", selectlags = "AIC")
summary(adf.trend) 

# El tau nos dice que la serie tiene al menos una raíz unitaria, mientras el phi3 nos dice que la tendencia 
# no es significativa a un 5 % de significancia. 

#El tau me dice si la serie tiene o no al menos una raíz unitaria. El phi1 me indica si la deriva es
#significativa, y por consiguiente, si se debe incluir en el test de raíz unitaria. 
adf.drift= ur.df(PIB, type="drift", selectlags = "AIC")
summary(adf.drift) 

#Los resultados indican que la serie tiene al menos una raíz unitaria. El phi1, por su parte, indica que 
#la deriva de la serie es significativa, por tanto, no es necesario hacer una prueba
# sin términnos determinísticos.

#Claramente se evidencia que la serie tiene al menos una raíz unitaria, en tanto no se rechaza la hipótesis nula. 
#Noten la importancia de determinar si la serie tiene términos detemrminísticos, pues el valor calculado en cada
#especificación de la prueba cambió de forma importante.

# Por completitud se coloca la prueba sin intercepto ni deriva (No obstante, no es necesario realizarla dados
# los resultados en el phi1 y el tau de la prueba anterior que indica que el proceso tiene raíz unitaria
# y además el término de deriva es significativo)
# Esta prueba se realizaría si la hipótesis nula para el phi1 no se hubiera rechazado 
adf.none= ur.df(PIB, type="none", selectlags = "AIC")
summary(adf.none) 

#Por consiguiente, procedemos a diferenciar la serie. La inclusión de logaritmos es recomendable
#para estabilizar la varianza, es decir, cuando la serie tiene alta volatilidad. 
d.PIB= diff(PIB)             # serie diferenciada
l.PIB=log(PIB)               # serie que se le aplica solo el logaritmo 
dl.PIB= diff(log(PIB))*100   # diferencia de logaritmos de la serie (tasa de crecimiento)

# creacion del logaritmo y la tasa de crecimiento para el PIB como objeto xts
l.PIB_xts = log(PIB_xts)
dl.PIB_xts = diff(log(PIB_xts))

#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
par(mfrow=c(2,2))
plot.ts(PIB, xlab="",ylab="", main="PIB en nivel para Estados Unidos 1947-2012",lty=1, lwd=2, col="blue")
plot.ts(l.PIB, xlab="",ylab="", main="PIB en logaritmo para Estados Unidos 1947-2012",lty=1, lwd=2, col="black")
plot.ts(d.PIB, xlab="",ylab="", main="Variación del PIB para Estados Unidos 1947-2012",lty=1, lwd=2, col="red")
plot.ts(dl.PIB, xlab="",ylab="", main="Tasa de crecimiento del PIB para Estados Unidos 1947-2012",lty=1, lwd=2, col="green")

#Vamos a elegir la tasa de crecimiento del ITCR debido a que la varianza es mucho más estable, pese a 
#tener la misma media constante que la serie en primera diferencia.

# Ojo es muy importante a la hora de seleccionar un modelo ARIMA para explicar el proceso generador de datos 
# usar tanto la gráfia de la serie, las ACF y la PACF y los criterios de información 

#Ahora hacemos la ACF y la PACF, en donde evidenciamos un proceso débilmente dependiente. 
lags=30
par(mfrow=c(1,2))
acf(dl.PIB,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la tasa de crecimiento del PIB') 
pacf(dl.PIB,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la tasa de crecimiento del PIB')
# Usando ggplot
ggAcf(dl.PIB,lag.max=lags,plot=T,lwd=2) + ggtitle('ACF de la tasa de crecimiento del PIB')
ggPacf(dl.PIB,lag.max=lags,plot=T,lwd=2) + ggtitle('PACF de la tasa de crecimiento del PIB')
#usando el paquete astsa
acf2(dl.PIB)

# grafica de la tasa de crecimiento del PIB
par(mfrow=c(1,1))
plot(dl.PIB_xts)

#Aún así, lo vamos a confirmar con una ADF con término deterministico pues parece que la serie tiene. 
ADF.dl.PIB <- ur.df(dl.PIB, type="drift", selectlags = "AIC")
summary(ADF.dl.PIB) #Rechazamos H0, así que la tasa de crecimiento del PIB es estacionaria en sentido débil. 

#Ahora procederemos a hacer la identificación del posible PGD subyacente según los criterios de información, puesto 
#que en este caso no es sencillo hacerlo gráficamente. 

#Vamos a analizar las estadísticas descriptivas de la serie en primera diferencia. 
describe(dl.PIB) #Parece que la media es distina a cero, por eso se incluye un intercepto en los arima
describe(l.PIB)
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
# Notese que acá estoy utilizando la serie l.PIB dado que el parámetro d = 1 
# me diferencia la serie
df_PIB_1 = arma_seleccion_df(l.PIB, AR.m, MA.m, d = 1, TRUE, "ML")
view(df_PIB_1)

# Acá estoy trabajando con la serie en logartimos y diferenciada 
# Es decir, con una tasa de cambio 
df_PIB_0 = arma_seleccion_df(dl.PIB, AR.m, MA.m, d = 0, TRUE, "ML")
view(df_PIB_0)

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
min_aic_1 = arma_min_AIC(df_PIB_1); min_aic_1
min_bic_1 = arma_min_BIC(df_PIB_1); min_bic_1

# Para cuando uso la serie dl.ITCR (d = 0 para la serie diferenciada en logaritmos)
min_aic_0 = arma_min_AIC(df_PIB_0); min_aic_0
min_bic_0 = arma_min_BIC(df_PIB_0); min_bic_0

# La mejor opción para la serie en logaritmos 
# para el proceso AIC es un proceso ARIMA(6,1,3) mientras para el BIC es un ARIMA(1,1,3)

# La mejor opción para la tasa de crecimiento 
# para el proceso AIC es un proceso ARIMA(5,0,6) mientras para el BIC es un ARIMA(1,0,0)

# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast

auto.arima(l.PIB, method = "ML")

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

# De acá en adelante, trabajaremos con los resultados de df_PIB_1. Es decir para los modelos calculados 
# por el AIC y el BIC cuando se uso d = 1 para la serie l.PIB dado que es mejor que la función arima haga
# automáticamente la diferenciación a la hora de hacer la estimación

# Estimación del ARIMA arima6.1.3 para l.PIB y del ARIMA arima6.0.3 para dl.PIB (Usando AIC)

## Estimación por maxima verosimilitud (ML por sus siglas en inglés)
arima6.1.3_ML <- arima(l.PIB, order = c(6,1,3), include.mean = F, method = "ML");summary(arima6.1.3_ML)
arima6.0.3_ML <- Arima(dl.PIB, order = c(6,0,3), include.mean = F, method = "ML");summary(arima6.0.3_ML)

## Estimación por condidional sum of squares (CSS por sus siglas en inglés)
arima6.1.3_CSS <- Arima(l.PIB, order = c(6,1,3), include.mean = F);summary(arima6.1.3_CSS)
arima6.0.3_CSS <- arima(dl.PIB, order = c(6,0,3), include.mean = F);summary(arima6.0.3_CSS)
# Los resultados muestran que alguan vez la estimación por el método de CSS no converge 
# mientras que por ML si lo puede hacer

## Verificar los fitted values dados por estimación por ML 
fitted(arima6.1.3_ML) # método 1 para calcular los valores ajustados

# Estimación del ARIMA arima1.1.3 para l.PIB y del ARIMA arima1.0.3 para dl.PIB (Usando BIC)

## Estimación por maxima verosimilitud (ML por sus siglas en inglés)
arima1.1.3_ML = arima(l.PIB, order = c(1,1,3), include.mean = F, method = "ML");summary(arima1.1.3_ML)
arima1.0.3_ML = Arima(dl.PIB, order = c(1,0,3), include.mean = F, method = "ML");summary(arima1.0.3_ML)

## Estimación por condidional sum of squares (CSS por sus siglas en inglés)
arima1.1.3_CSS = Arima(l.PIB, order = c(1,1,3), include.mean = F);summary(arima1.1.3_CSS)
arima1.0.3_CSS = Arima(dl.PIB, order = c(1,0,3), include.mean = F);summary(arima1.0.3_CSS)

## Verificar que me dan casi los mismos fitted values por ML o por CSS
fitted(arima1.1.3_ML) # método 1 para calcular los valores ajustados
l.PIB - arima1.1.3_CSS$residuals # método 2 para calcular los valores ajustados

### Usando el comando sarima para estimar tanto el ARIMA(6.1.3) como el ARIMA(0.1.1)
# El comando sarima solo estima por medios de CSS (no hace ML)
## No puede converger para el modelo ARIMA(6,1,3) porque el método de estimación de SARIMA es CSS y no converge 
## para el modelo ARIMA(6,1,3) dicho método de estimación para esta serie
sarima(l.PIB, p = 6, d = 1, q = 3, no.constant = TRUE) # Me da los mismos valores estimados que arima6.1.3_CSS
sarima(l.PIB, p = 1, d = 1, q = 3, no.constant = TRUE) # Me da los mismos valores estimados que arima1.1.3_CSS


# De acá en adelante se trabajará con las estimaciones dados por el método de máxima verosimilitud. 
# Es decir, se trabajará con los modelos arima6.1.3_ML y con arima1.1.3_ML

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
lags.test = length(l.PIB)/4;lags.test

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

# arima1.1.3_ML

## ACF y PACF para el modelo arima0.1.1_ML

x11()
acf2(residuals(arima1.1.3_ML))

## #Como evidenciamos que la ACF y PACF los residuales del modelo ARIMA(1,1,3) 
# no presentan autocorrelación

## Pruebas formales para ver si existe autocorrelación en los residuales del modelo arima6.1.3_ML

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(l.PIB)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima1.1.3_ML),lag=lags.test, type = c("Box-Pierce")) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima1.1.3_ML),type='Box-Pierce',lag=20) #No rechazo H0, se cumple el supuesto. 
Box.test(residuals(arima1.1.3_ML),type='Box-Pierce',lag=30) #No rechazo H0, se cumple el supuesto.
# Test  Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima1.1.3_ML),lag=lags.test, type = c("Ljung-Box")) #No rechazo H0, se cumple el supuesto.
Box.test(residuals(arima1.1.3_ML),type='Ljung-Box',lag=20) #No rechazo H0, se cumple el supuesto.
Box.test(residuals(arima1.1.3_ML),type='Ljung-Box',lag=30) #No rechazo H0, se cumple el supuesto.

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima1.1.3_ML, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

#Ahora vamos a analizar el supuesto de normalidad. 

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima1.1.3_ML$residuals
qqPlot(Residuales)
#Prueba Jarque-Bera
jarque.bera.test(residuals(arima1.1.3_ML)) #Se rechaza H0, no hay normalidad. 

#PASO 4: PRONÓSTICO. (Supongamos que se satisface el supuesto de normalidad)

#arima6.1.3_ML (AIC)

# Pronóstico pasos adelante 

## Utilizando el comando forecast del paquete forecast 
forecast.l.PIB <- forecast(arima6.1.3_ML, lead = 12, alpha = 0.05,output = T)   # Pronóstico 12 pasos adelante.
forecast.l.PIB

## Utilizando el comando sarima.for del paquete astsa
sarima.for(l.PIB, n.ahead = 12, p = 6, d = 1, q = 3)

#Ahora vamos a ver el ajuste dentro de muestra

## Para la series l.PIB
fit_1 <- l.PIB - residuals(arima6.1.3_ML)
## Para la series dl.PIB
fit_0 <- dl.PIB - residuals(arima6.0.3_ML)

#Predicción sobre la muestra

x11()
plot.ts(l.PIB,type="l",main="log(PIB) ajustada VS log(PIB) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

x11()
plot.ts(dl.PIB,type="l",main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#arima1.1.3_ML (BIC)

# Pronóstico pasos adelante 

## Utilizando el comando forecast del paquete forecast 
forecast.l.PIB <- forecast(arima1.1.3_ML, lead = 12, alpha = 0.05,output = T)   # Pronóstico 12 pasos adelante.
forecast.l.PIB

## Utilizando el comando sarima.for del paquete astsa
sarima.for(l.PIB, n.ahead = 12, p = 6, d = 1, q = 3)

#Ahora vamos a ver el ajuste dentro de muestra

## Para la series l.ITCR
fit_1 <- l.PIB - residuals(arima1.1.3_ML)
## Para la series dl.ITCR
fit_0 <- dl.PIB - residuals(arima1.0.3_ML)

#Ajuste sobre la muestra

x11()
plot.ts(l.PIB,type="l",main="log(PIB) ajustada VS log(PIB) observada",lwd=2)
points(fit_1,col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

x11()
plot.ts(dl.PIB,type="l",main="Tasa de crecimiento ajustada VS Tasa de crecimeinto observada",lwd=2)
points(fit_0,col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#Nota

# Dado que ambos modelos dan casi un mismo ajuste y 
# son igual de buenos (en términos de validación de supuestos) 
# se prefiere el modelo arima(1,1,3) al ser más parsimonioso que el modelo arima(6,1,3) 


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
