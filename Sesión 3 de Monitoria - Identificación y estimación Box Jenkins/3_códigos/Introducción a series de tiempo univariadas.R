##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA : Metodología Box-Jenkins para la identificación y
#                     estimación de series  de tiempo univariadas
##____________________________________________________________________________________
##_____________________________________________________________________________________

#Limpiamos el environment.
remove(list = ls())

#Activamos algunos paquetes que vamos a utilizar en toda la metodología

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
library(gridExtra)   # Para presentar gráficas de ggplot como matrices
library(grid)        # Para presentar gráficas de ggplot como matrices
library(ggplot2)
#---------------------------

###Vamos a simular un Proceso Generador de Datos (PGD) para la variable y_t

#Supondremos que dicha variable sigue una caminata aleatoria pura (sin drift) y con autocorrelación de segundo orden

set.seed(9821) #Para que los valores de la simulación no cambien
obs =  350 #Número de observaciones
y_t = ts(arima.sim(model= list(order = c(2,1,0), ar=c(0.6,0.15)), n = obs, innov=rnorm(n = obs, mean = 0, sd = 1))) #Con este comando simulamos la serie

##Ahora graficamos la serie donde claramente se evidencia una tendencia estocástica en la serie y_t

#Comando estándar para graficar en R
x11()
plot.ts(y_t, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,1,0)",lty=1, lwd=2, col="red")

#Gráficas más estéticas con ggplot
x11()
autoplot(y_t, xlab="",ylab="", main=" Simulación de una serie y_t que sigue un PGD ARIMA (2,1,0)",lty=1, lwd=2, col="red")


##Vamos a graficar la ACF y la PACF donde evidenciamos un proceso altamente persistente, pues
#incluso con 20 rezagos la correlación persiste: el proceso no es débilmente dependiente. 
#Inicialmente lo haremos de forma gráfica en la medida en que conocemos el PGD. No obstante,
#cuando trabajemos con datos reales tenemos que hacer pruebas formales para determinar si una
#serie es estacionaria o no. 

#Versión estándar de R
lags=20
par(mfrow=c(1,2))
acf(y_t,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de y_t') 
pacf(y_t,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de y_t')

#Versión ggplot
ggAcf(y_t,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF de y_t")
ggPacf(y_t,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF de y_t")

#PASO 1 - IDENTIFICACIÓN: REALIZAR TRANSFORMACIONES SOBRE y_t para que se vuelva estacionaria

log(y_t)  #Se utiliza para estabilizar varianza, sobre todo cuando hay alta volatilidad en las series.
dify_t = diff(y_t) #Diferenciamos para eliminar la tendencia estocástica

#Únicamente vamos a graficar la primera diferencia debido a que la serie toma valores negativos
x11()
plot.ts(dify_t, xlab="",ylab="", main="Primera diferencia de y_t",lty=1, lwd=2, col="green")

#Vamos a aplicar el test de raíz unitaria Dickey-Fuller, el cual nos dice si una serie tiene al menos una raíz unitaria o no.
#La hipótesis nula es que la serie tiene al menos una raíz unitaria (no es estacionaria), mientras la hipótesis 
#alternativa dice que es estacionaria. 

DF.Test = ur.df(dify_t, type="none", selectlags = "AIC")
summary(DF.Test) #Rechazo la hipótesis nula, asi que dify_t es estacionaria. 

#Ahora para seguir con el proceso de identficación, vamos a graficar los correlalogramas para la serie diferenciada.
#Claramente se evidencia que las autocorrelaciones simples caen rápidamente a cero, mientras la PACF trunca en 2, 
#indicando que la serie diferenciada sigue un proceso ARIMA (2,0,0)

lags=20
par(mfrow=c(1,2))
acf(dify_t,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de la primera diferencia de y_t') 
pacf(dify_t,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de la primera diferencia de y_t')

#El análisis gráfico nos dice que dify_t sigue un proceso AR(2), lo cual es consistente con la simulación realizada.
#Sin embargo, en la vida real los Procesos Generadores de Datos no se conocen y el análisis gráfico puede no dar 
#certeza absoluta del número de rezagos autorregresivos y de promedio móvil.


#Una herramienta muy utilizada para la identificación son los criterios de información. Los criterios de información
#permiten comparar el ajuste entre dos modelos, de manera que el mejor modelo será aquel que tenga el criterio de 
#información menor, debido a que tiene una menor varianza en los residuales y por ende en las estimaciones. Los criterios
#penalizan la inclusión de variables irrelevantes si estas no aportan poder explicativo al modelo.

#Ahora calcularemos los criterios de información AIC & BIC para un conjunto de modelos

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 4 #Supondremos que el rezago autorregresivo máximo es 4 (pmax)
MA.m <- 4 #Supondremos que el rezago de promedio móvil máximo es 4. (qmax)

describe(dify_t) #Como la media incondicional no es cero incluiremos intercepto en la estimación

# función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC correspondientes a dicho modelo
# Ojo: utlizar method = "ML" dentro de arima(), de lo contrario les arrojará error

arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}


# Llamo la función arma_selection_df para construir un data frame con todos los posibles modelos
# ARIMA(p, d, q) donde p y q varían entre 0 y 4. Nótese que acá estoy utilizando la serie y_t 
#dado que el órden de integración d = 1  me diferencia la serie

mod.d1 = arma_seleccion_df(y_t, AR.m, MA.m, d = 1, TRUE, "ML")
view(mod.d1)

#Acá estoy trabajando con la serie diferenciada debido a que d=0

mod.d0 = arma_seleccion_df(dify_t, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod.d0)


# Creo la función arma_min_AIC para seleccionar el mejor modelo según AIC.
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# Creo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

#Aplicamos las funciones cuando d=1
min_aic_1 = arma_min_AIC(mod.d1); min_aic_1 #ARIMA (2,1,0)
min_bic_1 = arma_min_BIC(mod.d1); min_bic_1 #ARIMA (2,1,0)


##SEGUNDO PASO- ESTIMACIÓN: Una vez se ha identificado la serie, se procede a estimar el modelo ARIMA. Debe tomarse en
#cuenta que incluir muy pocos rezagos puede llevar a que los residuales del modelo no se comporten como un Ruido Blanco y que incluir 
#muchos parámetros puede llevar a que nuestro modelo no sea parsimonioso/se pierdan muchos grados de libertad y se pierda eficiencia.

# Existen 3 métodos de estimación para la función arima: (por default los 3 comandos utilizan CSS)
## ML: Máxima verosimilitud (más preciso y usualmente la mejor opción para bases pequeñas donde se pueda encontrar una solución analítica)
## CSS: (más veloz generalmente. se usa como aproximación o para bases de datos grandisimas)
## CSS-ML: Una combinación de ambas

### ML puede no converger
### CSS puede no hacer estimaciones lo suficientemente precisas y arrojar error 

# Para más información ver: 
# https://stats.stackexchange.com/questions/209730/fitting-methods-in-arima#:~:text=CSS%20sets%20the%20initial%20observations,an%20estimate%20of%20these%20values.

AR.2 <- arima(y_t, order = c(2,1,0), include.mean=TRUE, method = "ML")

#Como podemos evidenciar, ambos coeficientes autorregresivos son muy cercanos a sus valores teóricos.
summary(AR.2)

#Veamos la ACF y PACF de los residuales, donde observamos que se comportan como un RB
x11()
lags=20
par(mfrow=c(1,2))
acf(residuals(AR.2),lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales') 
pacf(residuals(AR.2),lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales')

#Analizamos la gráfica del ajuste dentro de muestra
x11()
plot.ts(y_t,type="l",main="Serie ajustada vs. Serie observada - MA",lwd=2)
points(fitted.values(AR.2),col="red",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","red"),lty=1,lwd=2)


#¿Qué pasa si estimamos un ARIMA (1,1,0) o un ARIMA (0,1,1)?

AR.1 <- arima(y_t, order = c(1,1,0), include.mean=TRUE, method="ML") 
MA.1 <- arima(y_t, order = c(0,1,1), include.mean=TRUE, method="ML")
summary(AR.1)
summary(MA.1)

#Vemos las ACF y PACF respectivas
x11()
lags=20
par(mfrow=c(2,2))
acf(residuals(AR.1),lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales del AR(1)') 
pacf(residuals(AR.1),lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales del AR(1)')
acf(residuals(MA.1),lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales del MA(1)') 
pacf(residuals(MA.1),lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales del MA(1)')

#Cuando se estima el modelo equivocado, los residuales de dicho modelo no se comportan como un Ruido Blanco. 
#En el primer caso se omite un rezago autorregresivo, mientras en el segundo no se incluye ninguno. Las estimaciones
#de estos últimos modelos son inconsistentes en tanto hubo estructuras de correlación en la serie que no 
#se capturaron adecuadamente. No obstante, esto es muy fácil decirlo con simulaciones porque se conoce el PGD, pues 
#cuando se trabaja con datos dicho reales el PGD es desconocido. 

########################
#      EJEMPLO 2       #
########################

#Vamos a trabajar con una serie Anual del PIB real de Estados Unidos con datos de la FED. Utilizaremos datos 
#anuales para no tener que tomar en cuenta los efectos estacionales que afectan generalmente a las series 
#mensuales o trimestrales.

z <- read_excel(file.choose())
PIB.US <- ts(z[,2], start = 1929, frequency = 1)
View(PIB.US)  

#PASO 1- IDENTIFICACIÓN

#Vamos a graficar el PIB de Estados Unidos en Nivel
x11()
autoplot(PIB.US, xlab="",ylab="", main="PIB real de Estados Unidos 1929-2018)",lty=1, lwd=2, col="blue")


##Vamos a calcular la ACF y la PACF para el PIB de Estados Unidos en Nivel. El proceso es altamente persistente, pues dada
#la frecuencia de la serie, aún 20 años después la correlación es estadísticamente diferente de cero. 
lags=20
par(mfrow=c(1,2))
acf(PIB.US,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del PIB USA') 
pacf(PIB.US,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del PIB USA')


#Vamos a graficar ahora su nivel, su variación, su tasa de crecimiento y su valor en logaritmos.
x11()
grid.arrange(a, b, c, d, ncol=2)
a=autoplot(PIB.US, xlab="",ylab="", main="PIB real de Estados Unidos 1929-2018 en Nivel)",lty=1, lwd=2, col="blue")
b=autoplot(diff(PIB.US), xlab="",ylab="", main="Variación PIB real de Estados Unidos 1929-2018)",lty=1, lwd=2, col="red")
c=autoplot(diff(log(PIB.US)), xlab="",ylab="", main=" Tasa de crecimiento PIB real de Estados Unidos 1929-2018)",lty=1, lwd=2, col="green")
d=autoplot(log(PIB.US), xlab="",ylab="", main=" logaritmo PIB real de Estados Unidos 1929-2018)",lty=1, lwd=2, col="black")

#La serie que evidencia un mejor comportamiento es la tasa de crecimiento del PIB, en tanto permite que la media sea 
#constante y estabiliza la varianza. No obstante, para sustentar lo anterior, realizaremos la estimación desde el año
#1947 para evitar el periodo de la gran depresión y parte de la SGM que alteraron notablemente al crecimiento económico.

# Ojo: Para definir la tasa de crecimiento de la serie de interés aplicamos primero el logaritmo y luego la diferenciación
#      Es decir, la tasa de crecimiento se entiende como: log(x_{t+1}) - log(x_{t})

##Definimos la serie en tasas de crecimiento como:
GDP.gr = diff(log(PIB.US))[18:89]   # Sirve para seleccionar la porción de la serie que nos interesa
Cr.PIB = ts(GDP.gr,start = 1947, frequency = 1)

##Graficamos nuevamente la serie
x11()
autoplot(Cr.PIB, xlab="",ylab="", main=" Tasa de crecimiento PIB real de Estados Unidos 1947-2018)",lty=1, lwd=2, col="black")

##Graficamos su ACF y PACF
lags=20
par(mfrow=c(1,2))
acf(Cr.PIB,lag.max=lags,plot=T,lwd=2,xlab='',main='ACF del crecimiento del PIB USA') 
pacf(Cr.PIB,lag.max=lags,plot=T,lwd=2,xlab='',main='PACF del crecimiento del PIB USA')

#Si bien las gráficas podrían indicar un proceso de ruído blanco,vamos a evaluar otras opciones. Por 
#consiguiente, vamos a utilizar los criterios de información para determinar los rezagos más adecuados.
#Para indicar si incluimos o no intercepto, analizamos la tasa de crecimiento promedio de la economía 
#(su media incondicional), la cual es del 3%
mean(Cr.PIB)

#Utilizaremos la misma función que empleamos en el ejemplo con la serie simulada. Así pues, definimos el modelo que
#vamos a estimar para el conjunto máximo de rezagos ar y ma ya especificados.Hacemos la integrada d=0 debido a que 
#estamos trabajando con la serie diferencias

mod.GDP = arma_seleccion_df(Cr.PIB, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod.GDP)

#Seleccionamos los modelos con menor criterio AIC y menor criterio BIC
min_aic_GDP = arma_min_AIC(mod.GDP); min_aic_GDP #ARIMA (0,0,0) #Ruido Blanco
min_bic_GDP = arma_min_BIC(mod.GDP); min_bic_GDP #ARIMA (0,0,0) #Ruido Blanco

#PASO 2 - ESTIMACIÓN

#Vamos a estimar el modelo ARIMA(0,0,0), pero también la 2da mejor opción ARIMA(1,0,0) y ARIMA(0,0,1)

RB.GDP <- arima(Cr.PIB, order = c(0,0,0), include.mean=TRUE, method="ML") 
AR.GDP <- arima(Cr.PIB, order = c(1,0,0), include.mean=TRUE, method="ML") 
MA.GDP <- arima(Cr.PIB, order = c(0,0,1), include.mean=TRUE, method="ML")
summary(RB.GDP)
summary(AR.GDP)
summary(MA.GDP)

#Vemos las ACF y PACF respectivas, donde observamos que se comportan como un RB
x11()
lags=20
par(mfrow=c(3,2))
acf(residuals(RB.GDP),lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales del RB') 
pacf(residuals(RB.GDP),lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales del RB')
acf(residuals(AR.GDP),lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales del AR(1)') 
pacf(residuals(AR.GDP),lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales del AR(1)')
acf(residuals(MA.GDP),lag.max=lags,plot=T,lwd=2,xlab='',main='ACF de los residuales del MA(1)') 
pacf(residuals(MA.GDP),lag.max=lags,plot=T,lwd=2,xlab='',main='PACF de los residuales del MA(1)')

##Analizamos el ajuste por dentro de muestra

#Ruido Blanco
x11()
plot.ts(Cr.PIB,type="l",main="Tasa de crecimiento ajustada vs. Tasa de crecimiento observada - RB",lwd=2)
points(fitted.values(RB.GDP),col="green",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","green"),lty=1,lwd=2)

#AR(1)
x11()
plot.ts(Cr.PIB,type="l",main="Tasa de crecimiento ajustada vs. Tasa de crecimiento observada - AR",lwd=2)
points(fitted.values(AR.GDP),col="red",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","red"),lty=1,lwd=2)

#MA(1)
x11()
plot.ts(Cr.PIB,type="l",main="Tasa de crecimiento ajustada vs. Tasa de crecimiento observada - MA",lwd=2)
points(fitted.values(AR.GDP),col="blue",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","blue"),lty=1,lwd=2)

#NOTA: Si queremos un modelo con mejor ajuste dentro de muestra, debemos trabajar con series que tengan menor 
#volatilidad, o estimar un modelo menos parmisomonioso. 

ARMA3.2 <- arima(Cr.PIB, order = c(3,0,2), include.mean=TRUE, method="ML")
summary(ARMA3.2)

x11()
plot.ts(Cr.PIB,type="l",main="Tasa de crecimiento ajustada vs. Tasa de crecimiento observada - ARMA3.2",lwd=2)
points(fitted.values(ARMA3.2),col="orange",lwd=2, type = "l")
legend("topleft",c("observada", "estimada"), col=c("black","orange"),lty=1,lwd=2)
