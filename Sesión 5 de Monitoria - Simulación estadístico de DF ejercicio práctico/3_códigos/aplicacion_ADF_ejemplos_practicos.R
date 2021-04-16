##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA: Ejemplos prácticos de uso de pruebas de ADF sobre series reales
##____________________________________________________________________________________
##_____________________________________________________________________________________

# Limpiar el Environment
# rm(list = ls()) 

# Importación de paquetes
library(forecast)
library(lmtest)      # Significancia individual de los coeficientes ARIMA
library(tseries)
library(stargazer)
library(dynlm)    # Paquete que me permite realizar MCOD
library(urca)     # Paquete que me permite realizar pruebas de raíz unitaria 
library(tidyverse)  #Conjunto de paquetes que incluyen ggplot2 para realizar la parte gráfica

# Nota: Para poder usar la función autoplot requiero los paquetes: tidyverse, forecast y lmtest

##
# Procedimiento para determianr el orden de intregración (estacionaridad de una serie): 
##
##### 1. Mirar la gráfica de la serie (la gráfica da una idea general del comportamiento de la serie)
##### 2. Mirar la ACF y la PACF de la serie (El comportamiento de la ACF podría indicar si el proceso es altamente persistente)
##### 3. Conducir la prueba secuencial de Dickey Fuller (de ser necesario)
        # Ojo: Conducir la prueba con las especificaciones correctas 
        #      Tener en cuenta que la prueba es sensible a la inclusión de términos determínisticos
##### 4. Ver si luego de diferenciar la serie la serie diferenciada es estadicionar
        # (Usando de nuevo un test de DF. Si no es estacionario mirar si una nueva diferencia puede 
        #  estacionarizar la serie)

# Nota: Recuerden que un test de DF 
# Nota2: En la prática, generalmente, no se usa solo el test de DF sino sus resultados se corroboran con prueba de raiz unitaria complementarias
       # Un ejemplo, es el uso de la prueba de kpss: cuya hipótesis nula es que la serie es estacionaria
       #                                      (fijense que ésto es contrario a la prueba de DF donde la hipótesis nula es no estacionaridad)
# Nota3: Es importante que la serie a estudiar no presente cambio estructural. 
       # De presentar la serie cambio estructural deja de ser válida la prueba de DF 
# Nota4: Los valores críticos que ofrece 

#---- Ejemplo práctico de uso de pruebas de ADF 1: Ejemplo de Bernhard Pfaff: logaritmo del consumo UK  ---- 

# Se trabajara con el logartimo del consumo para el Reino Unido
# La serie es trimestral (De ahí que frequency = 4)
# Nota: La serie ya ha sido desestacinalizada por lo que no hay que hacer ajuste por estacionalidad 

data(Raotbl3) # Base de datos que se encuentra en el paquete urca
glimpse(Raotbl3)  # Visualizar la base de datos

# 1. Gráfico la serie
lc = ts(Raotbl3$lc, start = c(1966, 4), end = c(1991, 2), frequency = 4) # creación del objeto ts para trabajar con series de tiempo en R
x11()
autoplot(lc, col="blue", main = "Consumo real del Reino Unido\n(A precios de 1985)",xlab = "Fecha", ylab="", lwd=0.8) + theme_light()

# 2. Gráfico de la acf y la pacf 
lags = 24
x11()
ggAcf(lc,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF del PIB real de USA") + theme_light()
ggPacf(lc,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF del PIB real de USA")+ theme_light()

# 3. Test de Augmented Dickey Fuller seobre la serie a estududiar

adf.trend = ur.df(lc, type = "trend", selectlags = "AIC"); summary(adf.trend)

# 4. 

#---- Ejemplo práctico de uso de pruebas de ADF 2: Ejemplo de Enders  ---- 



