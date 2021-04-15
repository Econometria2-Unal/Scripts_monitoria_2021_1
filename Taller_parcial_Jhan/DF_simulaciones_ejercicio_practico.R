##____________________________________________________________________________________
##____________________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECONÓMICAS
#                         ECONOMETRIA II - 2021-I
#      SESIÓN MONITORIA: Simulación y uso de preubas de raís unitaria 
#                        En particular de la Augmented Dickey Fuller (ADF)
##____________________________________________________________________________________
##_____________________________________________________________________________________

# Limpiar el Environment
rm(list = ls()) 

# Importación de paquetes
library(tseries)
library(stargazer)
library(dynlm)    # Paquete que me permite realizar MCOD
library(urca)     # Paquete que me permite realizar pruebas de raiz unitaria 

# La primera parte del script consistira en realizar una serie de simulaciones de Monte Carlo de tal forma que se 
# pueda simular la distribución empírica del estadístico de Dickey-Fuller para diferentes especificaciones de la
# prueba, a saber: preuba con tendencia e intercepto, prueba con intercepto pero sin tendencia y prueba sin tendencia ni intercepto

# Preparación de las simulaciones 
set.seed(1)    # Se inicia una semilla para 
n = 100        # n denota el número de observaciones que tendrá cada serie simulada (es decir, cada serie simulada tendrá 100 datos)
N = 10000      # N denota el número total de series que se van a simular, en total se van a simular 10.000 series

# Otros parámetros importantes: 
trend = 1:n
drift = 0.5

#---- Funciones para la simulación ----
function simulacion_DF


#---- Simulación DF sin drift ni trend ---- 



#---- Simulación DF con drift pero sin trend ---- 



#---- Simulación DF sin drift ni trend ---- 



#---- Comparación DF  ---- 



