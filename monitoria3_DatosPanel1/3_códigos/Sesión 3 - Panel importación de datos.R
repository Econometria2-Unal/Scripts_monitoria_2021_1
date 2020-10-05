##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2020-II
#                 SESIÓN #3 MONITORIA : Modelos para datos panel.
##______________________________________________________________________________
##______________________________________________________________________________

rm(list = ls()) #limpiar el enviroment 

# #Paquetes 
# install.packages("plm")         #Panel linear models 
# install.packages("gplots")      #Tools for plotting data 
# install.packages("stargazer")   #Tablas más estéticas para publicar
# install.packages("dplyr")       #Para manipular bases de datos
# install.packages("foreing")     #Importar datos (e.g. archivos dta de stata)
# install.packages("haven")       #haven se usa a veces para compatibilidad de algunas versiones de stata 
# install.packages("sandwich")    #Estimador de errores robustos 
# install.packages("lmtest")      #Inferencia estad?stica para modelos lineales 
# install.packages("tseries")     #Para manejar series de tiempo y para algunas pruebas de series de tiempo
# install.packages("wooldridge")  #Bases de datos que se encuentran en Wooldridge
# install.packages("AER")         #Applied Econometrics with R(funciones y bases de datos)
#AER contiene estimadores como tobit y variables instrumentales (IV)
#AER contiene diferentes bases de datos para ejercicios

library(plm);library(gplots);library(stargazer)
library(foreign);library(sandwich);library(lmtest);library(haven)
library(tseries);library(wooldridge);library(AER);library(dplyr)



#Ejemplo adaptado de Stock y Watson ----------

# US traffic fatalities panel data for the "lower 48" US states 
# (i.e., excluding Alaska and Hawaii), annually for 1982 through 1988
# state and year are the individual and time index respectively

#Base de datos 
data("Fatalities")  # Base de datos importada desde AER
help("Fatalities")  # Fatalities puede ser trabajada directamente con R con dicho nombre
# Para usar la función glimpse solo es necesario importar el paquete dplyr
glimpse(Fatalities) # Visualizar la base de datos (se nota la estructura antes de transformar usando pdata.frame)
class(Fatalities) # Se importa la base de datos como un data frame
dim(Fatalities) # Da las dimensiones del data frame (336 obs y 34 variables)

#indexamos la Base de datos  como panel: 
Panel = pdata.frame(Fatalities, index = c("state","year"))
# class(Panel)   # el objeto ahora es un tipo especial de data frame llamado pdata.frame
help("pdata.frame")


# El panel resultante está ordenado primero por el indice individual y luego por el indice temporal


#Estructura del codigo ---------------------------------------------------#
#Nombre <- pdata.frame(Base_datos, index=c("Var_indivuo","Var_tiempo"))
#-------------------------------------------------------------------------#


# Definimos la tasa de mortalidad 
Panel$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# Extraer una variable del panel (e.g. pop de Panel)
#Panel$pop    # La extracción de una varaible es igual que para un data frame normal 
# La estracción incluye los dos indices del panel para cada observación              

# Para extraer solo los indices de cada observacion del panel 
index(Panel)  # Retorna un df cuyas dos variables son los dos índices

#Para conocer las dimensiones del panel
pdim(Panel) # Acá es importante que se fijen si indexaron bien el Panel a RStudio.

#Para determinar que variables no cambian a lo largo del tiempo o entre los individuos
pvar(Panel)


#Pooled 
fatalities_pooled <- plm(fatal_rate ~ beertax + drinkage + unemp + log(income), 
                         index = c("state", "year"),
                         model = "pooling",
                         data = Panel)

#Efectos Fijos 
fatalities_FE <- plm(fatal_rate ~ beertax + drinkage + unemp + log(income), 
                     index = c("state", "year"),
                     model = "within",
                     data = Panel)

#Efectos Aleatorios
fatalities_EA <- plm(fatal_rate ~ beertax + drinkage + unemp + log(income), 
                     index = c("state", "year"),
                     model = "random",
                     data = Panel)

#Cálculo del thetha 
attach(Panel)
fitted.values(fatalities_EA)[1:7] #valores ajustados de Y para el primer individuo
fatal_rate[1:7]#valores observados de Y para el primer individuo
mean(fatal_rate[1:7]) #valor promedio de Y para el primer individuo
residuals(fatalities_EA)[1:7] #Residuales para el primer individuo
theta = (fatal_rate[1:7]-fitted.values(fatalities_EA)[1:7]-residuals(fatalities_EA)[1:7])/mean(fatal_rate[1:7]);theta

#Primeras Diferencias 
fatalities_FD <- plm(fatal_rate ~ beertax +drinkage + unemp + log(income), 
                     index = c("state", "year"),
                     model = "fd",
                     data = Panel)

stargazer(fatalities_pooled, fatalities_FE, fatalities_EA, fatalities_FD, digits = 3,
          header = F, type = "text", 
          title = "Modelos de catastrofes de tránsito por conducir ebrio", 
          column.labels = c("Pooled","EF","RE","Primeras Diferencias"))

