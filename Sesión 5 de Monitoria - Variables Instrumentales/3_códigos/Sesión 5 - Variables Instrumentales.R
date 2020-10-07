##______________________________________________________________________________
##______________________________________________________________________________
#       UNIVERSIDAD NACIONAL DE COLOMBIA - FACULTAD DE CIENCIAS ECON?MICAS
#                         ECONOMETRIA II - 2020-II
#               SESIÓN MONITORIA : Variables Instrumentales
##______________________________________________________________________________
##______________________________________________________________________________


##1. Ejemplo #1  univariado ----------------------------------------------------


#Para limpiar el environment. 

rm(list = ls()) 

# Instalamos los paquetes.

# install.packages("AER") #Applied Econometrics with R for Instrumental Variables
# install.packages("foreing") #Para cargar datos con formato Stata
# install.packages("stargazer") #Para una presentaci?n m?s est?tica de los resultados
# install.packages("estimatr") #Para hacer MC2E con errores robustos
# install.packages("arm") #An?lisis de datos utilizando regresiones
# installl.packages("lmtest")

#Cargamos los paquetes. 

library(AER);library(foreign); library(stargazer); 
library(arm);library(lmtest);library(estimatr)

#Cargamos la base de datos. 

data=read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")

#Visualizamos la base de datos

View(data)

#La estructura de datos es de corte transversal y corresponde una data.frame de 753 para 22 variables, 
#las cuales son: inlf: =1 if in lab frce, 1975; hours: hours worked, 1975; kidslt6: # kids < 6 years
#kidsge6: # kids 6-18;age: woman's age in yrs;educ: years of schooling;wage: est. wage from earn, hrs;
#repwage: rep. wage at interview in 1976;hushrs; hours worked by husband, 1975;husage: husband's age
#huseduc: husband's years of schooling;huswage; husband's hourly wage, 1975;faminc: family income, 1975
#mtr: fed. marg. tax rte facing woman; motheduc; mother's years of schooling;fatheduc: father's years of schooling
#unem: unem. rate in county of resid;city: =1 if live in SMSA;exper: actual labor mkt exper
#nwifeinc: (faminc - wage*hours)/1000;lwage: log(wage);expersq: exper^2

#Ahora vamos a eliminar las observaciones que no tienen salario: !is.na(wage)

data.VI <- subset(data,!is.na(wage))
attach(data.VI)

#Regresi?n utilizando MCO: aqu? se espera que haya un problema de endegeneidad.Recuerden que la exogeneidad 
#es un supuesto, por ende, es la teor?a y el an?lisis del que problema el que nos indica si se puede estar violando

R.MCO = lm(log(wage)~ educ, data=data.VI);summary(R.MCO)   

#Ahora estimaremos el coeficiente Beta de inter?s utilizando como instrumento 
#la educaci?n del padre "fatheduc"

Beta = with(data.VI,cov(log(wage),fatheduc)/cov(educ,fatheduc)); Beta     


#Asimismo, realizaremos el procedimeinto mediante MC2E.
#Regresi?n en 2 etapas ---------------------------------------------------------
#1.1.Regresi?n auxiliar (primera etapa)-----------------------------------------

##Recuerden que aqu? deben incluirse como controles todas las variables 
##ex?genas del modelo. Tambi?n debe evaluarse la relevancia del instrumento. 

Reg.aux=lm(educ ~ fatheduc, data = data.VI); summary(Reg.aux)

#Calculamos los valores ajustados de la regresi?n, que har?n de variable
#instrumentada en la segunda etapa.

educ.fitted = fitted(Reg.aux)

#1.2. Regresi?n de la segunda etapa---------------------------------------------
 
Reg.VI = lm(log(wage)~educ.fitted, data=data.VI) 
summary(Reg.VI)


#1.3.ivreg del paquete AER------------------------------------------------------

R.VI = ivreg(log(wage) ~ educ| fatheduc , data=data.VI); summary(R.VI)

# Ahora haremos la regresi?n utilizando iv_robust del paquete estimatr, 
##el cual incorpora errores est?ndar robustos.

R.VI.Robust = iv_robust(log(wage) ~ educ| fatheduc , data=data.VI);summary(R.VI.Robust)

#Ahora presentaremos los resultados. 
stargazer(R.MCO,Reg.VI,R.VI, column.labels=c("MCO","MC2E", "IVREG"), type="text")


#2. Ejemplo #2  variables control y dos instrumentos ---------------------------


#Estimamos la regresi?n original por MCO---------------------------------------#

Reg.MCO = lm(log(wage)~educ+exper+I(exper^2)+city, data=data.VI)
summary(Reg.MCO)

#Vamos a analizar la relevancia de los instrumentos. 

cor(educ,motheduc)
cor(educ,fatheduc) 

summary(lm(educ ~ exper + I(exper^2) + city + motheduc + fatheduc, data = data.VI))

#2.1 Haremos la regresi?n auxiliar (primera etapa)------------------------------

stage1 <- lm(educ~exper+I(exper^2)+city+motheduc+fatheduc, data=data.VI)
summary(stage1)

# 2.2 Regresi?n de la segunda etapa --------------------------------------------

stage2<-lm(log(wage)~fitted(stage1)+exper+I(exper^2)+city, data=data.VI)
summary(stage2)

# 2.3 ivreg del paquete AER ----------------------------------------------------

aut.MC2E<-ivreg(log(wage)~educ+exper+I(exper^2)+city|motheduc+fatheduc+
                  exper+ I(exper^2) + city , data=data.VI)
summary(aut.MC2E)

#----------------------------------------------------------------------------#
#--- La forma es ivreg(variable end?gena ~ variable end?gena + ex?genas|  ---#
#----                  Instrumentos + Ex?genas , data="...")              ---#
#---- OJO: es importante el uso de la raya vertical "|" (No es una L ni   ---#
#---- una I) para indicar cuales son las v. ex?genas, las v. end?genas y  ---#
#---- los instrumentos                                                    ---#
#----------------------------------------------------------------------------#

# La forma directa con ivrobust es: 
MC2E.Robusto <- iv_robust(log(wage)~educ+exper+I(exper^2)+city|motheduc+
                            fatheduc+exper+I(exper^2) + city , data=data.VI)
summary(MC2E.Robusto)

#----------------------------------------------------------------------------#
#- La forma es iv_robust(variable end?gena ~ variable end?gena + ex?genas|  -#
#----                   Instrumentos + Ex?genas, data="...")              ---#
#---- OJO: es importante el uso de la raya vertical "|" (No es una L ni   ---#
#---- una I) para indicar cuales son las v. ex?genas, las v. end?genas y  ---#
#---- los instrumentos                                                    ---#
#----------------------------------------------------------------------------#

# Presentaci?n de resultados

stargazer(Reg.MCO, stage1,stage2,aut.MC2E,type="text",
          column.labels = c("MCO", "IV Etapa 1","IV Etapa 2","IVREG"))


#3. TEST DE ENDOGENEIDAD DE HAUSMAN---------------------------------------------

## ?Es necesario el uso de variables instrumentales? 
## La respuesta a esta pregunta es fundamentalmente te?rica, pero un complemento
## resulta de hacer la regresi?n original aumentada por los residuales de la primera 
## etapa, pues si dichos residuales explican a la variable dependiente, puede deberse
## a un problema de endogeneidad en la regresi?n originalmente planteada. As? pues, se
## recomendar?a utilizar VI.

#Ho= NO ENDOGENEIDAD/EXOGENEIDAD  

#Se calculan los residuos de la regresi?n de la primera etapa. 

res.stage1<-resid(stage1)
 
#####Estimar regresi?n aumentada:
#(var.dependiente contra ex?genas originales, end?gena y residuos de la primera etapa)

Reg.aum <- lm(log(wage)~educ+exper+I(exper^2)+city+res.stage1, data=data.VI)
summary(Reg.aum)

#se rechaza la hip?tesis nula de exogeneidad si el coeficiente que acompa?a 
#los residuos estimados en la primera etapa es significativo.


#4. tests de diagn?stico con base en el modelo estimado con errores robustos.

MC2E.Robusto <- iv_robust(log(wage)~educ+exper+I(exper^2)+city|motheduc+
                            fatheduc+exper+I(exper^2) + city , data=data.VI, diagnostics=TRUE)
summary(MC2E.Robusto, diagnostics = TRUE)

##Weak instrument: Ho = los instrumentos son debiles 
##Hausman: Ho = No hay endogeneidad (si se rechaza es necesario usar los instrumentos) 
##Overidentifying/Sargan: Ho= instrumentos ex?genos / H1= al menos uno es end?geno (sistema sobreidentificado)

#OJO --------------------------------------------------------------------------#
# Recuerden que los supuestos b?sicos en variables instrumentales es la relevancia 
# y la exogeneidad del instrumento. NO obstante, recuerden que desde el punto de 
# vista estad?stico no se puede comprobar la exogeneidad (es un supuesto y los 
# supuestos no siempre se pueden probar). Una correlaci?n diferente de 
# cero o cercana a cero entre la variable instrumental y los errores de la regresi?n 
# no son una prueba concluyente sobre la exogeneidad. Por lo general, el supuesto 
# de exogeneidad se valida desde la teor?a. 
#------------------------------------------------------------------------------#

## 4. Ejemplo 3 ----------------------------------------------------------------

### Ahora vamos a estudiar los efectos de la fertilidad sobre la oferta de trabajo,
### tomando informaci?n del censo de los EE.UU para 1980 sobre mujeres de entre 21 
### y 35 a?os con dos o m?s hijos. Esta base de datos contine 255.000 observaciones
### sobre 8 variables, las cuales son:

#morekids: factor. Does the mother have more than 2 children?
#gender1: factor indicating gender of first child.
#gender2:factor indicating gender of second child.
#age:age of mother at census.
#afam:factor. Is the mother African-American?
#hispanic:factor. Is the mother Hispanic?
#other:factor. Is the mother's ethnicity neither African-American nor Hispanic, nor Caucasian? (see below)
#work:number of weeks in which the mother worked in 1979.


##Cargamos la base de datos. 

data(Fertility)
attach(Fertility)
Fertility$morekids = ifelse(morekids=="yes",1, 0)

#Visualizamos la base de datos.

View(Fertility)

#4.1 Regresi?n por MCO --------------------------------

#(la categor?a caucasica va a ser de referencia)
Reg.F = lm(work~morekids + age + afam + hispanic + other, data = Fertility)
summary(Reg.F) #?Por qu? la variable morekids puede ser end?gena?


#Creamos la variable same sex que es igual a 1 si los dos primeros hijos de la mujer
#son del mismo sexo, y cero en el caso que no sea as?.

Fertility$gender1 = ifelse(gender1=="male",1,0) #Dummy =1 si el primer hijo es hombre
Fertility$gender2 = ifelse(gender2=="male",1,0) #Dummy =1 si el segundo hijo es hombre

same = Fertility$gender1 + Fertility$gender2
samesex=ifelse(same!=1,1,0)
 

##4.2 MC2E --------------------------------------------------------------------

# vamos a utilizar samesex como instrumento de morekids

## 4.2.1 Primera etapa --------------------------------------------------------

##Aqu? deben incluirse como controles todas las variables ex?genas del modelo
##Tambi?n debe evaluarse la relevancia del instrumento. 

Reg.aux1=lm(morekids ~ samesex + age + afam + hispanic + other, data = Fertility )
summary(Reg.aux1)

#Calculamos los valores ajustados de la regresi?n, que har?n de instrumento en la segunda etapa.
morekids.fitted = fitted(Reg.aux1)

## 4.2.2 Segunda etapa --------------------------------------------------------

Reg.VI_1 = lm(work ~ morekids.fitted+ age + afam + hispanic + other, 
              data = Fertility ) 
summary(Reg.VI_1)

# La forma directa con ivreg es:

MC2E.F<-ivreg(work ~ morekids+ age + afam + hispanic + other|age + afam + 
                hispanic + other+ samesex, data = Fertility)  
summary(MC2E.F)

#----------------------------------------------------------------------------#
#--- La forma es ivreg(variable end?gena ~ variable end?gena + ex?genas|  ---#
#----                   Exogenas + Instrumentos, data="...")              ---#
#---- OJO: es importante el uso de la raya vertical "|" (No es una L ni   ---#
#---- una I) para indicar cuales son las v. ex?genas, las v. end?genas y  ---#
#---- los instrumentos                                                    ---#
#----------------------------------------------------------------------------#

# 4.2.3 La forma directa con ivrobust ------------------------------------------
MC2E.F.Robusto <-  iv_robust(work ~ morekids+ age + afam + hispanic + other|age + 
                               afam + hispanic + other+ samesex, data = Fertility)  
summary(MC2E.F.Robusto)

#----------------------------------------------------------------------------#
#- La forma es iv_robust(variable end?gena ~ variable end?gena + ex?genas|  -#
#----                   Exogenas + Instrumentos, data="...")              ---#
#---- OJO: es importante el uso de la raya vertical "|" (No es una L ni   ---#
#---- una I) para indicar cuales son las v. ex?genas, las v. end?genas y  ---#
#---- los instrumentos                                                    ---#
#----------------------------------------------------------------------------#

# Presentaci?n de resultados

stargazer(Reg.F,Reg.aux1,Reg.VI_1,MC2E.F,type="text",
          column.labels = c("MCO","Etapa 1","Etapa 2","IVREG"))


# 4.3 TEST DE ENDOGENEIDAD DE HAUSMAN-------------------------------------------

## ?Es necesario el uso de variables instrumentales? (OJO, es un test proxy. La justificaci?n es te?rica)) 
##Ho= NO ENDOGENEIDAD 

#Se calculan los residuos de la regres?n de la primera etapa. 

resid.stage1<-resid(Reg.aux1)

#####Estimar regresi?n aumentada:
#(var.dependiente contra ex?genas originales, end?gena y residuos de la primera etapa)

Reg.aumentada <- lm(work~morekids + age + afam + hispanic + other+resid.stage1,
                    data = Fertility)
summary(Reg.aumentada)

#se rechaza la hip?tesis nula de exogeneidad si el coeficiente que acompa?a 
#los residuos estimados en la primera etapa es significativo.


# Tests de diagn?stico con base en el modelo estimado con errores robustos.

MC2E.F.Robusto <-  iv_robust(work ~ morekids+ age + afam + hispanic + other|age + 
                               afam + hispanic + other+ samesex, 
                             data = Fertility, diagnostics = TRUE)  
summary(MC2E.F.Robusto)

##Weak instrument: Ho= los instrumentos son debiles 
##Hausman: Ho = No hay endogeneidad (si se rechaza hay que usar los instrumentos)
##Overidentifying/Sargan: Ho= instrumentos ex?genos - H1= al menos uno es end?geno (sistema sobreidentificado)

# Ejemplo 5 --------------------------------------------------------------------

#Importar datos sobre el consumo de cigarrillos en EEUU para 1985 y 1995

data("CigarettesSW")

#state: Factor que indica Estado
#year: Factor que indica a?o
#cpi: ?ndice de precios al consumidor
#population: Poblaci?n del estado
#packs: N?mero de cajas per capita
#income: Ingreso estatal total nominal
#tax: Impuestos promedio locales
#price: Precio medio durante el a?o fiscal, incluyendo impuesto sobre las ventas
#taxs: Impuestos promedio locales, incluyendo impuesto sobre las ventas de cigarrillos


#Visualizamos la base de datos

View(CigarettesSW)
attach(CigarettesSW)

#Transformaci?n-Creaci?n de variables

CigarettesSW$rprice <- price/cpi #precios deflactados
CigarettesSW$rincome <- income/population/cpi #ingreso per c?pita deflactado
CigarettesSW$tdiff <- (taxs - tax)/cpi #impuesto sobre las ventas deflactado


##Estimar Modelo (a?o 1985)
#Variable dependiente: logaritmo de packs (cajas de cigarros)
#Variables ex?genas: logaritmo de rincome (ingreso per c?pita deflactado)
#Variable end?gena: logaritmo de rprice (precios deflactados)
#Instrumentos: tdiff (impuesto sobre las ventas) y tax/cpi (impuestos locales deflactados)

#Modelo por ivreg
modelo <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + (tax/cpi), data = CigarettesSW, subset = year == "1995")
summary(modelo, diagnostics=TRUE)

#Modelo por iv_robust

modelo.R <- iv_robust(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + (tax/cpi), data = CigarettesSW, subset = year == "1995", diagnostics=TRUE)
summary(modelo.R, diagnostics=TRUE)
 
