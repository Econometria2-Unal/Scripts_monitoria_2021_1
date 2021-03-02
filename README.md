# Repositorio Monitoria Econometría II

> En el presente repositorio encontraran todos los scripts asociados al curso de econometría 2 para el Semestre 2020-2

El equipo se encuentra conformado por:
- [![profesora titular](https://img.shields.io/badge/profesor%20titular-Nancy%20Milena%20Hoyos-orange)](mailto:nmhoyosg@unal.edu.co)
- [![profesor asistente](https://img.shields.io/badge/Profesor%20asistente-Luis%20Alfonso%20Luna%20-blue)](mailto:nmhoyosg@unal.edu.co)
- [![monitor](https://img.shields.io/badge/monitor-Jhan%20Jhailer%20Andrade-yellow)](mailto:jandradep@unal.edu.co)
- [![monitor](https://img.shields.io/badge/monitor-Juan%20Camilo%20Forero-red)](mailto:jcforerob@unal.edu.co)
- [![monitor](https://img.shields.io/badge/monitor-Germ%C3%A1n%20C%20Rodr%C3%ADguez-green)](mailto:gecrodriguezpe@unal.edu.co)

## Objetivo general

El curso busca profundizar en el aspecto conceptual, teórico y práctico de algunas técnicas econométricas actuales empleadas en el análisis económico.

## Evaluación

 Actividad de Evaluación | Porcentaje Objetivo 
-------------------------|---------------------
 Quizzes                 |         70%         
 Trabajos                |         30%         


## Estructura del Repositorio

> Cada sesión de monitoria tendrá tres subcarpetas. El subdirectorio *1_documentos_pdfs* tiene todos los documentos importantes de la sesión de monitoria, el subdirectorio *2_datos* tiene todas las bases de datos usadas en monitoria y finalmente el subdirectorio *3_códigos* tiene el script utilizado para la monitoria. Queda como responsabilidad del estudiante descargar o en su defecto copiar este repositorio y luego organizar en su computador de la manera que considere más adecuada. 

### Sesión 1 de Monitoria - Programación básica en R

> El proposito es introducir los elementos básicos de programación en R y manejo de Rstudio

#### Scripts:
* Sesión 1 - Introducción a R.R

#### Documentos:
* rmarkdown-spanish (1).pdf
* rstudio-ide.pdf

### Sesión 2 de Monitoria - Cargar datos, MCO y Supuestos

> Consiste en introducir la manera de cargar datos en R así como en la forma de realizar regresiones en R. Se analizarán algunos supuestos básicos del modelo de regresión lineal

#### Scripts:
* Script 2 - Importación de datos y MCO.R

#### Bases de datos:
* Datos1 - Script 2.xlsx
* Datos2 - Script 2.csv
* Elecciones - Script 2.xls
* titanic.csv

#### Documentos:
* monitoria2_estadisticaR_supuestos.Rmd
* monitoria2_estadisticaR_supuestos.ipynb
* monitoria2_estadisticaR_supuestos.pdf

### Sesión 3 de Monitoria - Datos Panel 1

> Consiste en una introducción al manejo de datos panel en R. Para ello se empleará de manera extensiva el paquete **plm**. Se muestra como transformar una base de datos a un objeto *pdata.frame*. Se muestra los principales métodos de estimación para datos panel: **MCOC**, **efectos fijos**, **primeras diferencias** y **efectos aleatorias**

#### Scripts:
* Sesión 3 - Panel importación de datos.R

#### Bases de datos:

#### Documentos:
* monitoria3_conceptos_basicos_panel.Rmd
* monitoria3_conceptos_basicos_panel.ipynb
* monitoria3_conceptos_basicos_panel.pdf

### Sesión 4 de Monitoria - Datos Panel 2

> Consiste en un manejo completo de datos panel en R por medio del paquete **plm**. Primero, se muestra, nuevamente, los principales métodos de estimación para datos panel: **MCOC**, **efectos fijos**, **primeras diferencias** y **efectos aleatorias**. Luego, se ilustra una manera práctica de seleccionar el mejor modelo estimado por los anteriores métodos utilizando los test estadísticos de: **test de Hausman**, **test de primeras diferencias de Wooldridge** y **Prueba de Multiplicadores de Lagrange de Breusch-Pagan**. Posteriormente, se muetra como realizar la validación de supuestos sobre el modelo escogido utilizando las pruebas de: **Breush-Pagan**, **Breusch-Godfrey**, **Breusch-Pagan LM test for cross-sectional dependence in panels** y la corrección usando errores robustos cálculados pr el método de **arellano**. Finalmente, se ilustra como presentar los resultados de las estimaciones realizadas utilizando el paquete **stargazer**. 

#### Scripts:
* Sesión 4 - Panel con pruebas.R

#### Bases de datos:

#### Documentos:
* monitoria4_panel_completo.Rmd
* monitoria4_panel_completo.ipynb
* monitoria4_panel_completo.pdf

### Sesión 5 de Monitoria - Variables Instrumentales

> Consiste en una revisión del método de estimación por **variables instrumentales** en R por medio de los paquetes **AER** y **estimatr**. El script, contiene un total de 4 ejemplos en donde cada ejemplo expone de manera detallada y concisa el flujo de trabajo para estimar modelos por medio de variables instrumetnales usando R. A lo largo del script, se mira como realizar la estimación por medio de variables instrumentales utilzando tres enfoques distintos: se realiza **MC2E** de manera manual, es decir, realiando las dos estimaciones de manera explicita mediante el comando **lm**, se utiliza el comando **iv_reg** para realizar **MC2E** de manera automática usando un solo comando y se utiliza el comando **iv_robust** para realizar **MC2E** de manera automática pero corregida por errores robustos, siendo éste último comando la forma más recomendable de realizar *MC2E* en R. Finalmente, se exponen los *test de diagnósticos*: **Hausman**, **Instrumentos débiles** y **sobreidentificación de Sargan** para verificar algunos supuestos sobre los regresores y sobre los instrumentos. 

#### Scripts:
* Sesión 5 - Variables Instrumentales.R

#### Bases de datos:

#### Documentos:
* Sesión 5 - Variables Instrumentales.Rmd
* Sesión 5 - Variables Instrumentales.ipynb
* Sesión 5 - Variables Instrumentales.pdf
* Variable Instrumental.pptx
