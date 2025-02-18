# ______________________________________________________________________________
#
# Proyecto:       Laboratorio - Econometría II
#                 
# Script:         laboratorio1.R
# Objetivo:       
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          22-Enero-2025
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

#setwd("")

rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, tseries, lmtest, stargazer, pacman)


# CÓDIGO _______________________________________________________________________

# cmd+shift+C <- atajo para comentario

# Librerías:
## install.packages("nombre_paquete")
# install.packages("lmtest")
# install.packages("stargazer")
# install.packages("pacman")
# Llamarlas:
## library(nombre_paquete)
# library(lmtest)
# library(stargazer)
# library(pacman)

# pacman: Gestión de paquetes
pacman::p_load(lmtest, stargazer, pacman)

# Eliminar todos los objetos:
# rm(list=ls())
ls()

# - Activar rainbow parenthesis: Tools - Code - Rainbow Parenthesis

# Pendiente: Template para R-Studio

# Data -------------------------------------------------------------------------

# Simular una población normal:
?rnorm()
# rnorm(n=numero_de_realizaciones, mean = 0, sd = 1)
rnorm(10, 5, 1)

# Simular una poblción uniforme:
?runif()
# runif(n=numero_de_realizaciones, min = 0, max = 1)
runif(10, 0, 1)


# Distribución Normal:
# - Kurtosis: 3
# - Skewness: 0
# Jarque-Bera Test
pob_norm <- rnorm(100, 0, 1)
tseries::jarque.bera.test(pob_norm)

# H_0: Normalidad en la Población
# p-values >= 90 -> Normalidad en la población

# Simular la Población: 
# y = b_0 + b_1 x_1 + b_2 x_2 + e

df <- data.frame(
  # Sintaxis: Nombre de la Columna = Valor de la Columna
  # n=100,000
  x_1=rnorm(1000000, 100, 5),
  x_2=runif(1000000, 10, 20),
  e=rnorm(1000000, 0, 5)
)

df <- df %>% 
  mutate(
    y = 10 + 0.7 * x_1 + 23 * x_2 + e
  )

# Script -----------------------------------------------------------------------

# Correr regresión lineal
# lm( data=df, dep ~ indep + indep + ... )
?lm()
modelo1 <- lm(data=df, y ~ x_1 + x_2)
summary(modelo1)
typeof(modelo1)

# Acceder a componentes de una regresión
# - Coeficientes
modelo1$coefficients
# - Errores
modelo1$residuals


# ForLoop para coeficientes
coeficientes <- numeric(500000)
indices <- 1:1000000

for (i in 1:500000) {
  muestra <- sample(indices, 15000)
  modelo_loop <- lm(
    # Acceder a columna/fila de un Df: df[fila,columna]
    data=df[muestra,],
    y ~ x_1 + x_2
  )
  coeficientes[i] <- modelo_loop$coefficients[2]
}


# Graficar
plot(density(coeficientes))
abline(v=0.7, lwd=2, col="red")

tseries::jarque.bera.test(coeficientes)
