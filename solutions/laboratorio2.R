# ______________________________________________________________________________
#
# Proyecto:       Laboratorio - Econometría II
#                 
# Script:         laboratorio2.R
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

# rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, tseries, lmtest, stargazer, pacman)


# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

# Reproducir las bases de datos que generamos el laboratorio anterior
df <- data.frame(
  # Sintaxis: Nombre de la Columna = Valor de la Columna
  # n=100,000
  x_1=rnorm(1000000, 100, 5),
  x_2=runif(1000000, 10, 20),
  e=rnorm(1000000, 0, 5)
)

df <- data.frame(
  # Sintaxis: Nombre de la Columna = Valor de la Columna
  # n=100,000
  rnorm(1000000, 100, 5),
  runif(1000000, 10, 20),
  rnorm(1000000, 0, 5)
)

names(df) <- c("x_1", "x_2", "e")


## Acceder a una columna
df[["x_1"]] # 1 columna por nombre
df$x_1
# Varias columnas
df %>% select(x_1, x_2)
vars <- c("x_1", "x_2")
df[vars]
df %>% select(all_of(vars))


# Elemento particular
# Recuerden que los DF son matrices
df[2,3]
# Sintaxis: df[fila, columna]
df[,2] # <- Una columna (2da)
df[2,] # <- Una fila


# Lista
uuuuu <- list(TRUE, c(1,2,3), c("asd", "alskf", "asd"), as.Date("2019-01-01"))
uuuuu[1]
names(uuuuu) <- c("logico", "numerico", "texto", "fecha")
names(uuuuu)
uuuuu["logico"]

uuuuu[["nuevo_elemento"]] <- df


# Data Frame
typeof(df)
class(df)


# Vectores
typeof(character(1))
letras <- character(1)
numeros <- numeric(1)
length(letras)

numeros <- seq(1,100,1)
numeros
as.character(numeros)


# Clase DATE
?as.Date()
# Sintaxis:
# clase Date: año - mes - fecha
as.Date("2025-01-29")
Sys.Date()

difftime(as.Date("2025-01-01"), as.Date("2025-01-29"))
seq(as.Date("2025-01-01"), as.Date("2025-01-29"), by="day")

hoy <- Sys.Date()
class(hoy)
typeof(hoy)

unclass(hoy)

year(hoy)
month(hoy)
day(hoy)

as.Date("01-2025-29", format="%m-%Y-%d")

# %Y <- año en 4 digitos  
# %y <- año en 2 digitos
# %m <- mes en 2 digitos
# %B <- mes en nombre
# %b <- mes en abrebiatura
# %d <- dia en 2 digitos

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

format(hoy, "%d-%B")
format(hoy, "%d/%B")
fecha_prueba <- format(hoy, "%d de %b de %y")


# Volver a lo que aplica
df <- df %>% 
  mutate(
    y = 10 + 0.7 * x_1 + 23 * x_2 + e
  )

# Creamos un data frame que contiene una serie de tiempo
# df_ts <- data.frame(
#   # Longitud de 10 años
#   # Fecha
  fechas <- seq(as.Date("2010-01-01"), as.Date("2019-12-31"), by="day")
  # Variable generada con un proceso
  e_t <- rnorm(length(fechas),0,10)
# )

## Serie ----

  # Intercepto
  a=1000  

x_t = numeric(length(e_t))
x_t[1] <- a
for (i in 2:length(x_t)) {
  x_t[i] <- 0.8*x_t[i-1] + e_t[i]
}


c(0, diff(log(x_t)))


# Unir al df -----
df_ts <- bind_cols(df_ts, x_t=x_t) %>% 
  mutate(
    y_t=a+x_t+e_t
  )


# Script -----------------------------------------------------------------------


