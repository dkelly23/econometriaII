# ______________________________________________________________________________
#
# Proyecto:       Laboratorio - Econometría II
#                 
# Script:         laboratorio3.R
# Objetivo:       
#
# Autor:          Daniel Kelly
# Email:          daniel.kelly@transformaciondigital.gob.mx
# 
# Fecha:          5-Febrero-2025
#
# ______________________________________________________________________________

# PREAMBULO ____________________________________________________________________

#setwd("")

rm( list=ls() )
#.rs.restartR()

# librerias
pacman::p_load(tidyverse, readxl, showtext, janitor, wooldridge, tseries)

# idioma
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANG = "es_MX.UTF-8")

# utilidades
font_add_google("Montserrat")
showtext_auto() 
options(scipen = 999)

# funciones
#source("/Users/danielkelly/Documents/Estadística/R/Utilities/mex_gob_gg/ggmx/R/graph_tsline.R")
#source("/Users/danielkelly/Documents/Estadística/R/Utilities/mex_gob_gg/ggmx/R/graph_tsbar.R")

# opciones
#create_project_folders()
#file.edit("~/.Rprofile")
#file.edit("/Users/danielkelly/.config/rstudio/templates/default.R")


# CÓDIGO _______________________________________________________________________

# Data -------------------------------------------------------------------------

data("barium")
data("ezanders")
data("fertil3")

load("/Users/danielkelly/Library/Mobile Documents/com~apple~CloudDocs/El Colegio de México/Lab Ectria 2/IPC.RData")

# Script -----------------------------------------------------------------------

## K1 --------------------------------------------------------------------------

# 1. Obtener cambios porcentuales
IPC |> view()

# Sintaxis: df |> mutate(var_nueva=expresion)
# length(expresion)=1, o length(expresion)=nrow(df)

length(diff(IPC$IPC))
nrow(IPC)

IPC$gIPC <- c(0, diff(log(IPC$IPC)))
mean(IPC$gIPC)

# 2. Graficarlos
hist(IPC$gIPC)
abline(v=mean(IPC$gIPC), col="red")

plot(density(IPC$gIPC))
abline(v=mean(IPC$gIPC), col="red")

tseries::jarque.bera.test(IPC$gIPC)


# 3. Modelos
IPC <- IPC |> 
  mutate(gIPC=abs(gIPC)) |> 
  mutate(t = 1:n(), dIPC=lag(gIPC))
IPC$dIPC[1] <- 0

variables <- list(
  c("t"),
  c("dIPC"),
  c("t", "dIPC")
)

for (i in seq_along(variables)) {
  assign(
    paste0("fml_", i), 
    paste("gIPC ~", paste(variables[[i]], collapse="+")) |> 
      as.formula()
    )
}

for (i in seq_along(variables)) {
  assign(
    paste0("modelo_",i),
    lm(formula = get(paste0("fml_", i)), data=IPC)
  )
}

lapply(1:3, function(x) {
  summary(get(paste0("modelo_", x)))
})
