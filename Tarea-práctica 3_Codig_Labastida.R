library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)

# Datos
metrobus <- read_csv("https://raw.githubusercontent.com/CarlAo27/Uni/d9c62ffa26980030f139585cbd0f1cffc9f549ea/afluenciamb_desglosado_01_2026.csv",
                     show_col_types = FALSE)

datos <- metrobus %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(linea == "Línea 1",
         fecha >= "2023-01-01",
         fecha <= "2024-12-31") %>%
  group_by(fecha) %>%
  summarise(afluencia = sum(afluencia), .groups = "drop")

serie <- ts(datos$afluencia, frequency = 7)

# Gráfica
autoplot(serie)

# ACF y PACF
acf(serie)
pacf(serie)

# Diferenciación
serie_diff <- diff(serie)
acf(serie_diff)
pacf(serie_diff)

# Modelo automático
modelo <- auto.arima(serie)
modelo

# Diagnóstico
checkresiduals(modelo)

# Pronóstico
modelo %>%
  forecast(h = 30) %>%
  autoplot()

