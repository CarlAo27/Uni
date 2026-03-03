library(readr)
library(dplyr)
library(ggplot2)
library(stats)
library(TTR)

metrobus <- read_csv("C:/Users/EQUIPO/Documents/uniiii/teo/afluenciamb_desglosado_01_2026.csv")

# Filtrar datos: Periodo pospandemia y líneas de interés
datos_analisis <- metrobus %>%
  mutate(fecha = as.Date(fecha)) %>%
  filter(linea %in% c("Línea 1", "Línea 7"),
         fecha >= "2023-01-01" & fecha <= "2024-12-31") %>%
  group_by(fecha, linea) %>%
  summarise(afluencia_diaria = sum(afluencia, na.rm = TRUE)) %>%
  ungroup()

# Crear objeto de serie de tiempo para Línea 1 
serie_l1 <- ts(datos_analisis %>% filter(linea == "Línea 1") %>% pull(afluencia_diaria), frequency = 7)


#GRÁFICO DE LOS DATOS

ggplot(datos_analisis, aes(x = fecha, y = afluencia_diaria, color = linea)) +
  geom_line() +
  labs(title = "Cap. 1: Afluencia Diaria Metrobús (Datos Originales)", 
       subtitle = "Sucesión de observaciones 2023-2024",
       x = "Fecha", y = "Pasajeros") +
  theme_minimal()

## Gráfica Linea 1
ggplot(linea_1, aes(x = fecha, y = afluencia_diaria)) +
  geom_line(color = "red", alpha = 0.8) +
  labs(
    title = "Afluencia diaria – Metrobús Línea 1",
    subtitle = "Periodo 2023–2024",
    x = "Fecha",
    y = "Número de pasajeros"
  ) +
  theme_minimal()
# DESCOMPOSICIÓN TEMPORAL 
# Separación de tendencia, estacionalidad y componente irregular [1, 2]
descomposicion <- decompose(serie_l1, type = "additive")
plot(descomposicion, col = "blue")
title("Descomposición de la afluencia diaria - Línea 1 (2023–2024)")

#PROCESOS ESTOCÁSTICOS ---
# Evaluación de Estacionariedad y Ruido Blanco [1, 3]


test_ruido <- Box.test(serie_l1, lag = 7, type = "Ljung-Box")
print(test_ruido)

# 3.3: Visualización para identificar posible Caminata Aleatoria o No Estacionariedad
# Si la media varía con el tiempo, el proceso no es estacionario.
plot.ts(serie_l1, main = "Cap. 3: Evaluación de Estacionariedad (Proceso Estocástico)")


# FUNCIONES DE AUTOCOVARIANZA Y AUTOCORRELACIÓN ---
# Análisis de dependencia temporal y correlogramas 
par(mfrow = c(1, 2))
# ACF: Muestra la estacionalidad (picos en lags múltiplos de 7)
acf(serie_l1, main = "Correlograma (ACF)", lag.max = 35)
# PACF: Ayuda a identificar el orden del proceso autoregresivo
pacf(serie_l1, main = "Autocorrelación Parcial (PACF)", lag.max = 35)
par(mfrow = c(1, 1))


#  TRANSFORMACIONES 
# Estabilización de la serie para lograr estacionariedad [1, 5]
# 5.1: Suavizamiento por Medias Móviles (para identificar tendencia limpia)
serie_suavizada <- SMA(serie_l1, n = 7)
plot.ts(serie_suavizada, col="red", main="Cap. 5: Suavizamiento (Media Móvil 7 días)")

# 5.3: Diferencias de Box-Jenkins
# Aplicamos una diferencia para eliminar la tendencia y estabilizar la media.
serie_estacionaria <- diff(serie_l1, differences = 1)
plot.ts(serie_estacionaria, col="darkgreen", main="Cap. 5: Serie Transformada (Diferenciación Box-Jenkins)")
abline(h = 0, col = "black", lty = 2)

