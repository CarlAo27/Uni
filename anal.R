# ============================================================================
# ANÁLISIS ACTUARIAL - EJERCICIO 1: MODELADO DE PAGOS DE DEUDA
# ============================================================================
# Objetivo: Entender patrones de pago, riesgo de churn y comportamiento crediticio
# Contexto: Datos de clientes que realizan pagos sobre deuda del mismo tipo
# ============================================================================

# Cargar librerías necesarias
library(tidyverse)
library(moments)
library(gridExtra)
library(psych)

# 1. CARGA Y ESTRUCTURA BÁSICA DE DATOS
# ============================================================================
cat("\n==== PASO 1: CARGA Y ESTRUCTURA DE DATOS ====\n")

datos <- read.csv("Ejercicio_1.csv")

# Dimensiones del dataset
cat("\nDimensiones:", nrow(datos), "registros x", ncol(datos), "variables\n")

# Estructura de datos
str(datos)

# Primeras observaciones
head(datos, 10)

# ============================================================================
# 2. ANÁLISIS DESCRIPTIVO UNIVARIADO (Variable por variable)
# ============================================================================
cat("\n==== PASO 2: ESTADÍSTICAS DESCRIPTIVAS UNIVARIADAS ====\n")

# 2.1 Variable GENDER (Género)
cat("\n--- GÉNERO ---\n")
gender_dist <- table(datos$Gender)
gender_prop <- prop.table(gender_dist)
cat("Distribución:\n")
print(cbind(Frecuencia = gender_dist, Proporción = gender_prop))

# 2.2 Variable AGE (Edad)
cat("\n--- EDAD ---\n")
age_stats <- data.frame(
  Min = min(datos$Age, na.rm=T),
  Q1 = quantile(datos$Age, 0.25, na.rm=T),
  Mediana = median(datos$Age, na.rm=T),
  Media = mean(datos$Age, na.rm=T),
  Q3 = quantile(datos$Age, 0.75, na.rm=T),
  Max = max(datos$Age, na.rm=T),
  DesvEst = sd(datos$Age, na.rm=T),
  CV = sd(datos$Age, na.rm=T)/mean(datos$Age, na.rm=T) # Coef. Variación
)
print(age_stats)
cat("\nAsimetría (Skewness):", skewness(datos$Age, na.rm=T), "\n")
cat("Curtosis:", kurtosis(datos$Age, na.rm=T), "\n")

# 2.3 Variable PAYMENT.METHOD (Método de Pago)
cat("\n--- MÉTODO DE PAGO ---\n")
payment_method <- table(datos$Payment.Method)
payment_prop <- prop.table(payment_method)
cat("Distribución:\n")
print(cbind(Frecuencia = payment_method, Proporción = payment_prop))

# 2.4 Variable CHURN (Estado del Cliente)
cat("\n--- CHURN (RIESGO CREDITICIO) ---\n")
churn_dist <- table(datos$Churn, useNA="ifany")
churn_prop <- prop.table(churn_dist)
cat("Distribución:\n")
print(cbind(Frecuencia = churn_dist, Proporción = churn_prop))
cat("\nInterpretación Actuarial:\n")
cat("- 'loyal': Clientes que continuarán pagando\n")
cat("- 'churn': Clientes con riesgo de incumplimiento\n")
cat("- NA: Valores faltantes (requieren imputación)\n")

# 2.5 Variable PAYMENT (Montos de Pago)
cat("\n--- PAGOS REALIZADOS ---\n")
payment_stats <- data.frame(
  Min = min(datos$Payment, na.rm=T),
  Q1 = quantile(datos$Payment, 0.25, na.rm=T),
  Mediana = median(datos$Payment, na.rm=T),
  Media = mean(datos$Payment, na.rm=T),
  Q3 = quantile(datos$Payment, 0.75, na.rm=T),
  Max = max(datos$Payment, na.rm=T),
  DesvEst = sd(datos$Payment, na.rm=T),
  CV = sd(datos$Payment, na.rm=T)/mean(datos$Payment, na.rm=T)
)
print(payment_stats)
cat("\nAsimetría:", skewness(datos$Payment, na.rm=T), "\n")
cat("Curtosis:", kurtosis(datos$Payment, na.rm=T), "\n")

# ============================================================================
# 3. ANÁLISIS DE CALIDAD DE DATOS (DATA QUALITY)
# ============================================================================
cat("\n==== PASO 3: ANÁLISIS DE CALIDAD DE DATOS ====\n")

# Valores faltantes
missing_data <- data.frame(
  Variable = colnames(datos),
  ValoresFaltantes = colSums(is.na(datos)),
  Porcentaje = round(colSums(is.na(datos))/nrow(datos)*100, 2)
)
print(missing_data)

# Datos atípicos (outliers) en montos de pago
cat("\n--- DETECCIÓN DE OUTLIERS EN PAGOS ---\n")
Q1 <- quantile(datos$Payment, 0.25, na.rm=T)
Q3 <- quantile(datos$Payment, 0.75, na.rm=T)
IQR <- Q3 - Q1
limite_inferior <- Q1 - 1.5*IQR
limite_superior <- Q3 + 1.5*IQR

outliers <- sum(datos$Payment < limite_inferior | datos$Payment > limite_superior, na.rm=T)
cat("Número de outliers:", outliers, "(",
    round(outliers/nrow(datos)*100, 2), "%)\n")
cat("Límite inferior:", round(limite_inferior, 2), "\n")
cat("Límite superior:", round(limite_superior, 2), "\n")

# ============================================================================
# 4. ANÁLISIS BIVARIADO: RELACIONES ENTRE VARIABLES
# ============================================================================
cat("\n==== PASO 4: ANÁLISIS BIVARIADO ====\n")

# 4.1 Relación: CHURN vs PAGOS (Variable crítica)
cat("\n--- CHURN vs MONTOS DE PAGO ---\n")
churn_payment <- aggregate(datos$Payment, 
                           list(Churn = datos$Churn), 
                           function(x) c(Media = mean(x, na.rm=T),
                                        Mediana = median(x, na.rm=T),
                                        DesvEst = sd(x, na.rm=T),
                                        Min = min(x, na.rm=T),
                                        Max = max(x, na.rm=T)))
print(churn_payment)

# Test ANOVA para diferencias significativas
anova_result <- aov(Payment ~ Churn, data = datos)
cat("\nTest ANOVA (Churn vs Payment):\n")
print(summary(anova_result))

# 4.2 Relación: GENDER vs PAGOS (Segmentación por género)
cat("\n--- GÉNERO vs MONTOS DE PAGO ---\n")
gender_payment <- aggregate(datos$Payment,
                            list(Gender = datos$Gender),
                            function(x) c(Media = mean(x, na.rm=T),
                                         Mediana = median(x, na.rm=T),
                                         DesvEst = sd(x, na.rm=T),
                                         N = length(x)))
print(gender_payment)

# 4.3 Relación: PAYMENT.METHOD vs PAGOS
cat("\n--- MÉTODO DE PAGO vs MONTOS ---\n")
method_payment <- aggregate(datos$Payment,
                            list(Metodo = datos$Payment.Method),
                            function(x) c(Media = mean(x, na.rm=T),
                                         Mediana = median(x, na.rm=T),
                                         DesvEst = sd(x, na.rm=T),
                                         N = length(x)))
print(method_payment)

# 4.4 Relación: EDAD vs PAGOS (Análisis de Ciclo de Vida)
cat("\n--- EDAD vs MONTOS DE PAGO (Correlación) ---\n")
correlacion <- cor(datos$Age, datos$Payment, use = "complete.obs")
cat("Correlación de Pearson:", round(correlacion, 4), "\n")

# Regresión simple: Payment ~ Age
reg_age <- lm(Payment ~ Age, data = datos)
cat("\nModelo lineal: Payment ~ Age\n")
print(summary(reg_age))

# 4.5 Tabla de contingencia: CHURN vs PAYMENT.METHOD
cat("\n--- CHURN vs MÉTODO DE PAGO ---\n")
contingency <- table(datos$Churn, datos$Payment.Method, useNA="ifany")
print(contingency)

# Proporciones
cat("\nProporciones:\n")
print(prop.table(contingency, margin = 2)) # Por columna (método de pago)

# ============================================================================
# 5. SEGMENTACIÓN ACTUARIAL (RISK SEGMENTATION)
# ============================================================================
cat("\n==== PASO 5: SEGMENTACIÓN POR RIESGO ACTUARIAL ====\n")

# Crear segmentos por edad y churn
datos <- datos %>%
  mutate(
    Grupo_Edad = cut(Age, 
                     breaks = c(0, 30, 40, 50, 60, 100),
                     labels = c("17-30", "31-40", "41-50", "51-60", "61+")),
    Riesgo = case_when(
      is.na(Churn) ~ "Desconocido",
      Churn == "churn" ~ "Alto",
      Churn == "loyal" ~ "Bajo",
      TRUE ~ "Otro"
    )
  )

# Tabla de segmentación
cat("\n--- MATRIZ RIESGO vs GRUPO EDAD ---\n")
risk_matrix <- table(datos$Riesgo, datos$Grupo_Edad)
print(risk_matrix)
print(prop.table(risk_matrix, margin = 2))

# Análisis de pagos por segmento
cat("\n--- PAGOS PROMEDIO POR SEGMENTO ---\n")
segment_analysis <- datos %>%
  group_by(Riesgo, Grupo_Edad) %>%
  summarise(
    Media_Pago = mean(Payment, na.rm=T),
    Mediana_Pago = median(Payment, na.rm=T),
    DesvEst_Pago = sd(Payment, na.rm=T),
    N_Clientes = n(),
    Prop_Riesgo = n()/nrow(datos)*100,
    .groups = 'drop'
  )
print(segment_analysis)

# ============================================================================
# 6. MODELADO DE DISTRIBUCIONES (ACTUARIAL MODELING)
# ============================================================================
cat("\n==== PASO 6: MODELADO DE DISTRIBUCIONES DE PAGO ====\n")

# Test de normalidad (Shapiro-Wilk)
cat("\n--- TEST DE NORMALIDAD (Shapiro-Wilk) ---\n")
shapiro_test <- shapiro.test(datos$Payment[!is.na(datos$Payment)])
cat("Estadístico:", round(shapiro_test$statistic, 4), "\n")
cat("P-valor:", round(shapiro_test$p.value, 6), "\n")
if(shapiro_test$p.value < 0.05) {
  cat("Conclusión: Los pagos NO siguen distribución normal (p < 0.05)\n")
  cat("Implicación: Usar métodos no paramétricos o transformaciones\n")
}

# Análisis de log-normalidad
cat("\n--- TEST DE LOG-NORMALIDAD ---\n")
log_payment <- log(datos$Payment)
shapiro_log <- shapiro.test(log_payment[!is.na(log_payment)])
cat("P-valor (log-escala):", round(shapiro_log$p.value, 6), "\n")

# ============================================================================
# 7. ANÁLISIS DE COHORTES (COHORT ANALYSIS)
# ============================================================================
cat("\n==== PASO 7: ANÁLISIS DE COHORTES ====\n")

# Cohortes por género y riesgo
cat("\n--- DESEMPEÑO POR COHORTE ---\n")
cohort_analysis <- datos %>%
  group_by(Gender, Riesgo) %>%
  summarise(
    N = n(),
    Pago_Promedio = mean(Payment, na.rm=T),
    Pago_Mediano = median(Payment, na.rm=T),
    CV = sd(Payment, na.rm=T) / mean(Payment, na.rm=T),
    Min = min(Payment, na.rm=T),
    Max = max(Payment, na.rm=T),
    .groups = 'drop'
  )
print(cohort_analysis)

# ============================================================================
# 8. IDENTIFICACIÓN DE FACTORES DE RIESGO
# ============================================================================
cat("\n==== PASO 8: FACTORES DE RIESGO IDENTIFICADOS ====\n")

# Tasa de churn por grupo de edad
cat("\n--- TASA DE CHURN POR GRUPO EDAD ---\n")
churn_by_age <- datos %>%
  filter(!is.na(Churn)) %>%
  group_by(Grupo_Edad) %>%
  summarise(
    Total = n(),
    Churn_Count = sum(Churn == "churn"),
    Tasa_Churn = sum(Churn == "churn")/n() * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(Tasa_Churn))
print(churn_by_age)

# Tasa de churn por método de pago
cat("\n--- TASA DE CHURN POR MÉTODO DE PAGO ---\n")
churn_by_method <- datos %>%
  filter(!is.na(Churn)) %>%
  group_by(Payment.Method) %>%
  summarise(
    Total = n(),
    Churn_Count = sum(Churn == "churn"),
    Tasa_Churn = sum(Churn == "churn")/n() * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(Tasa_Churn))
print(churn_by_method)

# Tasa de churn por género
cat("\n--- TASA DE CHURN POR GÉNERO ---\n")
churn_by_gender <- datos %>%
  filter(!is.na(Churn)) %>%
  group_by(Gender) %>%
  summarise(
    Total = n(),
    Churn_Count = sum(Churn == "churn"),
    Tasa_Churn = sum(Churn == "churn")/n() * 100,
    Pago_Promedio = mean(Payment, na.rm=T),
    .groups = 'drop'
  )
print(churn_by_gender)

# ============================================================================
# 9. ANÁLISIS DE DATOS FALTANTES (MISSING DATA STRATEGY)
# ============================================================================
cat("\n==== PASO 9: ESTRATEGIA PARA DATOS FALTANTES ====\n")

# Ubicación de valores faltantes
cat("\n--- UBICACIÓN DE FALTANTES ---\n")
missing_location <- datos %>%
  filter(is.na(Churn)) %>%
  summarise(
    N_Faltantes = n(),
    Edad_Media = mean(Age, na.rm=T),
    Pago_Promedio = mean(Payment, na.rm=T),
    Prop_Faltante = n()/nrow(datos)*100
  )
print(missing_location)

cat("\nRecomendación: Imputar basado en género, edad y método de pago\n")

# ============================================================================
# 10. RESUMEN EJECUTIVO ACTUARIAL
# ============================================================================
cat("\n==== PASO 10: RESUMEN EJECUTIVO ====\n")

cat("\n📊 ESTADÍSTICAS CLAVE:\n")
cat("├─ Tamaño Cartera:", nrow(datos), "clientes\n")
cat("├─ Pago Promedio:", round(mean(datos$Payment, na.rm=T), 2), "unidades\n")
cat("├─ Rango Pagos: [", round(min(datos$Payment, na.rm=T), 2), 
    "-", round(max(datos$Payment, na.rm=T), 2), "]\n")
cat("├─ Desviación Estándar:", round(sd(datos$Payment, na.rm=T), 2), "\n")
cat("└─ Valores Faltantes:", sum(is.na(datos$Churn)), "(",
    round(sum(is.na(datos$Churn))/nrow(datos)*100, 2), "%)\n")

cat("\n⚠️  RIESGOS IDENTIFICADOS:\n")
tasa_churn_total <- sum(datos$Churn == "churn", na.rm=T) / 
                    sum(!is.na(datos$Churn)) * 100
cat("├─ Tasa de Churn Global:", round(tasa_churn_total, 2), "%\n")
cat("├─ Grupo más riesgoso:", 
    as.character(churn_by_age$Grupo_Edad[1]),
    "(",
    round(churn_by_age$Tasa_Churn[1], 2), "%)\n")
cat("├─ Método pago más riesgoso:", 
    as.character(churn_by_method$Payment.Method[1]),
    "(",
    round(churn_by_method$Tasa_Churn[1], 2), "%)\n")
cat("└─ Variabilidad de Pagos (CV):", 
    round(sd(datos$Payment, na.rm=T)/mean(datos$Payment, na.rm=T), 4), "\n")

cat("\n💡 RECOMENDACIONES:\n")
cat("1. Monitorear clientes del grupo:", as.character(churn_by_age$Grupo_Edad[1]), "\n")
cat("2. Revisar proceso de:", as.character(churn_by_method$Payment.Method[1]), "\n")
cat("3. Imputar", sum(is.na(datos$Churn)), "valores faltantes antes de modelado\n")
cat("4. Considerar modelo de predicción de churn\n")
cat("5. Analizar correlación edad-pago para pricing\n")

# ============================================================================
cat("\n✅ ANÁLISIS COMPLETADO\n")
# ============================================================================
