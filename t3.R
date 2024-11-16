library(readxl)
library(dplyr)
HW3 <- read_excel("tarea3TEFI/HW3.xlsx")

# Calcular los rendimientos en exceso
HW3_exceso <- HW3 %>%
  mutate(across(3:27, ~ .x - RF))

# Lista de nombres de portafolios
portafolios <- colnames(HW3)[3:27]

# Crear una matriz para almacenar los resultados
resultados <- matrix(NA, nrow = length(portafolios), ncol = 8)
colnames(resultados) <- c("Alfa", "Error_Alfa", "t_Alfa", "Beta", "Error_Beta", "t_Beta", "R2", "Portafolio")

# Bucle para ajustar la regresión para cada portafolio y guardar los resultados
for (i in seq_along(portafolios)) {
  port <- portafolios[i]
  
  # Ajustar el modelo de regresión lineal
  fit <- lm(HW3_exceso[[port]] ~ MKTRF, data = HW3_exceso)
  
  # Extraer coeficientes y R²
  coef <- summary(fit)$coefficients
  r2 <- summary(fit)$r.squared
  
  # Guardar los resultados en la matriz
  resultados[i, ] <- c(
    round(coef[1, 1], 4),  # Intercepto (Alfa)
    round(coef[1, 2], 4),  # Error estándar del Intercepto
    round(coef[1, 3], 4),  # t del Intercepto
    round(coef[2, 1], 4),  # Beta
    round(coef[2, 2], 4),  # Error estándar de Beta
    round(coef[2, 3], 4),  # t de Beta
    round(r2, 4),          # R2
    port                   # Nombre del portafolio
  )
}

# Convertir la matriz a un DataFrame
resultados_df <- as.data.frame(resultados)

# Ver los resultados
print(resultados_df)
colnames(resultados_df) <- c("Alfa", "Beta", "Error_Alfa", "Error_Beta", "t_Alfa", "t_Beta", "R2", "Portafolio", "P-valor_Beta")