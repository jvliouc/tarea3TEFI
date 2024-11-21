library(readxl)
library(dplyr)
HW3 <- read_excel("tarea3TEFI/HW3.xlsx")

# los rendimientos en exceso
HW3_exceso <- HW3 %>%
  mutate(across(3:27, ~ .x - RF))

# lsta de nombres de portafolios
portafolios <- colnames(HW3)[3:27]

#  matriz para almacenar los resultados
resultados <- matrix(NA, nrow = length(portafolios), ncol = 8)

# un for soluciona todo
for (i in seq_along(portafolios)) {
  port <- portafolios[i]
  fit <- lm(HW3_exceso[[port]] ~ MKTRF, data = HW3_exceso)
  coef <- summary(fit)$coefficients
  r2 <- summary(fit)$r.squared
  resultados[i, ] <- c(
    port,                 # Nombre del portafolio
    round(coef[1, 1], 4),  # Intercepto (Alfa)
    round(coef[1, 2], 4),  # Error estándar del Intercepto
    round(coef[1, 3], 4),  # t del Intercepto
    round(coef[2, 1], 4),  # Beta
    round(coef[2, 2], 4),  # Error estándar de Beta
    round(coef[2, 3], 4),  # t de Beta
    round(r2, 4)         # R2

  )
}

# pasar a df y poniendo nombre a las weas de arriba ñau siguiendo el orden de arriba
resultados_df <- as.data.frame(resultados)
colnames(resultados_df) <- c("Portafolio", "Alfa", "error estándar Alfa", "Estadístico Alfa", "Beta", "error estándar Beta", "Estadístico Beta", "R2")




resultados_df <- resultados_df %>%
  mutate(
    Alfa = as.numeric(Alfa),
    `error estándar Alfa` = as.numeric(`error estándar Alfa`),
    `Estadístico Alfa` = as.numeric(`Estadístico Alfa`),
    Beta = as.numeric(Beta),
    `error estándar Beta` = as.numeric(`error estándar Beta`),
    `Estadístico Beta` = as.numeric(`Estadístico Beta`),
    R2 = as.numeric(R2)
  )

#la d ahora, pa hacer esta wea hay que calcular los promedios de los excesos de cada portafolio
medias<-rep(1,25)
for (i in seq_along(portafolios)) {
  port <- portafolios[i]
  media<-mean(HW3_exceso[[port]])
  medias[i]<-as.numeric(media)
}

cross_sectional<-data.frame(
  nombre_portafolio=LETTERS[1:25],
  Betas=as.numeric(resultados_df$Beta),
  Promedios=medias
)
# regresion

corte_tranversal<-lm(Promedios~Betas,data = cross_sectional)
resumen=summary(corte_tranversal)
resumen$r.squared
#R2 del 5% habra algo malo?
#------------------------------------------------------------------------------
# Instalar y cargar los paquetes necesarios
install.packages("officer")
install.packages("flextable")
library(officer)
library(flextable)

# Crear el documento de Word
doc <- read_docx()

# Agregar una tabla al documento con el dataframe 'resultados_df'
doc <- doc %>%
  body_add_flextable(flextable(resultados_df))

# Guardar el documento en el disco
print(doc, target = "resultados_portafolios.docx")