# Cargar librerías necesarias
library(ggplot2)
library(GGally)
library(readxl)
datos <- read_excel("Datos Problema Practica 2.xlsx")
View(datos)

#Resumen de estadisticas descriptivas
resumen <- summary(datos)
View(resumen)

#Histogramas
hist(datos$PIB, main="Histograma del PIB per cápita", xlab="PIB per cápita")
hist(datos$Apoyo, main="Histograma del Apoyo social", xlab="Apoyo social")
hist(datos$Esperanza_vida, main="Histograma de la esperanza de vida", xlab="Esperanza de vida")
hist(datos$Felicidad, main="Histograma de felicidad", xlab="Felicidad")

#Graficos de cajas
boxplot(datos$PIB, main="Grafico del PIB per cápita")
boxplot(datos$Corrupcion, main="Grafico de corrupcion")

#Variables
datos$Felicidad <- as.numeric(datos$Felicidad)  # Felicidad (X)
datos$Corrupcion <- as.numeric(datos$Corrupcion)  # Corrupcion (Y)

# 1. Gráfico de dispersión
plot(datos$Felicidad,datos$Corrupcion, 
     xlab = "Felicidad", 
     ylab = "Corrupcion", 
     main = "Corrupcion vs Felicidad")
abline(lm(datos$Corrupcion~datos$Felicidad,data = datos),col="red")

#Con ggplot2
ggplot(datos, aes(x = Felicidad, y = Corrupcion)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Corrupcion vs Felicidad",
       x = "Felicidad",
       y = "Corrupcion") +
  theme_minimal()

# 2. Matriz de correlación
cor_matrix <- cor(datos[, c("Felicidad", "Corrupcion")])
cor_matrix

#Con ggpairs 
ggcor <- ggpairs(datos[, c("Felicidad", "Corrupcion")])
ggcor

# 3. Ajuste del modelo de regresión
modelo <- lm(Corrupcion ~ Felicidad, data = datos)

# 4. Resumen del modelo
summary(modelo)

# Coef correlación es el cuadrado del Coef determinación solo en simple

r2 <-(cor_matrix[1,2])^{2}


#Gráfico de dispersión con la ecuación de la recta encontrada
coeficientes <- coef(modelo)
plot(datos$Felicidad, datos$Corrupcion, 
     xlab = "Felicidad", 
     ylab = "Corrupcion", 
     main = "Corrupcion vs Felicidad")
abline(modelo, col = "red", lwd = 2)
eq_text <- paste0("y = ", round(coeficientes[1], 2), " + ", round(coeficientes[2], 2), "x")
legend("topright", legend = eq_text, col = "red", lty = 1, bty = "n") #si cambiana n por o se agrega un cuadro


