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
hist(datos$Libertad, main="Histograma del Libertad per cápita", xlab="Libertad per cápita")
hist(datos$Libertad, main="Histograma del Libertad social", xlab="Libertad social")
hist(datos$Esperanza_vida, main="Histograma de la esperanza de vida", xlab="Esperanza de vida")
hist(datos$Felicidad, main="Histograma de felicidad", xlab="Felicidad")

#Graficos de cajas
boxplot(datos$Libertad, main="Grafico del Libertad per cápita")
boxplot(datos$Corrupcion, main="Grafico de corrupcion")

#Variables
datos$Felicidad <- as.numeric(datos$Felicidad)  # Felicidad (X)
datos$Corrupcion <- as.numeric(datos$Corrupcion)  # Corrupcion (Y)
datos$Esperanza_vida <- as.numeric(datos$Esperanza_vida)  # Esperanza_vida (Y)
datos$Libertad <- as.numeric(datos$Libertad)  # Libertad(Y)
datos$Libertad <- as.numeric(datos$Libertad)  # Libertad (Y)
datos$Generosidad <- as.numeric(datos$Generosidad)  # Libertad (Y)
datos$Libertad <- as.numeric(datos$Libertad)  # Libertad (Y)

# 1. Gráfico de dispersión
plot(datos$Felicidad,datos$Libertad, 
     xlab = "Felicidad", 
     ylab = "Libertad", 
     main = "Libertad vs Felicidad")
abline(lm(datos$Libertad~datos$Felicidad,data = datos),col="red")

#Con ggplot2
ggplot(datos, aes(x = Felicidad, y = Libertad)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Libertad vs Felicidad",
       x = "Felicidad",
       y = "Libertad") +
  theme_minimal()

# 2. Matriz de correlación
cor_matrix <- cor(datos[, c("Felicidad", "Libertad")])
cor_matrix

#Con ggpairs 
ggcor <- ggpairs(datos[, c("Felicidad", "Libertad")])
ggcor

# 3. Ajuste del modelo de regresión
modelo <- lm(Libertad ~ Felicidad, data = datos)

# 4. Resumen del modelo
summary(modelo)

# Coef correlación es el cuadrado del Coef determinación solo en simple

r2 <-(cor_matrix[1,2])^{2}


#Gráfico de dispersión con la ecuación de la recta encontrada
coeficientes <- coef(modelo)
plot(datos$Felicidad, datos$Libertad, 
     xlab = "Felicidad", 
     ylab = "Libertad", 
     main = "Libertad vs Felicidad")
abline(modelo, col = "red", lwd = 2)
eq_text <- paste0("y = ", round(coeficientes[1], 2), " + ", round(coeficientes[2], 2), "x")
legend("topright", legend = eq_text, col = "red", lty = 1, bty = "o") #si cambiana n por o se agrega un cuadro


