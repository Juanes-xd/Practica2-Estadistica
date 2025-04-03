install.packages("easypackages")
library("easypackages")

# Listado de librerías requeridas
lib_req <- c("ggplot2", "dplyr", "readxl", "utils")
library(tidyr)
library(GGally)

# Verificación, instalación y carga de librerías
easypackages::packages(lib_req)

base <- read_excel("datos2.xlsx")

# Ver las primeras filas
head(base)
# Ver la estructura
str(base)
# Resumen estadístico básico
summary(base)

# Tabla de frecuencia por continente
tabla_continente <- table(base$Continente)
kable(tabla_continente, col.names = c("Continente", "Frecuencia")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Tabla de resumen estadístico
kable(summary(base[, -c(1, 9)])) %>% # Excluir país y continente
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

ggplot(base, aes(x = Libertad, y = Felicidad, color = Continente)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relación entre Felicidad y la Libertad",
       x = "Libertad", y = "Puntuación de Felicidad") +
  theme_minimal()

ggplot(base, aes(x = Apoyo, y = Felicidad, label = País)) +
  geom_point(aes(color = Continente), size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(title = "Relación entre Felicidad y Apoyo Social",
       x = "Apoyo Social", y = "Puntuación de Felicidad") +
  theme_minimal()

ggpairs(base[, c("Felicidad", "PIB", "Apoyo", "Esperanza_vida")]) +
  labs(title = "Matriz de correlación entre variables clave")

base %>%
  arrange(.,(Felicidad)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(País, Felicidad), y = Felicidad, fill = Continente)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 15 países más felices", x = "País", y = "Puntuación de Felicidad") +
  theme_minimal()

ggplot(base, aes(x = Continente, y = Felicidad, fill = Continente)) +
  geom_boxplot() +
  labs(title = "Distribución de Felicidad por Continente") +
  theme_minimal()
