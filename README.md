# Posgrado_Estadistica_2025
Material del curso de Posgrado de experimentacion y metodos estadisticos en MCF y DCMRN

##Primera Clase Estadistica
**Semana 1** (07/08/2025)Inicio de curso
+crear 
+sincronizar
+credenciales

##configuracion desde computadora personal
+sincronizado
+listo

## Tarea 1 parte 1
 
#Gastos totales
300 + 240 +1527 + 400 + 1500 + 1833

celular <- 300
celular

trasporte <- 240
trasporte

combustible <- 1527
combustible

gym <- 400
gym

renta <- 1500
renta

otros <- 1833
otros

total <- 5800
total

total * 5
total * 10

gastos <- c(celular, trasporte, combustible, gym, renta, otros) 

barplot(gastos)

?sort

sort(gastos, decreasing = TRUE)

gastosordenados <- sort(gastos, decreasing = TRUE)

barplot(gastosordenados)

nombres_variables <- c("otros", "combustibles", "renta", "gym", "celular", "trasporte")

barplot(gastosordenados, names.arg = nombres_variables)

# Parte 1: Análisis de Gastos Mensuales

## Cálculo de Gastos

```{r calculo_gastos, include=FALSE}
# Gastos mensuales
celular <- 300
transporte <- 240
combustible <- 1527
gym <- 400
renta <- 1500
otros <- 1833

# Vector de gastos
gastos <- c(celular, transporte, combustible, gym, renta, otros)
nombres <- c("Celular", "Transporte", "Combustible", "Gym", "Renta", "Otros")

# Cálculo del total
total <- sum(gastos)

# Proyección a 5 y 10 meses
proyeccion_5_meses <- total * 5
proyeccion_10_meses <- total * 10

# Ordenar gastos de mayor a menor
gastos_ordenados <- sort(gastos, decreasing = TRUE)
nombres_gastos <- c("Otros", "Combustible", "Renta", "Gym", "Celular", "Transporte")
```

### Resultados Numéricos

- **Total de gastos mensuales:** \$`r total`
- **Proyección a 5 meses:** \$`r proyeccion_5_meses`
- **Proyección a 10 meses:** \$`r proyeccion_10_meses`

## Gráfico de Gastos

```{r grafico_barras, fig.cap="Distribución de Gastos Mensuales ordenados de mayor a menor", fig.height=5, fig.width=8}
# Gráfico de barras
barplot(gastos_ordenados, names.arg = nombres_gastos)

        
```



# Parte 2: Clasificación de Variables

## Problema 1: Clasificación de Variables Estudiantiles

**Variables Cualitativas:**
- Nombre del estudiante
- Dirección
- Número de teléfono
- Grado universitario

**Variables Cuantitativas:**
- Fecha de nacimiento (cuantitativa)
- Edad (cuantitativa)
- Área de estudio (cuantitativa si se codifica numéricamente)
- Calificación numérica (cuantitativa)
- Tiempo (cuantitativa)
- Número de hermanos (cuantitativa)

- Calificación con letras: Semicuantitativa 

## Problema 2: Variables en el Estudio de Animales

**Variables Cuantitativas:**
- Edad
- Peso
- Longitud
- Número de descendencia
- Tiempo de gestación
- Temperatura
- Velocidad

**Variables Cualitativas:**
- Especie
- Dieta
- Color
- Tipo de actividad
- Tipo de reproducción
- Lugar de hábitat
- Distribución geográfica

## Problema 3: Escala de Opinión sobre Medios Electrónicos

Los medios electrónicos para expresar opinión son variables semicuantitativas. Inicialmente son cualitativos, pero al asignarles valores numéricos (1-5) se convierten en variables ordinales que permiten análisis cuantitativos de frecuencias etc...

## Problema 4: Identificación de Individuos y Variables

1. **Individuo:** Estudiante de universidad pública  
   **Variable:** Tiempo de trabajo  
   **Tipo:** Cuantitativa 

2. **Individuo:** Estudiantes universitarios de México  
   **Variable:** Proporción de estudiantes en públicas  
   **Tipo:** Cuantitativa 

3. **Individuo:** Estudiantes por sexo  
   **Variable:** Calificación de CENEVAL  
   **Tipo:** Cuantitativa 

4. **Individuo:** Atletas  
   **Variable:** Asesoramiento académico  
   **Tipo:** Cualitativa 

## Problema 5: Visualización de Datos Cualitativos

Para variables cualitativas no se pueden usar histogramas, ya que estos requieren variables cuantitativas continuas. Para estso datos se utiliza, Gráfico de barras,Gráfico de pastel, Gráfico de mosaico;  Para comparar frecuencias entre categorías,mostrar proporciones porcentuales y visualizar relaciones entre múltiples variables respectivamente


### Tarea 2 Iris
# Primer contacto con R
´´´
{r}
iris
View (iris)

data_sub <- subset(iris, Species %in% c("versicolor", "virginica"))
table(data_sub)

View(data_sub)

head (data_sub)

summary (data_sub)

# Petal.Length por especie
tapply(data_sub$Petal.Length, data_sub$Species, summary)

# Prueba estadistica:

# Pregunta de investigacion: ¿Existe diferencia significativa en la longitud del pétalo entre ambas especies?

# Hipotesis: No existe diferencia en la longitud del petalo H₀: μ_versicolor = μ_virginica, existe una diferencia significativa en la longitud H₁: μ_versicolor ≠ μ_virginica

var.test(Petal.Length ~ Species, data = data_sub)

t.test(Petal.Length ~ Species, data = data_sub,  var.equal = TRUE,   # Asume varianzas iguales
alternative = "two.sided")

# Medias y desviaciones estándar
media_virginica <- mean(data_sub$Petal.Length[data_sub$Species == "virginica"])
media_versicolor <- mean(data_sub$Petal.Length[data_sub$Species == "versicolor"])
sd_virginica <- sd(data_sub$Petal.Length[data_sub$Species == "virginica"])
sd_versicolor <- sd(data_sub$Petal.Length[data_sub$Species == "versicolor"])

# Desviación estándar pooled
n_virginica <- sum(data_sub$Species == "virginica")
n_versicolor <- sum(data_sub$Species == "versicolor")
sd_pooled <- sqrt(((n_virginica-1)*sd_virginica^2 + (n_versicolor-1)*sd_versicolor^2) / 
                  (n_virginica + n_versicolor - 2))

# Cohen's d
d_cohen <- (media_virginica - media_versicolor) / sd_pooled
print(d_cohen)

ggplot(data_sub, aes(x = Species, y = Petal.Length, fill = Species)) + geom_boxplot(alpha = 0.8) +  labs(title = "Diferencia en Longitud del Pétalo entre Especies", subtitle = "Iris versicolor vs Iris virginica", x = "Especies", y = "Longitud del Pétalo (cm)", caption = paste("Prueba t: p < 2.2e-16",                    "Cohen's d ≈ -2.5 (efecto muy grande)", sep = "\n")) +
scale_fill_manual(values = c("versicolor" = "#FF6B6B",
"virginica" = "#4ECDC4")) + theme_minimal() +   theme(legend.position = "none")

library(ggplot2)
  



