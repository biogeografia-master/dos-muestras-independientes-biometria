Prácticas de aula 2 (PA02). Inferencia estadística a partir de dos
muestras independientes usando datos biométricos<small><br>Biogeografía
(GEO-131)<br>Universidad Autónoma de Santo Domingo (UASD)<br>Semestre
2024-02</small>
================
El Tali
2024-08-27

Versión HTML (quizá más legible),
[aquí](https://biogeografia-master.github.io/dos-muestras-independientes-biometria/README.html)

# Fecha/hora de entrega

**27 de agosto de 2024, 7:59 pm.**

# Justificación

Aprender a aplicar pruebas estadísticas en biogeografía es crucial
porque permite analizar datos espaciales y ecológicos complejos, tomar
decisiones basadas en evidencia, y evaluar hipótesis sobre la
distribución de la biodiversidad. Las técnicas estadísticas ayudan a
manejar la variabilidad natural, comparar grupos y regiones, y
desarrollar modelos predictivos que son esenciales para la planificación
y conservación. Además, garantizan la solidez de los resultados en
publicaciones científicas, asegurando que los estudios sean rigurosos y
reproducibles.

Con el advenimiento del machine learning, deep learning e inteligencia
artificial, la biogeografía ha experimentado avances significativos,
permitiendo la construcción de modelos predictivos más precisos, la
automatización del análisis de grandes volúmenes de datos espaciales, y
la identificación de patrones complejos en la distribución de especies.
Sin embargo, la estadística sigue siendo fundamental para validar estos
modelos, interpretar resultados de manera rigurosa y garantizar la
reproducibilidad científica, sobre todo cuando lo que tenemos a mano son
muestras pequeñas. Mientras que la IA ofrece nuevas herramientas
poderosas, las pruebas estadísticas siguen siendo esenciales para
fundamentar hipótesis, comparar estudios y proporcionar un marco de
referencia sólido en la investigación biogeográfica. Ya no hablemos de
la modelización en presencia de mediciones repetidas, donde no se
satisfacen varios de los supuestos de las técnicas tradicionales,
relevando el papel de técnicas de modelización robustas muy usadas en
ecología (e.g. modelos de efecto mixto). Como digo, la muestra pequeña
sigue siendo predominante en ecología y biogeografía, y la estadística
ha demostrado ser muy robusta en estos casos.

# Ejercicio 1: Comparación de las Medidas de los Dedos entre Dos Estudiantes

## Objetivo

Aplicar la prueba t de Student para muestras pareadas, comparando las
medidas de los dedos de un estudiante (Muestra 1) con las medidas de los
dedos de otro estudiante (Muestra 2).

Este ejercicio te permitirá determinar si hay diferencias significativas
entre las medidas de los dedos de dos personas diferentes. Para
mantenerlo simple, no haremos comprobación de supuestos, sólo
aplicaremos la prueba. La comprobación de supuestos la abordaremos en
otra práctica.

Es importante que, cuando termines esta práctica, estudies sobre los
procedimientos usados, por qué en cada caso se usa una fórmula
específica, o qué criterios se usan para interpretar el resultado final.
Esto lo podrás encontrar en cualquier libro de estadística, pero te
recomiendo que uses a Triola (2012). Igualmente, documéntate sobre la
significancia como concepto y sobre la crisis que existe actualmente en
su aplicación. [Este
artículo](https://www.jvsmedicscorner.com/Statistics_files/Retire%20statistical%20significance.pdf)
de opinión, de Amrhein y otros (2019), puede ser un buen punto de
partida (hay “pila” de artículos sobre este tema, que conste).
Documéntate también sobre conceptos como “tamaño del efecto” y “diseño
experimental”, para lo cual te recomiendo [este trabajo de Frank y otros
(2021)](https://australianprescriber.tg.org.au/articles/is-it-time-to-stop-using-statistical-significance.html).

## Planteamiento del Problema

Se cuenta con las medidas de los cinco dedos de una mano de varios
estudiantes, registradas en un formulario que se almacena en una hoja de
cálculo (archivo `biometria-basica.csv`). Se han creado 20 conjuntos de
datos al azar, donde se señalan “Muestra_1” y “Muestra_2”. Debes elegir
uno de los 20 conjuntos asegurándote de no duplicar con otro/a
compañero/a.

Para las dos muestras de tu conjunto, compararás las medidas de los
cinco dedos, de forma pareada. En este caso, “pareada” significa, que
harás una comparación considerando el dedo medido, es decir,
pulgar-a-pulgar, índice-a-índice, y así. La comparación se realiza
utilizando la prueba t de Student pareada para determinar si hay
diferencias significativas entre las medidas de los dedos de dos
estudiantes (o muestras, en este caso, “Muestra_1” y “Muestra_2”).

## Recolección de Datos

1.  **Creación de los 20 conjuntos**: Se han creado 20 conjuntos de
    pares de estudiantes al azar utilizando sus pseudónimos con el
    siguiente código de R (si no lo ves, presiona el botón `Show`).

``` r
# Cargar los datos
data <- read.csv("biometria-basica.csv", check.names = F)
colnames(data) <- gsub("\\..*|\\(.*", "", colnames(data))

# Combinaciones
combinaciones <- t(combn(trimws(data[, grep('^Nombre', colnames(data))]), 2))
set.seed(999) # Fijar la semilla para reproducibilidad
seleccion <- combinaciones[sample(1:nrow(combinaciones), 20, replace = F), ]

# Crear la tabla con 20 conjuntos a partir de pseudónimos de estudiantes
conjuntos_par <- data.frame(
  Conjunto = paste("Conjunto", 1:20),
  Muestra_1 = seleccion[,1],
  Muestra_2 = seleccion[,2]
)
```

``` r
knitr::kable(conjuntos_par)
```

| Conjunto    | Muestra_1        | Muestra_2        |
|:------------|:-----------------|:-----------------|
| Conjunto 1  | Saderis          | Lenin Del Prado  |
| Conjunto 2  | Jennifer         | Linette          |
| Conjunto 3  | Katherine Galván | Linette          |
| Conjunto 4  | Patty            | Mai              |
| Conjunto 5  | Lay              | Saderis          |
| Conjunto 6  | Jennifer         | Mai              |
| Conjunto 7  | Katherine Galván | Mai              |
| Conjunto 8  | Iliana           | Luis Bautista    |
| Conjunto 9  | Lay              | Luis Bautista    |
| Conjunto 10 | Saderis          | Sebastian Bocio  |
| Conjunto 11 | Lay              | Linette          |
| Conjunto 12 | Katherine Galván | Jennifer         |
| Conjunto 13 | Mai              | Linette          |
| Conjunto 14 | Jennifer         | Sebastian Bocio  |
| Conjunto 15 | Iliana           | Patty            |
| Conjunto 16 | Olga Suriel      | Katherine Galván |
| Conjunto 17 | Katherine Galván | Sebastian Bocio  |
| Conjunto 18 | Lay              | Patty            |
| Conjunto 19 | Luis Bautista    | Linette          |
| Conjunto 20 | Olga Suriel      | Lenin Del Prado  |

2.  **Obtención de las medidas de los dedos**: Las medidas de los cinco
    dedos de la mano de cada estudiante están registradas en la hoja de
    cálculo proporcionada, la cual transcribo abajo. También encuentra
    en este mismo repo (archivo `biometria-basica.csv`) y en [esta
    ruta](https://docs.google.com/spreadsheets/d/14JrVEx-oKtIsGCDFh049DDtoh7o4FQJWFuDWNYS4nfk/edit?usp=sharing).

``` r
# Mostrar la tabla generada
knitr::kable(data)
```

| Marca temporal      | Nombre           | Género | Pulgar | Índice | Mayor | Anular | Meñique | Estatura |
|:--------------------|:-----------------|:-------|-------:|-------:|------:|-------:|--------:|---------:|
| 20/08/2024 17:20:02 | Iliana           | Mujer  |    6.5 |    7.5 |   7.5 |    6.9 |     5.6 |   167.60 |
| 20/08/2024 17:20:43 | Lay              | Mujer  |    6.0 |    6.5 |   7.5 |    7.0 |     6.0 |       NA |
| 20/08/2024 17:21:36 | Saderis          | Mujer  |    6.1 |    7.4 |   7.5 |    6.8 |     5.9 |   157.00 |
| 20/08/2024 17:24:44 | Olga Suriel      | Mujer  |    5.0 |    5.7 |   6.6 |    5.8 |     5.0 |   152.40 |
| 20/08/2024 17:26:17 | Lenin Del Prado  | Hombre |    5.9 |    6.9 |   7.6 |    7.1 |     5.6 |     1.71 |
| 20/08/2024 17:26:32 | Katherine Galván | Mujer  |    5.5 |    6.2 |   7.0 |    6.5 |     5.3 |   167.64 |
| 20/08/2024 17:27:02 | Jennifer         | Mujer  |    6.4 |    7.1 |   8.3 |    7.4 |     5.9 |   170.70 |
| 20/08/2024 17:27:40 | Patty            | Mujer  |    7.0 |    8.0 |   8.2 |    7.2 |     6.0 |   161.10 |
| 20/08/2024 17:27:42 | Mai              | Mujer  |    5.3 |    6.6 |   7.2 |    6.6 |     5.4 |       NA |
| 20/08/2024 17:28:14 | Deb              | Mujer  |    5.0 |    6.0 |   6.5 |    6.9 |     4.9 |   149.10 |
| 20/08/2024 17:28:29 | Luis Bautista    | Hombre |    6.1 |    7.3 |   7.9 |    7.4 |     5.8 |   173.73 |
| 20/08/2024 17:30:00 | Linette          | Mujer  |    6.2 |    6.5 |   7.6 |    6.8 |     5.7 |   162.60 |
| 20/08/2024 17:30:10 | Sebastian Bocio  | Hombre |    7.2 |    8.3 |   9.2 |    8.5 |     7.0 |   178.00 |
| 20/08/2024 17:33:34 | Isaura           | Mujer  |    5.6 |    7.1 |   8.1 |    7.4 |     6.2 |   162.00 |

3.  **Mandato**. Aplica la prueba t de Student para muestras pareadas,
    comparando las medidas de los dedos de un estudiante (Muestra 1) con
    las medidas de los dedos de otro estudiante (Muestra 2) para
    determinar si hay diferencias significativas entre las medidas de
    los dedos de dos personas diferentes; usa un nivel de significancia
    de 0.05. Interpreta y/o resume los resultados con un pequeño párrafo
    que explique si las diferencias encontradas son estadísticamente
    significativas y qué implicaciones podrían tener en el contexto del
    estudio. **Importante**. Considera mirar, como referencia, la matriz
    de distancias/mapa de calor de la práctica anterior [(PA01.
    Generación de la matriz de
    distancias)](https://biogeografia-master.github.io/matriz-de-distancias/README.html),
    y encuentra la combinación que te tocó. No existe una ley estricta
    que sugiera un patrón común entre la matriz de distancia y el
    resultado de la prueba, pero normalmente sí hay consistencia entre
    ambas técnicas.

## Aplicación de la Prueba t de Student para Muestras Pareadas

### Fórmula de la prueba t para muestras pareadas

La prueba t de Student para muestras pareadas se utiliza para comparar
las medias de dos conjuntos de datos emparejados por medio de algún
atributo común, en este caso, “dedos de la mano”. La fórmula es la
siguiente:

$$
t = \frac{\bar{D}}{s_D / \sqrt{n}}
$$

Donde:

- $\bar{D}$ es la media de las diferencias entre las medidas
  emparejadas.
- $s_D$ es la desviación estándar de las diferencias.
- $n$ es el número de pares de datos (en este caso, 5).

### Ejemplo Práctico

Supongamos que tienes los siguientes datos de medidas de los dedos para
un par de estudiantes (Muestra 1 y Muestra 2):

|  Dedo   | Muestra 1 (cm) | Muestra 2 (cm) |
|:-------:|:--------------:|:--------------:|
| Pulgar  |      6.5       |      6.3       |
| Índice  |      7.2       |      7.0       |
|  Mayor  |      7.8       |      7.7       |
| Anular  |      7.4       |      7.2       |
| Meñique |      5.9       |      5.8       |

1.  **Calcular las diferencias $D_i$ entre las medidas de la Muestra 1 y
    la Muestra 2, donde $i$ representa cada dedo**:

$$
D = \text{Muestra 1} - \text{Muestra 2}
$$

|  Dedo   | Muestra 1 (cm) | Muestra 2 (cm) | $D_i$ |
|:-------:|:--------------:|:--------------:|:-----:|
| Pulgar  |      6.5       |      6.3       |  0.2  |
| Índice  |      7.2       |      7.0       |  0.2  |
|  Mayor  |      7.8       |      7.7       |  0.1  |
| Anular  |      7.4       |      7.2       |  0.2  |
| Meñique |      5.9       |      5.8       |  0.1  |

2.  **Calcular la media de las diferencias $\bar{D}$**:

$$
\bar{D} = \frac{\sum D_i}{n} = \frac{0.2 + 0.2 + 0.1 + 0.2 + 0.1}{5} = \frac{0.8}{5} = 0.16
$$

3.  **Calcular la desviación estándar de las diferencias $s_D$**:

Primero, calculamos $D_i - \bar{D}$ para cada diferencia:

|  Dedo   | $D_i$ | $D_i - \bar{D}$ | $(D_i - \bar{D})^2$ |
|:-------:|:-----:|:---------------:|:-------------------:|
| Pulgar  |  0.2  |      0.04       |       0.0016        |
| Índice  |  0.2  |      0.04       |       0.0016        |
|  Mayor  |  0.1  |      -0.06      |       0.0036        |
| Anular  |  0.2  |      0.04       |       0.0016        |
| Meñique |  0.1  |      -0.06      |       0.0036        |

Luego, sumamos todos los $(D_i - \bar{D})^2$ y calculamos $s_D$:

$$
s_D = \sqrt{\frac{\sum (D_i - \bar{D})^2}{n - 1}} = \sqrt{\frac{0.0016 + 0.0016 + 0.0036 + 0.0016 + 0.0036}{5 - 1}} = \sqrt{\frac{0.012}{4}} = \sqrt{0.003} \approx 0.055
$$

4.  **Calcular la estadística t**:

$$
t = \frac{\bar{D}}{s_D / \sqrt{n}} = \frac{0.16}{0.055 / \sqrt{5}} = \frac{0.16}{0.0246} \approx 6.50
$$

5.  **Grados de libertad**:

$$
df = n - 1 = 5 - 1 = 4
$$

6.  **Determinación del valor crítico:**

Para un nivel de significancia de 0.05 y 4 grados de libertad, el valor
crítico de t en una prueba de dos colas es aproximadamente 2.776 (lo
puedes obtener en R con el código `qt(p = 0.975, df = 4)`, pero también
lo puedes buscar en tablas estadísticas).

7.  **Conclusión:**

Dado que el valor calculado de t (6.50) es mayor que el valor crítico de
t (2.776), podemos rechazar la hipótesis nula. Esto significa que hay
una diferencia significativa entre las medidas de los dedos de los dos
estudiantes en este ejemplo.

### ¿Cómo se haría en R?

``` r
# Ejemplo en R
muestra_1 <- c(6.5, 7.2, 7.8, 7.4, 5.9)
muestra_2 <- c(6.3, 7.0, 7.7, 7.2, 5.8)
t.test(muestra_1, muestra_2, paired = TRUE)
```

------------------------------------------------------------------------

# Ejercicio 2: Comparación de medidas de dedos entre géneros

## Objetivo

El objetivo de este ejercicio es aplicar la prueba t de Student para
muestras independientes, comparando las medidas de los dedos entre
géneros (hombre y mujer). Se busca determinar si existe una diferencia
significativa entre las medidas de los dedos de hombres y mujeres.

Para mantener el ejercicio simple, usaremos muestras balanceadas, es
decir, cada muestra tendrá el mismo número de elementos. Como en el
conjunto de datos, sólo hay tres personas de género hombre, las muestras
serán por lo tanto de ese tamaño. Esto no implica que no pueda aplicarse
la prueba t de Student con muestras desbalanceadas (hay un límite en el
desabalance), sólo que para fines de cálculos es más sencillo. Es
importante tener en cuenta que, con tamaños de muestras tan pequeños, el
poder estadístico de la prueba se reduce mucho.

Por otro lado, al igual que en el ejercicio anterior, nos saltaremos la
comprobación de supuestos para mantener el ejercicio lo más simple
posible, y lo abordaremos en otra práctica.

## Planteamiento del Problema

Cada estudiante elegirá un conjunto. A diferencia del ejercicio
anterior, en este las muestras no son pareadas, por lo que la
comparación no será vis a vis. Cada conjunto, se compone de dos muestras
independientes. La muestra 1 contiene las mediciones de un mismo dedo de
tres personas género mujer elegidas al azar. La muestra 2 se compone
igualmente de las mediciones del mismo dedo, pero de las únicas tres
personas de género hombre (estas no han sido elegidas al azar, sino a
conveniencia). Se aplicará la prueba t de Student para muestras
independientes para determinar si hay diferencias significativas entre
las medidas de de dedos entre géneros.

## Recolección de Datos

1.  **Selección de individuos**: La tabla a continuación muestra 20
    combinaciones de tres hombres (elegidos a conveniencia, sólo se
    altera el orden) y otras 20 combinaciones de tres mujeres elegidas
    al azar, así como un dedo de la mano, también elegido al azar.

- Código con el que se generó el conjuntos de datos

``` r
# Selección de hombres y mujeres
hombres <- trimws(data[data$Género == "Hombre", "Nombre"])
mujeres <- trimws(data[data$Género == "Mujer", "Nombre"])

# Crea la tabla de conjuntos
set.seed(123) # Fija la semilla para reproducibilidad
conjuntos_ind <- data.frame(
  Conjunto = 1:20,
  Hombres = replicate(20, paste(sample(hombres, 3, replace = FALSE), collapse = ", ")),
  Mujeres_elegidas = replicate(20, paste(sample(mujeres, 3, replace = FALSE), collapse = ", ")),
  Dedo_elegido = replicate(20, sample(colnames(data)[4:8], 1))
)
```

``` r
knitr::kable(conjuntos_ind)
```

| Conjunto | Hombres                                         | Mujeres_elegidas                    | Dedo_elegido |
|---------:|:------------------------------------------------|:------------------------------------|:-------------|
|        1 | Sebastian Bocio, Lenin Del Prado, Luis Bautista | Lay, Iliana, Deb                    | Mayor        |
|        2 | Luis Bautista, Lenin Del Prado, Sebastian Bocio | Isaura, Deb, Jennifer               | Índice       |
|        3 | Luis Bautista, Sebastian Bocio, Lenin Del Prado | Katherine Galván, Deb, Olga Suriel  | Meñique      |
|        4 | Lenin Del Prado, Luis Bautista, Sebastian Bocio | Jennifer, Mai, Isaura               | Meñique      |
|        5 | Luis Bautista, Lenin Del Prado, Sebastian Bocio | Jennifer, Patty, Iliana             | Mayor        |
|        6 | Sebastian Bocio, Luis Bautista, Lenin Del Prado | Jennifer, Lay, Iliana               | Anular       |
|        7 | Sebastian Bocio, Lenin Del Prado, Luis Bautista | Lay, Olga Suriel, Katherine Galván  | Anular       |
|        8 | Lenin Del Prado, Sebastian Bocio, Luis Bautista | Jennifer, Saderis, Deb              | Anular       |
|        9 | Sebastian Bocio, Luis Bautista, Lenin Del Prado | Olga Suriel, Jennifer, Deb          | Meñique      |
|       10 | Sebastian Bocio, Luis Bautista, Lenin Del Prado | Deb, Patty, Saderis                 | Mayor        |
|       11 | Luis Bautista, Lenin Del Prado, Sebastian Bocio | Mai, Deb, Saderis                   | Pulgar       |
|       12 | Luis Bautista, Lenin Del Prado, Sebastian Bocio | Patty, Saderis, Isaura              | Índice       |
|       13 | Sebastian Bocio, Lenin Del Prado, Luis Bautista | Jennifer, Linette, Katherine Galván | Pulgar       |
|       14 | Sebastian Bocio, Luis Bautista, Lenin Del Prado | Katherine Galván, Mai, Saderis      | Índice       |
|       15 | Lenin Del Prado, Sebastian Bocio, Luis Bautista | Linette, Lay, Isaura                | Meñique      |
|       16 | Lenin Del Prado, Luis Bautista, Sebastian Bocio | Linette, Jennifer, Olga Suriel      | Mayor        |
|       17 | Sebastian Bocio, Luis Bautista, Lenin Del Prado | Iliana, Jennifer, Saderis           | Anular       |
|       18 | Sebastian Bocio, Lenin Del Prado, Luis Bautista | Mai, Saderis, Isaura                | Anular       |
|       19 | Luis Bautista, Lenin Del Prado, Sebastian Bocio | Iliana, Patty, Linette              | Pulgar       |
|       20 | Lenin Del Prado, Sebastian Bocio, Luis Bautista | Patty, Linette, Jennifer            | Anular       |

2.  **Obtención de las medidas de los dedos**: Las medidas del elegido
    de tus muestras están registradas en la hoja de cálculo
    proporcionada, la cual transcribo abajo. También encuentra en este
    mismo repo (archivo `biometria-basica.csv`) y en [esta
    ruta](https://docs.google.com/spreadsheets/d/14JrVEx-oKtIsGCDFh049DDtoh7o4FQJWFuDWNYS4nfk/edit?usp=sharing).

``` r
# Mostrar la tabla generada
knitr::kable(data)
```

| Marca temporal      | Nombre           | Género | Pulgar | Índice | Mayor | Anular | Meñique | Estatura |
|:--------------------|:-----------------|:-------|-------:|-------:|------:|-------:|--------:|---------:|
| 20/08/2024 17:20:02 | Iliana           | Mujer  |    6.5 |    7.5 |   7.5 |    6.9 |     5.6 |   167.60 |
| 20/08/2024 17:20:43 | Lay              | Mujer  |    6.0 |    6.5 |   7.5 |    7.0 |     6.0 |       NA |
| 20/08/2024 17:21:36 | Saderis          | Mujer  |    6.1 |    7.4 |   7.5 |    6.8 |     5.9 |   157.00 |
| 20/08/2024 17:24:44 | Olga Suriel      | Mujer  |    5.0 |    5.7 |   6.6 |    5.8 |     5.0 |   152.40 |
| 20/08/2024 17:26:17 | Lenin Del Prado  | Hombre |    5.9 |    6.9 |   7.6 |    7.1 |     5.6 |     1.71 |
| 20/08/2024 17:26:32 | Katherine Galván | Mujer  |    5.5 |    6.2 |   7.0 |    6.5 |     5.3 |   167.64 |
| 20/08/2024 17:27:02 | Jennifer         | Mujer  |    6.4 |    7.1 |   8.3 |    7.4 |     5.9 |   170.70 |
| 20/08/2024 17:27:40 | Patty            | Mujer  |    7.0 |    8.0 |   8.2 |    7.2 |     6.0 |   161.10 |
| 20/08/2024 17:27:42 | Mai              | Mujer  |    5.3 |    6.6 |   7.2 |    6.6 |     5.4 |       NA |
| 20/08/2024 17:28:14 | Deb              | Mujer  |    5.0 |    6.0 |   6.5 |    6.9 |     4.9 |   149.10 |
| 20/08/2024 17:28:29 | Luis Bautista    | Hombre |    6.1 |    7.3 |   7.9 |    7.4 |     5.8 |   173.73 |
| 20/08/2024 17:30:00 | Linette          | Mujer  |    6.2 |    6.5 |   7.6 |    6.8 |     5.7 |   162.60 |
| 20/08/2024 17:30:10 | Sebastian Bocio  | Hombre |    7.2 |    8.3 |   9.2 |    8.5 |     7.0 |   178.00 |
| 20/08/2024 17:33:34 | Isaura           | Mujer  |    5.6 |    7.1 |   8.1 |    7.4 |     6.2 |   162.00 |

3.  **Mandato**. Aplica la prueba t de Student para muestras
    independientes, comparando las medidas de los dedos entre géneros,
    para determinar si existe una diferencia significativa entre las
    medidas de los dedos de hombres y mujeres. Interpreta y/o resume los
    resultados con un pequeño párrafo que explique si las diferencias
    encontradas son estadísticamente significativas y qué implicaciones
    podrían tener en el contexto del estudio. **Importante**. Considera
    mirar, como referencia, los diagramas de caja de la práctica
    anterior [(PA01. Generación de la matriz de
    distancias)](https://biogeografia-master.github.io/matriz-de-distancias/README.html),
    y encuentra el diagrama correspondiente al dedo que te tocó
    analizar. Se supone que el diagrama de caja y la prueba estadística,
    deben ser consistentes entre sí (una prueba con resultado
    significativo debería ser consistente con un diagrama de caja con
    efecto). No obstante, ten presente que los diagramas de caja
    analizan los 14 participantes, mientras que tú solamente estás
    analizando seis elementos (tres hombres y tres mujeres).

Como comenté arriba, usarás muestras balanceadas, es decir, tres hombres
y tres mujeres para realizar la comparación. Ten en cuenta que, con
tamaños de muestras tan pequeños, el poder estadístico de la prueba es
limitado, y esto deberías destacarlo en tu redacción.

## Aplicación de la Prueba t de Student para Muestras Independientes

### Fórmula de la prueba t para muestras independientes

La prueba t de Student para muestras independientes se utiliza para
comparar las medias de dos grupos no relacionados. La fórmula es:

$$
t = \frac{\bar{X}_1 - \bar{X}_2}{\sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}}
$$

Donde:

- $\bar{X}_1$ y $\bar{X}_2$ son las medias de los dos grupos.
- $s_1^2$ y $s_2^2$ son las varianzas de los grupos.
- $n_1$ y $n_2$ son los tamaños de muestra de los dos grupos (en este
  caso, cada grupo tiene 3 elementos u observaciones).

La media de cada grupo se calcula utilizando la siguiente fórmula:

$$
\bar{X} = \frac{\sum_{i=1}^{n} X_i}{n}
$$

La varianza de cada grupo se calcula usando la siguiente fórmula:

$$
s^2 = \frac{\sum_{i=1}^{n}(X_i - \bar{X})^2}{n - 1}
$$

### Ejemplo Práctico

Supongamos que tenemos las siguientes medidas para un dedo específico
(e.g., índice):

| Género | Índice |
|:------:|:------:|
| Hombre |  7.4   |
| Hombre |  7.1   |
| Hombre |  7.3   |
| Mujer  |  6.8   |
| Mujer  |  6.7   |
| Mujer  |  6.9   |

1.  **Calcular las medias de los dos grupos $\bar{X}_1$ y $\bar{X}_2$:**

$$
\bar{X}_1 = \frac{7.4 + 7.1 + 7.3}{3} = \frac{21.8}{3} = 7.27
$$

$$
\bar{X}_2 = \frac{6.8 + 6.7 + 6.9}{3} = \frac{20.4}{3} = 6.8
$$

2.  **Calcular las varianzas de los dos grupos $s_1^2$ y $s_2^2$:**

Primero, calculamos las diferencias al cuadrado para cada grupo:

| Género | Medidas (cm) |  $(X_i - \bar{X}_1)^2$   | $(X_j - \bar{X}_2)^2$  |
|:------:|:------------:|:------------------------:|:----------------------:|
| Hombre |     7.4      | $(7.4 - 7.27)^2 = 0.017$ |                        |
| Hombre |     7.1      | $(7.1 - 7.27)^2 = 0.029$ |                        |
| Hombre |     7.3      | $(7.3 - 7.27)^2 = 0.001$ |                        |
| Mujer  |     6.8      |                          | $(6.8 - 6.8)^2 = 0.0$  |
| Mujer  |     6.7      |                          | $(6.7 - 6.8)^2 = 0.01$ |
| Mujer  |     6.9      |                          | $(6.9 - 6.8)^2 = 0.01$ |

Luego, calculamos las varianzas:

$$
s_1^2 = \frac{0.017 + 0.029 + 0.001}{3 - 1} = \frac{0.047}{2} = 0.0235
$$

$$
s_2^2 = \frac{0.0 + 0.01 + 0.01}{3 - 1} = \frac{0.02}{2} = 0.01
$$

3.  **Calcular la estadística t:**

$$
t = \frac{\bar{X}_1 - \bar{X}_2}{\sqrt{\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}}} = \frac{7.27 - 6.8}{\sqrt{\frac{0.0235}{3} + \frac{0.01}{3}}}
$$

$$
t = \frac{0.47}{\sqrt{0.00783 + 0.00333}} = \frac{0.47}{\sqrt{0.01116}} = \frac{0.47}{0.1056} \approx 4.45
$$

4.  **Grados de libertad (usando la corrección de Welch para muestras
    independientes con varianzas diferentes):**

$$
df = \frac{\left(\frac{s_1^2}{n_1} + \frac{s_2^2}{n_2}\right)^2}{\frac{\left(\frac{s_1^2}{n_1}\right)^2}{n_1-1} + \frac{\left(\frac{s_2^2}{n_2}\right)^2}{n_2-1}}
$$

Calculamos:

$$
df = \frac{\left(0.00783 + 0.00333\right)^2}{\frac{(0.00783)^2}{2} + \frac{(0.00333)^2}{2}} = \frac{0.01116^2}{\frac{6.1289e-5}{2} + \frac{1.1089e-5}{2}} \approx \frac{0.000124}{0.000035} \approx 3.54
$$

Aproximadamente, el número de grados de libertad es 3.

**Importante**. Si quieres simplificar tu ejercicio, no uses la
corrección de Welch. No obstante, si lo haces así, debes considerar que
tu prueba pierde algo de poder.

Si no usáramos la corrección de Welch y en su lugar asumiéramos
varianzas iguales entre las dos muestras, los grados de libertad (df) se
calcularían como la suma de los tamaños de las dos muestras menos 2. Es
decir:

$$
df = n_1 + n_2 - 2
$$

En el ejemplo:

- $n_1 = 3$ (tamaño de la muestra 1)
- $n_2 = 3$ (tamaño de la muestra 2)

Entonces, los grados de libertad serían:

$$
df = 3 + 3 - 2 = 6 - 2 = 4
$$

Por lo tanto, sin la corrección de Welch, los grados de libertad serían
4, y puedes usar este valor para simplificar tu ejercicio.

No obstante, en esta demostración, seguiremos adelante con los grados de
libertad caclulados por medio de la corrección de Welch, es decir,
usáremos 3 grados de libertad.

5.  **Determinación del valor crítico:**

Para un nivel de significancia de 0.05 y 3 grados de libertad, el valor
crítico de t en una prueba de dos colas es aproximadamente 3.182.

6.  **Conclusión:**

Dado que el valor calculado de t (4.45) es mayor que el valor crítico de
t (3.182), podemos rechazar la hipótesis nula. Esto significa que hay
una diferencia significativa entre las medidas de los dedos de hombres y
mujeres en este ejemplo.

Este cálculo demuestra que, a pesar del tamaño de muestra pequeño, se
detectó una diferencia significativa entre las dos muestras en este caso
particular. Sin embargo, con tamaños de muestra tan pequeños, los
resultados deben interpretarse con cautela, ya que el poder estadístico
es limitado.

### ¿Cómo se haría en R?

``` r
# Datos de ejemplo
hombres_medidas <- c(7.4, 7.1, 7.3)
mujeres_medidas <- c(6.8, 6.7, 6.9)

# Aplicar la prueba t para muestras independientes
t.test(hombres_medidas, mujeres_medidas, paired = FALSE)
```

## Referencias

Amrhein, V., Greenland, S., & McShane, B. (2019). Scientists rise up
against statistical significance. Nature, 567(7748), 305-307.

Frank O, Tam CM, Rhee J. Is it time to stop using statistical
significance? Aust Prescr. 2021 Feb;44(1):16-18. doi:
10.18773/austprescr.2020.074. Epub 2021 Feb 1. PMID: 33664545; PMCID:
PMC7900272.

Triola, M. F. (2012). Estadistica. España: Pearson Education.
