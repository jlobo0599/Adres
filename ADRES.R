
#Se instalan los paquetes necesarios para analizar la informacion
install.packages("DBI")
install.packages("RSQLite")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("RODBC")
install.packages("stringr")
install.packages("tidyverse")
install.packages("geojsonio")
install.packages("ggthemes")
install.packages("sf")
install.packages("ggspatial")
install.packages("patchwork")
install.packages("ggthemes")

#Se cargan los mismos
library(DBI)
library(RSQLite)
library(readxl)
library(dplyr)
library(ggplot2)
library(RODBC)
library(scales)
library(stringr)
library(tidyverse)
library(geojsonio)
library(ggthemes)
library(sf)
library(ggspatial)
library(patchwork)
library(ggthemes)

#Se importan los datos de Excel
municipios <- read_excel("C:/sqlite/Municipios/Municipios.xlsx")
prestadores <- read_excel("C:/sqlite/Prestadores/Prestadoress.xlsx")

#Se limpia la columna Departamento de la base municipios

for (valor in unique(municipios$Dep)) {
#Se encuentra el primer nombre de departamento antes de que se repita, sin simbolos % ni > ni
#< y se convierte a minusculas
primer_nombre <- tolower(str_replace_all(iconv(municipios$Departamento[which(municipios$Dep == valor)[1]], "UTF-8", "ASCII", sub = ""), "[^[:alnum:]]", ""))
#Se encuentran todos los índices donde Dep es igual al valor actual
indices <- which(municipios$Dep == valor)
#Se actualizan los nombres de departamento donde Dep es igual al valor actual, utilizando el primer nombre encontrado
municipios$Departamento[indices] <- primer_nombre}

#se eliminan los valores duplicados
nombres_existentes <- unique(municipios$Departamento)

#se crea un diccionario de correspondencia de nombres
diccionario_nombres <- c("antioquia" = "Antioquia",
                         "atlntico" = "Atlántico",
                         "bogotdc" = "Bogotá D.C",
                         "bolvar" = "Bolívar",
                         "boyac" = "Boyacá",
                         "caldas" = "Caldas",
                         "caquet" = "Caquetá",
                         "cauca" = "Cauca",
                         "cesar" = "Cesár",
                         "crdoba" = "Córdoba",
                         "cundinamarca" = "Cundinamarca",
                         "choc" = "Chocó",
                         "huila" = "Huila",
                         "laguajira" = "La Guajira",
                         "magdalena" = "Magdalena",
                         "meta" = "Meta",
                         "nario" = "Nariño",
                         "nortedesantander" = "Norte de Santander",
                         "quindo" = "Quindío",
                         "risaralda" = "Risaralda",
                         "santander" = "Santander",
                         "sucre" = "Sucre",
                         "tolima" = "Tolima",
                         "valledelcauca" ="Valle del Cauca",
                         "arauca" = "Arauca",
                         "casanare" = "Casanare",
                         "putumayo" = "Putumayo",
                         "sanandrs" = "San Andrés",
                         "amazonas" = "Amazonas",
                         "guaina" = "Guainía",
                         "guaviare" = "Guaviare",
                         "vaups" = "Vaupés",
                         "vichada" = "Vichada")

#se crea y ejecuta una funcion para corregir el nombre de los departamentos 
for (nombre_existente in nombres_existentes) {
  nuevo_nombre <- diccionario_nombres[nombre_existente]
  if (!is.null(nuevo_nombre)) {
    municipios$Departamento[municipios$Departamento == nombre_existente] <- nuevo_nombre
  }
}

#lo mismo sucede con la columna de municipios, se corrigen estos simbolos 
#que aparecen cuando se usan tildes

#se eliminan los simbolos innecesarios
municipios$Municipio <- gsub("[^[:alnum:]\\s]", "", municipios$Municipio)

#Se conecta a la base 
base <- dbConnect(RSQLite::SQLite(), "basededatos.sqlite")

# se guardan los datos en la base de datos SQLite
dbWriteTable(base, "municipios_sql", municipios, overwrite = TRUE)
dbWriteTable(base, "prestadores_sql", prestadores, overwrite = TRUE)

####################
#base de municipios#
####################

#queries

-------------------------
#Cantidad de municipios--
-------------------------  
  
#1 Cuantos municipios tiene cada region 
munxregion <- dbGetQuery(base,"SELECT region, COUNT(*) AS cantidad_municipios 
                       FROM municipios_sql GROUP BY region")


#2 Cual region tiene mas municipios
region_conmas_mun <- dbGetQuery(base,"SELECT region, COUNT(*) AS veces_repetidas
                              FROM municipios_sql
                              GROUP BY region
                              ORDER BY veces_repetidas DESC
                              LIMIT 1")

#3 Cual region tiene menos municipios
region_conmenos_mun <- dbGetQuery(base,"SELECT region, COUNT(*) AS veces_repetidas
                              FROM municipios_sql
                              GROUP BY region
                              ORDER BY veces_repetidas ASC
                              LIMIT 1")

#4 Cantidad de municipios por departamento
munxdpto <- dbGetQuery(base,"SELECT departamento, COUNT(*) AS veces_repetidas 
                       FROM municipios_sql GROUP BY departamento")

#5 Departamento con mayor cantidad de municipios
dpto_conmas_mun <- dbGetQuery(base,"SELECT departamento, COUNT(*) AS veces_repetidas
                              FROM municipios_sql
                              GROUP BY Departamento
                              ORDER BY veces_repetidas DESC
                              LIMIT 1")

#6 Departamento con menor cantidad de municipios 
dpto_conmenos_mun <- dbGetQuery(base,"SELECT departamento, COUNT(*) AS veces_repetidas
                              FROM municipios_sql
                              GROUP BY Departamento
                              ORDER BY veces_repetidas ASC
                              LIMIT 1")
------------
#Superficie-
------------
  
#1 Region mas extensa 
regionmas_extensa <- dbGetQuery(base,"SELECT region, SUM(superficie) AS superficie_total
  FROM municipios_sql
  GROUP BY region
  ORDER BY superficie_total DESC
  LIMIT 1")

#2 Region menos extensa 
regionmenos_extensa <- dbGetQuery(base,"SELECT region, SUM(superficie) AS superficie_total
  FROM municipios_sql
  GROUP BY region
  ORDER BY superficie_total ASC
  LIMIT 1")

#3 Promedio de superficie
promedio_superficie <- dbGetQuery(base,"  SELECT AVG(superficie) AS promedio_superficie
  FROM municipios_sql")

#4 Municipio mas extenso
munmas_extenso <- dbGetQuery(base,"SELECT departamento, municipio, superficie
  FROM municipios_sql
  ORDER BY superficie DESC
  LIMIT 1")

#5 Municipio menos extenso
munmenos_extenso <- dbGetQuery(base,"SELECT departamento, municipio, SUM(superficie) AS superficie_total
  FROM municipios_sql
  WHERE superficie > 0
  GROUP BY municipio
  ORDER BY superficie_total ASC
  LIMIT 1")

#6 Departamento mas extenso
dptomas_extenso <- dbGetQuery(base,"SELECT departamento, superficie
  FROM municipios_sql
  GROUP BY departamento 
  ORDER BY superficie DESC
  LIMIT 1")

#7 Departamento menos extenso
dptomenos_extenso <- dbGetQuery(base,"SELECT departamento, superficie
  FROM municipios_sql
  GROUP BY departamento 
  ORDER BY superficie ASC
  LIMIT 1")
  
-----------
#Poblacion-
-----------  

#1 Regiones agrupadas por la cantidad de poblacion  
poblacion_regional <- dbGetQuery(base,"SELECT region, SUM(poblacion) AS poblacion_total
FROM municipios_sql
GROUP BY region")

#2 Region mas poblada 
regionmas_poblada <- dbGetQuery(base,"SELECT region, SUM(poblacion) AS poblacion_total
FROM municipios_sql
GROUP BY region
ORDER BY poblacion_total DESC
LIMIT 1")

#3 Region menos poblada
regionmenos_poblada <- dbGetQuery(base,"SELECT region, SUM(poblacion) AS poblacion_total
FROM municipios_sql
GROUP BY region
ORDER BY poblacion_total ASC
LIMIT 1")

#4 Departamento mas poblado 
deptomas_poblado <- dbGetQuery(base,"SELECT departamento, SUM(poblacion) AS poblacion_total
FROM municipios_sql
GROUP BY departamento
ORDER BY poblacion_total DESC
LIMIT 1")  
  
#5 Departamento menos poblado 
deptomenos_poblado <- dbGetQuery(base,"SELECT departamento, SUM(poblacion) AS poblacion_total
FROM municipios_sql
GROUP BY departamento
ORDER BY poblacion_total ASC
LIMIT 1") 

#6 Municipio menos poblado 
munmenos_poblado <- dbGetQuery(base,"SELECT departamento, municipio, SUM(poblacion) AS poblacion_total
FROM municipios_sql
GROUP BY municipio
ORDER BY poblacion_total ASC
LIMIT 1") 

#7 Grafico de categorizacion por municipio por la poblacion  

#esta se realiza, según la ley 136 de 1994, la cual indica
#que hay 7 categorias para municipios segun la poblacion. Estas son:
#la especial, en la cual habitan mas de 500.001 personas
#la primera, en la cual habitan de 100.001 - 500.000 personas
#la segunda, en la cual habitan de 50.001 - 100.000 personas
#la tercera, en la cual habitan mas de 30.001 - 50.000 personas
#la cuarta, en la cual habitan mas de 20.001 - 30.000 personas
#la quinta, en la cual habitan mas de 10.001 - 20.000 personas
#la sexta, en la cual habitan menos de 10.000 personas

#7.1 Se crean las categorias
cat_especial <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE Poblacion >= 500001 ")
cat_1 <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE(Poblacion >=100001 AND Poblacion <= 500000)")
cat_2 <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE(Poblacion >=50001 AND Poblacion <= 100000)")
cat_3 <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE(Poblacion >=30001 AND Poblacion <= 50000)")
cat_4 <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE(Poblacion >=20001 AND Poblacion <= 30000)")
cat_5 <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE(Poblacion >=10001 AND Poblacion <= 20000)")
cat_6 <- dbGetQuery(base, "SELECT Municipio, Poblacion FROM municipios_sql WHERE Poblacion <= 10000")

#7.2 Se cuenta el numero de municipios en cada categoria
categorias <- c(
  nrow(cat_especial),
  nrow(cat_1),
  nrow(cat_2),
  nrow(cat_3),
  nrow(cat_4),
  nrow(cat_5),
  nrow(cat_6)
)

#7.3 Se asignan las etiquetas
etiquetas_categoriasmun <- c(
  "Especial",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6"
)

#7.4 Se realiza el grafico
barplot(categorias, names.arg = etiquetas_categoriasmun, col = "green", las = 1, xlab = "Categorías de población", ylab = "Número de municipios", main = "Distribución de municipios por población")

----------------------
#Densidad poblacional- 
----------------------
  
#1 Densidad por departamento para saber la  cantidad de personas que habitan
#por kilometro cuadrado en cada departamento 
densidad_depto <- dbGetQuery(base, "SELECT Departamento, SUM(Poblacion)/SUM(Superficie) AS Densidad_poblacional FROM municipios_sql GROUP BY Departamento")

#2 Departamento con mayor densidad poblacional
mayor_densdepto <- dbGetQuery(base, "SELECT Departamento, 
                              SUM(Poblacion)/SUM(Superficie) AS 
                              Densidad_poblacional 
                              FROM municipios_sql 
                              GROUP BY Departamento
                              ORDER BY Densidad_poblacional DESC
                              LIMIT 1")

#3 Departamento con menor densidad poblacional
menor_densdepto <- dbGetQuery(base, "SELECT Departamento, 
                              SUM(Poblacion)/SUM(Superficie) AS 
                              Densidad_poblacional 
                              FROM municipios_sql 
                              GROUP BY Departamento
                              ORDER BY Densidad_poblacional ASC
                              LIMIT 1")

#4 Municipio con menor densidad poblacional
menor_densmun <- dbGetQuery(base, "SELECT municipio, 
                              SUM(Poblacion)/SUM(Superficie) AS 
                              Densidad_poblacional 
                              FROM municipios_sql 
                              GROUP BY municipio
                              ORDER BY Densidad_poblacional ASC
                              LIMIT 1")

-----------
#Ruralidad-
-----------  
#1 Ruralidad por regiones
ruralidad_regional <- dbGetQuery(base,"SELECT Region, Irural FROM municipios_sql
                              GROUP BY Region")

#1.1 Grafico de ruralidad regional
barplot(ruralidad_regional$Irural, names.arg = ruralidad_regional$Region, 
        main = "Porcentaje de ruralidad por regiones", xlab = "Region", ylab = "Porcentaje de ruralidad") 

#2 Region mas rural
regionmas_rural <- dbGetQuery(base,"SELECT Region, Irural FROM municipios_sql
                              GROUP BY Region
                              ORDER BY Irural DESC LIMIT 1")

#3 Region menos rural
regionmenos_rural <- dbGetQuery(base,"SELECT Region, Irural FROM municipios_sql
                              GROUP BY Region
                              ORDER BY Irural ASC LIMIT 1")

#4 Ruralidad departamental
ruralidad_deptal <- dbGetQuery(base, "SELECT Departamento, Irural  FROM municipios_sql GROUP BY Departamento")

#4.1 Grafico de ruralidad departamental
barplot(ruralidad_deptal$Irural, names.arg = ruralidad_deptal$Departamento, 
        main = "Porcentaje de ruralidad por departamento", xlab = "Departamento", ylab = "Porcentaje de ruralidad") 

#5 Departamento mas rural 
dptomas_rural <- dbGetQuery(base,"SELECT Departamento,municipio, Irural FROM municipios_sql
                              GROUP BY Departamento
                              ORDER BY Irural DESC  LIMIT 1")

#6 Departamento menos rural 
dptomenos_rural <- dbGetQuery(base,"SELECT Departamento, municipio, Irural FROM municipios_sql
                              GROUP BY Departamento
                              ORDER BY Irural ASC  LIMIT 1")

#7 Grafico de barras de la ruralidad por municipio

#7.1 Categorizacion de los municipios rurales y urbanos
municipios_rurales <- dbGetQuery(base, "SELECT Municipio, Irural FROM municipios_sql WHERE Irural == 100 ")
municipios_76a99 <- dbGetQuery(base, "SELECT Municipio, Irural FROM municipios_sql WHERE (Irural >=76 AND Irural < 100)")
municipios_51a75 <- dbGetQuery(base, "SELECT Municipio, Irural FROM municipios_sql WHERE (Irural >=51 AND Irural <= 75)")
municipios_26a50 <- dbGetQuery(base, "SELECT Municipio, Irural FROM municipios_sql WHERE (Irural >=26 AND Irural <= 50)")
municipios_1a25 <- dbGetQuery(base, "SELECT Municipio, Irural FROM municipios_sql WHERE (Irural >=1 AND Irural <= 25)")
municipios_urbanos <- dbGetQuery(base, "SELECT Municipio, Irural FROM municipios_sql WHERE Irural == 0 ")

#7.2  Conteo del número de municipios en cada categoría
conteo <- c(
  nrow(municipios_rurales),
  nrow(municipios_76a99),
  nrow(municipios_51a75),
  nrow(municipios_26a50),
  nrow(municipios_1a25),
  nrow(municipios_urbanos))

#7.3 se agregan las etiquetas para el grafico 
etiquetas <- c(
  "Rurales",
  "76-99",
  "51-75",
  "26-50",
  "1-25",
  "Urbanos")

# 7.4 se crea el gráfico de barras
barplot(conteo, names.arg = etiquetas, col = "skyblue", xlab = "Categorías de ruralidad", ylab = "Número de municipios", main = "Distribución de municipios por índice de ruralidad")



-----------------------------#base de prestadores#------------------------------

#1 Cantidad de prestadores de servicio por municipio, especificando las publicas y las privadas
prestadorespor_mpio <- dbGetQuery(base,"SELECT muni_nombre,depa_nombre, 
                         COUNT(*) AS num_registros,
                         SUM(CASE WHEN naju_codigo = 1 THEN 1 ELSE 0 END) AS num_privados,
                         SUM(CASE WHEN naju_codigo = 4 THEN 1 ELSE 0 END) AS num_publicos
                         FROM prestadores_sql 
                         GROUP BY muni_nombre")


#2 Cantidad de prestadores de servicio naturales y juridicos que son publicos y privados
natjur_pubpriv <- dbGetQuery(base,"SELECT clase_persona,
 SUM(CASE WHEN naju_codigo = '4' THEN 1 ELSE 0 END) AS publicos,
    SUM(CASE WHEN naju_codigo = '1' THEN 1 ELSE 0 END) AS privados
FROM prestadores_sql 
GROUP BY clase_persona")

#3 Cantidad de prestadores de servicios de salud por cada una de las cuatro clasificaciones
clpr_veces <- dbGetQuery(base,"SELECT clpr_codigo, clpr_nombre,
                         COUNT(*) AS cantidad_repeticiones,
                         SUM(CASE WHEN naju_codigo = '4' THEN 1 ELSE 0 END)
                         AS publicos,
    SUM(CASE WHEN naju_codigo = '1' THEN 1 ELSE 0 END) AS privados
                         FROM prestadores_sql GROUP BY clpr_codigo")



#Orden de los prestadores por fecha de radicacion de manera descendente
radicacion_descend <- dbGetQuery(base,"SELECT * FROM prestadores_sql ORDER BY fecha_radicacion DESC")

#Cuantas y cuales son las categorias de caracter
cat_caracter <- dbGetQuery(base,"SELECT caracter, COUNT(*) as cantidad FROM prestadores_sql GROUP BY caracter")


#################
#    MAPAS      #
#################

#1. Mapa de la cantidad de poblacion por municipios

#Se cargan las bases de datos
# el archivo shapefile geojson es descargado de
#https://github.com/caticoa3/colombia_mapa/blob/master/co_2018_MGN_MPIO_POLITICO.geojson

#Se llama el archivo
colombia <- read_sf("C:/sqlite/Mapas/co_2018_MGN_MPIO_POLITICO.geojson")

#Se conecta a SQLite
poblacion <- dbGetQuery(base,"select * from municipios_sql")

#Se filtra poblacion
poblacion <- poblacion[, c(4, 6)]

#Se cambia el nombre de los municipios
colnames(colombia)[6] <-  colnames(poblacion)[1]

#Se combina la base de datos 
colombia <- merge(colombia, poblacion, by = "Depmun")

#Se crea la categoria para el mapa
colombia <- colombia %>%
  mutate(categoria = case_when(
    Poblacion > 500000 ~ "Especial",
    Poblacion >= 100001 & Poblacion <= 500000 ~ "Primera",
    Poblacion >= 50001 & Poblacion <= 100000 ~ "Segunda",
    Poblacion >= 30001 & Poblacion <= 50000 ~ "Tercera",
    Poblacion >= 20001 & Poblacion <= 30000 ~ "Cuarta",
    Poblacion >= 10001 & Poblacion <= 20000 ~ "Quinta",
    Poblacion <= 10000 ~ "Sexta"
  ))  

#Se ordena por categoria
colombia$categoria <- factor(colombia$categoria, 
                             levels = c("Primera", "Segunda", 
                                        "Tercera", "Cuarta",
                                        "Quinta", "Sexta", 
                                        "Especial"), 
                             labels = c("Primera: De 100.001 a 500.000 personas",
                                        "Segunda: De 50.001 a 100.000 personas",
                                        "Tercera: De 30.001 a 50.000 personas",
                                        "Cuarta: De 20.001 a 30.000 personas",
                                        "Quinta: De 10.001 a 20.000 personas",
                                        "Sexta: Menos de 10.000 personas",
                                        "Especial: Más de 500.000 personas"))

#Se hace el mapa de Colombia
 ggplot(colombia, aes(fill = colombia$categoria)) + geom_sf()  + coord_sf() +
  theme_minimal()  +
  labs(fill = "Categoria de municipios ", 
       title = "Población por municipio") +
  scale_fill_tableau() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = c(-0.20, 0.45),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         height = unit(2.5, "cm"), width = unit(2.0, "cm"),
                         style = north_arrow_fancy_orienteering)



#2. Mapa de la cantidad de prestadores de servicios de salud por municipio

#se cargan las bases   
colombia2 <- read_sf("C:/sqlite/Mapas/Archivos/co_2018_MGN_MPIO_POLITICO.geojson")

#se genera la query para obtener los datos de SQLite
prestadores <- dbGetQuery(base, "SELECT depa_nombre, muni_nombre, codigo_habilitacion
                          FROM Prestadores_sql")

#Se substrae de la letra 1 a la 5 del codigo completo
prestadores$MPIO_CCNCT <- substr(prestadores$codigo_habilitacion, 1, 5)

#Se crea con table una base de datos que contenga el numero de centro prestadores 
#de salud por municipio
info <- table(prestadores$muni_nombre) |> as.data.frame()

-#Se le cambia el nombre de las columnas de la base creada
colnames(info) <- c("muni_nombre", "cantidad")

#Se unen ambas bases de datos
info <- merge(info, prestadores[, c(2, 4)], by = "muni_nombre")

#Se eliminan los valores duplicados
info <- unique(info)

#Se crean los municipios faltantes
faltantes <- colombia2$MPIO_CCNCT[!(colombia2$MPIO_CCNCT %in% info$MPIO_CCNCT)]

#Se extraen los municipios faltantes
faltantes <- colombia2[colombia2$MPIO_CCNCT %in% faltantes ,]

#Se extrae el nombre de los municipios faltantes
info2 <- data.frame(muni_nombre = faltantes$MPIO_CNMBR, 
                    cantidad = rep(0, nrow(faltantes)),
                    MPIO_CCNCT = faltantes$MPIO_CCNCT)

#Se unen ambos data.frame de info
info <- rbind(info, info2)

#Se incluye la informacion de la cantidad de prestadores de servicios de salud
colombia2 <- merge(colombia2, info, by = "MPIO_CCNCT")

#Se crean las categorias
colombia2 <- colombia2 %>%
  mutate(categoria = case_when(
    cantidad <= 10 ~ "Menor o igual a 10",
    cantidad > 10 & cantidad <= 100 ~ "entre 11 y 100",
    cantidad > 100 & cantidad <= 1000 ~ "entre 101 y 1.000",
    cantidad  > 1000 & cantidad <= 5000  ~ "entre 1.001 y 5.000",
    cantidad > 5000 & cantidad <= 10000  ~ "entre 5.001 y 10.000",
    cantidad > 10000  ~ "Mayor a 10.000"
  ))  

#se ordenan las categorias
colombia2$categoria <- factor(colombia2$categoria, 
                              levels = c("Menor o igual a 10", 
                                         "entre 11 y 100",
                                         "entre 101 y 1.000",
                                         "entre 1.001 y 5.000",
                                         "entre 5.001 y 10.000",
                                         "Mayor a 10.000"))



# Se crea el mapa 
ggplot(colombia2, aes(fill = colombia2$categoria)) + geom_sf(col = "black")  + coord_sf() +
  theme_minimal()  +
  labs(fill = "Numero de prestadores", 
       title = "Prestadores de servicios de salud por municipio") +
  scale_fill_tableau() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = c(0.05, 0.2),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         height = unit(2.5, "cm"), width = unit(2.0, "cm"),
                         style = north_arrow_fancy_orienteering)

