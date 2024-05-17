# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("c:/proyectos/mineriadatos/dm2024a/") # Establezco el Working Directory
nombre_archivo <- "K999_002.csv"

# cargo el dataset
# dataset <- as.data.table(readxl::read_excel('C:/proyectos/mineriadatos/TareasHogar/cl3_grids.xlsx'))
dataset <- fread(input = 'exp/HT2020/total_gs.txt')
dataset[,ganancia_promedio:=ganancia_promedio/1000000]
dataset[,V1:=NULL]

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "ganancia_promedio ~ .",
        data = dataset, # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 100, # minima cantidad de registros para que se haga el split
        minbucket = 20, # tamaño minimo de una hoja
        maxdepth = 4
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


