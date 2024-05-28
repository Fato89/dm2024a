# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol utilizando un subset distinto de atributos del dataset

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

# parmatros experimento
PARAM <- list()
PARAM$experimento <- "3510b"

loop_parms <- list(
  c(minsplit=50,minbucket=5,maxdepth=14)
 # c(minsplit=100,minbucket=5,maxdepth=10),
 # c(minsplit=100,minbucket=50,maxdepth=6),
 # c(minsplit=250,minbucket=50,maxdepth=14),
 # c(minsplit=500,minbucket=10,maxdepth=8),
 # c(minsplit=500,minbucket=100,maxdepth=6),
 # c(minsplit=1000,minbucket=20,maxdepth=10),
 # c(minsplit=1000,minbucket=200,maxdepth=8),
 # c(minsplit=2000,minbucket=20,maxdepth=14),
 # c(minsplit=2000,minbucket=200,maxdepth=10)
)

purrr::map(
  loop_parms,
  ~{

# parameetros rpart
PARAM$rpart_param <- list(
  "cp" = -1,
  "minsplit" = .x["minsplit"],
  "minbucket" = .x["minbucket"],
  "maxdepth" = .x["maxdepth"]
)

# parametros  arbol
# entreno cada arbol con solo 50% de las variables variables
PARAM$feature_fraction <- 0.1
# voy a generar 500 arboles,
#  a mas arboles mas tiempo de proceso y MEJOR MODELO,
#  pero ganancias marginales
PARAM$num_trees_max <- 200

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

#cargo MI semilla, que esta en MI bucket
tabla_semillas <- fread("~/datasets//mis_semillas.txt" )
ksemilla_azar <- tabla_semillas[ 1, semilla ]  # 1 es mi primer semilla

# cargo los datos
dataset <- fread("~/datasets/dataset_pequeno.csv")


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
carpeta_experimento <- paste0("./exp/KA", PARAM$experimento, "/")
dir.create(paste0("./exp/KA", PARAM$experimento, "/"),
  showWarnings = FALSE
)

setwd(carpeta_experimento)


# que tamanos de ensemble grabo a disco, pero siempre debo generar los 500
grabar <- c(1, 5, 10, 50, 100, 200, 500)


# defino los dataset de entrenamiento y aplicacion
dtrain <- dataset[foto_mes == 202107]
dapply <- dataset[foto_mes == 202109]

# arreglo clase_ternaria por algun distraido ""
dapply[, clase_ternaria := NA ]

# elimino lo que ya no utilizo
rm(dataset)
gc()

# aqui se va acumulando la probabilidad del ensemble
dapply[, prob_acumulada := 0]

# Establezco cuales son los campos que puedo usar para la prediccion
# el copy() es por la Lazy Evaluation
campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))



# Genero las salidas
set.seed(ksemilla_azar) # Establezco la semilla aleatoria

for (arbolito in 1:PARAM$num_trees_max) {
  qty_campos_a_utilizar <- as.integer(length(campos_buenos)
  * PARAM$feature_fraction)

  campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

  # paso de un vector a un string con los elementos
  # separados por un signo de "+"
  # este hace falta para la formula
  campos_random <- paste(campos_random, collapse = " + ")

  # armo la formula para rpart
  formulita <- paste0("clase_ternaria ~ ", campos_random)

  # genero el arbol de decision
  modelo <- rpart(formulita,
    data = dtrain,
    xval = 0,
    control = PARAM$rpart_param
  )

  # aplico el modelo a los datos que no tienen clase
  prediccion <- predict(modelo, dapply, type = "prob")

  dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]

  if (arbolito %in% grabar) {
    # Genero la entrega para Kaggle
    umbral_corte <- (1 / 40) * arbolito
    entrega <- as.data.table(list(
      "numero_de_cliente" = dapply[, numero_de_cliente],
      "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
    )) # genero la salida

    nom_arch <- paste0(
      "KA", PARAM$experimento, "_",
      sprintf("%.3d", arbolito), # para que tenga ceros adelante
      "_",
      paste(.x["minsplit"],.x["minbucket"],.x["maxdepth"],sep="_"),
      ".csv"
    )
    fwrite(entrega,
      file = nom_arch,
      sep = ","
    )

    cat(arbolito, " ")
  }
}
  } 
)