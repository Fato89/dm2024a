# Experimentos Colaborativos Default
# Workflow  Training Strategy

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

#cargo la libreria
# args <- c( "~/dm2024a" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "z571_TS_training_strategy.r  START\n" )
action_inicializar() 

envg$PARAM$train$semilla <- envg$PARAM$semilla

  
# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

cat( "ordeno_dataset\n")
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

archivos_salida <- c()

if( "future" %in%  names( envg$PARAM ) )
{
  cat( "inicio grabar future\n")
  # grabo los datos del futuro
  cat( "Iniciando grabado de dataset_future.csv.gz\n" )
  fwrite(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, ],
    file = "dataset_future.csv.gz",
    logical01 = TRUE,
    sep = ","
  )
  cat( "Finalizado grabado de dataset_future.csv.gz\n" )

  # grabo los datos donde voy a entrenar los Final Models
  cat( "Iniciando grabado de dataset_train_final.csv.gz\n" )
  fwrite(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, ],
    file = "dataset_train_final.csv.gz",
    logical01 = TRUE,
    sep = ","
  )
  cat( "Finalizado grabado de dataset_train_final.csv.gz\n" )

  archivos_salida <- c( archivos_salida, "dataset_future.csv.gz", "dataset_train_final.csv.gz" )
}


if( "train" %in%  names( envg$PARAM ) )
{
  cat( "inicio grabar train\n")
  # grabo los datos donde voy a hacer la optimizacion de hiperparametros
  set.seed(envg$PARAM$train$semilla, kind = "L'Ecuyer-CMRG")
  dataset[
    get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training,
    azar := runif(nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training]))
  ]

  dataset[, fold_train := 0L]
  dataset[
    get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$training &
      (azar <= envg$PARAM$train$undersampling |
        get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$clase_minoritaria ),
    fold_train := 1L
  ]

  dataset[, fold_validate := 0L]
  dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$validation, fold_validate := 1L]

  dataset[, fold_test := 0L]
  dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$train$testing, fold_test := 1L]

  cat( "Iniciando grabado de dataset_training.csv.gz\n" )
  fwrite(dataset[fold_train + fold_validate + fold_test >= 1, ],
    file = "dataset_training.csv.gz",
    logical01 = TRUE,
    sep = ","
  )
  cat( "Finalizado grabado de dataset_training.csv.gz\n" )

  archivos_salida <- c( archivos_salida, "dataset_training.csv.gz" )
}


#------------------------------------------------------------------------------
# copia la metadata sin modificar
cat( "grabar metadata\n")

write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )


#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(is.na(x))
    }
  ),
  "ceros" = sapply(
    dataset[fold_train + fold_validate + fold_test >= 1, ],
    function(x) {
      sum(x == 0, na.rm = TRUE)
    }
  )
))

fwrite(tb_campos,
  file = "dataset_training.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset_train$ncol <- ncol(dataset[fold_train > 0, ])
envg$OUTPUT$dataset_train$nrow <- nrow(dataset[fold_train > 0, ])
envg$OUTPUT$dataset_train$periodos <- dataset[fold_train > 0, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_validate$ncol <- ncol(dataset[fold_validate > 0, ])
envg$OUTPUT$dataset_validate$nrow <- nrow(dataset[fold_validate > 0, ])
envg$OUTPUT$dataset_validate$periodos <- dataset[fold_validate > 0, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_test$ncol <- ncol(dataset[fold_test > 0, ])
envg$OUTPUT$dataset_test$nrow <- nrow(dataset[fold_test > 0, ])
envg$OUTPUT$dataset_test$periodos <- dataset[fold_test > 0, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_future$ncol <- ncol(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, ])
envg$OUTPUT$dataset_future$nrow <- nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, ])
envg$OUTPUT$dataset_future$periodos <- dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$future, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$dataset_finaltrain$ncol <- ncol(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, ])
envg$OUTPUT$dataset_finaltrain$nrow <- nrow(dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, ])
envg$OUTPUT$dataset_finaltrain$periodos <- dataset[get(envg$PARAM$dataset_metadata$periodo) %in% envg$PARAM$final_train, length(unique(get(envg$PARAM$dataset_metadata$periodo)))]

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = archivos_salida) 
cat( "z571_TS_training_strategy.r  END\n" )
