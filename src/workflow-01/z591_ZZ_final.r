# Experimentos Colaborativos Default

# Workflow  ZZ proceso final con semillas
# Este nuevo script fue desarrolado para ayudar a los alumnos en la realizacion
#  de los experimentos colaborativos
#  y las tareas habituales de comparar el resultado de varias semillas
#  en datos para los que se posee la clase

# SIEMPRE el final train se realiza SIN undersampling

# Acepta un vector de semillas y genera un modelo para cada semilla
# Acepta un vector de modelos_rank; se le puede especificar cuales modelos
#  de la Bayesian Optimzation se quiere
#    modelos_rank es la posición en el ranking, por ganancia descendente
#    ( Atencion,NO es el campo iteracion_bayesiana )
#    por ejemplo, si  PARAM$modelos_rank  <- c( 1 )
# En el caso que el dataset de future posea el campo clase_ternaria con todos los valores,
#  entonces solo genera el mejor modelo
#  se generan los graficos para las semillas en color gris y el promedio en color rojo
# en el caso que se posea clase_completa con sus valores el dataset future,
#  NO se generan los archivos para Kaggle
# en el caso de estar incompleta la clase_ternaria,  se generan los archicos para Kaggle



# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")
require("primes")

require("lightgbm")

#cargo la libreria
# args <- c( "~/dm2024a" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------

carpeta_actual <- function()
{
  st <- getwd()

  largo <- nchar(st)
  i <- largo
  while( i >= 1 & substr( st, i, i) == "/" )  i <- i - 1   # quito ultimo
  largo <- i

  while( i >= 1 & substr( st, i, i) != "/" & substr( st, i, i) != "\\"  )  i <- i - 1

  if( substr(st, i, i) == "/" ) i <- i + 1

  res <- ""
  if( i <= largo )  res <- substr( st, i, largo )

  return( res )
}
#------------------------------------------------------------------------------
# grabo la importancia de variables

grabar_importancia <- function(modelo_final, modelo_rank, iteracion_bayesiana) {
  tb_importancia <- as.data.table(lgb.importance(modelo_final))
  fwrite(tb_importancia,
    file = paste0(
      "impo_",
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      ".txt"
    ),
    sep = "\t"
  )

  rm(tb_importancia)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
cat( "z591_ZZ_final.r  START\n")
action_inicializar() 

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla)
# me quedo con PARAM$semillerio  primos al azar
envg$PARAM$semillas <- sample(primos)[1:envg$PARAM$qsemillas]


GrabarOutput()

# leo la salida de la optimizacion bayesiana
# En PARAM$input[1]  tango el nombre del experimento de Hyperparameter Tuning
arch_log <- paste0( "./", envg$PARAM$input[1], "/BO_log.txt")
action_verificar_archivo( arch_log )
tb_log <- fread(arch_log)
setorderv(tb_log, "metrica", envg$PARAM$metrica_order)


# leo el dataset donde voy a entrenar el modelo final
# En PARAM$input[2]  tango el nombre del experimento de TS Training Strategy
arch_dataset <- paste0("./", envg$PARAM$input[2], "/dataset_train_final.csv.gz")
cat( "lectura dataset_train_final.csv.gz\n")
action_verificar_archivo( arch_dataset )
dataset <- fread(arch_dataset)
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[2], "/dataset_metadata.yml" ) )

envg$PARAM$experimento <- carpeta_actual()  # nombre del experimento donde estoy parado

# leo el dataset donde voy a aplicar el modelo final
arch_future <- paste0("./", envg$PARAM$input[2], "/dataset_future.csv.gz")
cat( "lectura dataset_future.csv.gz\n")
dfuture <- fread(arch_future)

# logical que me indica si los datos de future tienen la clase con valores,
# y NO va para Kaggle
future_con_clase <- dfuture[get(envg$PARAM$dataset_metadata$clase) == "" | is.na(get(envg$PARAM$dataset_metadata$clase)), .N] == 0

# defino la clase binaria
dataset[, clase01 := ifelse(get(envg$PARAM$dataset_metadata$clase) %in%  envg$PARAM$train$clase01_valor1, 1, 0)]

campos_buenos <- setdiff(colnames(dataset), c(envg$PARAM$dataset_metadata$clase, "clase01"))


dir.create("modelitos", showWarnings = FALSE)

# genero un modelo para cada uno de las modelos_qty MEJORES
# iteraciones de la Bayesian Optimization
vganancias_suavizadas <- c()

imodelo <- 0L
for (modelo_rank in envg$PARAM$modelos_rank) {
  imodelo <- imodelo + 1L
  cat("\nmodelo_rank: ", modelo_rank, ", semillas: ")
  envg$OUTPUT$status$modelo_rank <- modelo_rank

  parametros <- as.list(copy(tb_log[modelo_rank]))
  iteracion_bayesiana <- parametros$iteracion_bayesiana


  # creo CADA VEZ el dataset de lightgbm
  cat( "creo lgb.Dataset\n")
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01],
    weight = dataset[, ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 1.0000001, 1.0)],
    free_raw_data = FALSE
  )

  ganancia <- parametros$ganancia

  # elimino los parametros que no son de lightgbm
  parametros$experimento <- NULL
  parametros$cols <- NULL
  parametros$rows <- NULL
  parametros$fecha <- NULL
  parametros$estimulos <- NULL
  parametros$ganancia <- NULL
  parametros$iteracion_bayesiana <- NULL

  #  parametros$num_iterations  <- 10  # esta linea es solo para pruebas

  if (future_con_clase) {
    tb_ganancias <-
      as.data.table(list("envios" = 1:envg$PARAM$graficar$envios_hasta))

    tb_ganancias[, gan_sum := 0.0]
  }

  sem <- 0L

  for (vsemilla in envg$PARAM$semillas)
  {
    sem <- sem + 1L
    cat(sem, " ")
    envg$OUTPUT$status$sem <- sem
    GrabarOutput()

    # Utilizo la semilla definida en este script
    parametros$seed <- vsemilla

    nombre_raiz <- paste0(
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      "_s",
      parametros$seed
    )

    arch_modelo <- paste0(
      "./modelitos/modelo_",
      "modelo_",
      nombre_raiz,
      ".model"
    )

    # genero el modelo entrenando en los datos finales
    if( !file.exists( arch_modelo ) )
    {
      cat( "\nentrenando modelo = ", sem, "  ." )
      set.seed(parametros$seed, kind = "L'Ecuyer-CMRG")
      modelo_final <- lightgbm(
        data = dtrain,
        param = parametros,
        verbose = -100
      )
      cat( " ...Fin." )

      # grabo el modelo, achivo .model
      lgb.save(modelo_final,
        file = arch_modelo
      )

      # creo y grabo la importancia de variables, solo para la primer semilla
      if (sem == 1) {
        cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
            file = "z-Rcanresume.txt",
            append = TRUE
        )

        grabar_importancia(modelo_final, modelo_rank, iteracion_bayesiana)
      }
    } else {
       modelo_final <- lgb.load(filename = arch_modelo)
    }

    # genero la prediccion, Scoring
    cat( "creo predict\n")
    prediccion <- predict(
      modelo_final,
      data.matrix(dfuture[, campos_buenos, with = FALSE])
    )

    tb_prediccion <-
      dfuture[, c( envg$PARAM$dataset_metadata$primarykey, envg$PARAM$dataset_metadata$clase), with=FALSE]

    tb_prediccion[, prob := prediccion]


    nom_pred <- paste0(
      "pred_",
      nombre_raiz,
      ".csv"
    )

   cat( "write prediccion\n")
    fwrite(
      tb_prediccion[, c( envg$PARAM$dataset_metadata$primarykey, "prob", envg$PARAM$dataset_metadata$clase), with=FALSE],
      file = nom_pred,
      sep = "\t"
    )




    setorder(tb_prediccion, -prob)

    if (!future_con_clase) {
      # genero los archivos para Kaggle

      cortes <- seq(
        from = envg$PARAM$kaggle$envios_desde,
        to = envg$PARAM$kaggle$envios_hasta,
        by = envg$PARAM$kaggle$envios_salto
      )

      for (corte in cortes)
      {
        tb_prediccion[, Predicted := 0L]
        tb_prediccion[1:corte, Predicted := 1L]

        nom_submit <- paste0(
          envg$PARAM$experimento,
          "_",
          nombre_raiz,
          "_",
          sprintf("%05d", corte),
          ".csv"
        )

        cat( "write prediccion Kaggle\n")
        cat( "Columnas del dataset: ",  colnames(tb_prediccion), "\n" )
        fwrite(tb_prediccion[, c(envg$PARAM$dataset_metadata$entity_id, "Predicted"), with=FALSE],
          file = nom_submit,
          sep = ","
        )
        cat( "written prediccion Kaggle\n")

        # hago el submit
        submitear <- TRUE
        if( "rango_submit"  %in%  names(envg$PARAM$kaggle) )
        {
          if( !(sem %in% envg$PARAM$kaggle$rango_submit) ) submitear <- FALSE
        }

        if( "competition" %in% names(envg$PARAM$kaggle) & submitear)
        {
          l1 <- "#!/bin/bash \n"
          l2 <- "source ~/.venv/bin/activate  \n"
          l3 <- paste0( "kaggle competitions submit -c ", envg$PARAM$kaggle$competition)
          l3 <- paste0( l3, " -f ", nom_submit )
          l3 <- paste0( l3,  " -m ",  "\"", carpeta_actual(),  " , ",  nom_submit , "\"",  "\n")
          l4 <- "deactivate \n"
          
          cat( paste0( l1, l2, l3, l4 ) , file = "subir.sh" )
          Sys.chmod( "subir.sh", mode = "744", use_umask = TRUE)

          system( "./subir.sh" )
        }
      }
    }

    if (future_con_clase) {
      tb_prediccion[, ganancia_acum :=
        cumsum(ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, envg$PARAM$train$gan1, envg$PARAM$train$gan0))]

      tb_ganancias[, paste0("g", sem) :=
        tb_prediccion[1:envg$PARAM$graficar$envios_hasta, ganancia_acum]]

      tb_ganancias[, gan_sum :=
        gan_sum + tb_prediccion[1:envg$PARAM$graficar$envios_hasta, ganancia_acum]]
    }

    # borro y limpio la memoria para la vuelta siguiente del for
    rm(tb_prediccion)
    rm(tb_importancia)
    rm(modelo_final)
    gc()
  }


  if (future_con_clase) {
    qsemillas <- length(envg$PARAM$semillas)
    tb_ganancias[, gan_sum := gan_sum / qsemillas]

    # calculo la mayor ganancia  SUAVIZADA
    tb_ganancias[, gan_suavizada := frollmean(
      x = gan_sum,
      n = envg$PARAM$graficar$ventana_suavizado,
      align = "center",
      na.rm = TRUE,
      hasNA = TRUE
    )]

    ganancia_suavizada_max <- tb_ganancias[, max(gan_suavizada, na.rm = TRUE)]

    vganancias_suavizadas <- c(vganancias_suavizadas, ganancia_suavizada_max)

    ymax <- max(tb_ganancias, na.rm = TRUE)

    campos_ganancias <- setdiff(colnames(tb_ganancias), "envios")
    ymin <- min(tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, campos_ganancias, with=FALSE ],
      na.rm = TRUE
    )

    arch_grafico <- paste0(
      "modelo_",
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      ".pdf"
    )

    pdf(arch_grafico)

    plot(
      x = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, envios],
      y = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, g1],
      type = "l",
      col = "gray",
      xlim = c(envg$PARAM$graficar$envios_desde, envg$PARAM$graficar$envios_hasta),
      ylim = c(ymin, ymax),
      main = paste0("Mejor gan prom = ", as.integer(ganancia_suavizada_max)),
      xlab = "Envios",
      ylab = "Ganancia",
      panel.first = grid()
    )

    # las siguientes curvas
    if (qsemillas > 1) {
      for (s in 2:qsemillas)
      {
        lines(
          x = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, envios],
          y = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, get(paste0("g", s))],
          col = "gray"
        )
      }
    }

    # finalmente la curva promedio
    lines(
      x = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, envios],
      y = tb_ganancias[envios >= envg$PARAM$graficar$envios_desde, gan_sum],
      col = "red"
    )

    dev.off()

    # grabo las ganancias, para poderlas comparar con OTROS modelos
    arch_ganancias <- paste0(
      "ganancias_",
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana),
      ".txt"
    )

    fwrite(tb_ganancias,
      file = arch_ganancias,
      sep = "\t",
    )

    rm(tb_ganancias)
    gc()
  }

  # impresion ganancias
  rm(dtrain)
  rm(parametros)
  gc()
}

#------------------------------------------------------------------------------
if (future_con_clase) {
  envg$OUTPUT$ganancias_suavizadas <- vganancias_suavizadas
}

envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c()) 
cat( "z591_ZZ_final.r  END\n")
