# Experimentos Colaborativos Default
# ZZ con semillerio

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("primes")
require("yaml")

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
# impresion de los graficos cuando conozco la clase, cuando future tiene clase

ImprimirGraficos <- function(
    tb_ganancias, modelo_rank,
    iteracion_bayesiana, qsemillas) {
  # calculo la mayor ganancia  SUAVIZADA
  tb_ganancias[, gan_suavizada := frollmean(
    x = ganancia_acum,
    n = envg$PARAM$graficar$ventana_suavizado,
    align = "center",
    na.rm = TRUE,
    hasNA = TRUE
  )]

  ganancia_suavizada_max <- tb_ganancias[, max(gan_suavizada, na.rm = TRUE)]

  ymax <- max(tb_ganancias, na.rm = TRUE) * 1.05

  arch_grafico <- paste0(
    "modelo_",
    sprintf("%02d", modelo_rank),
    "_",
    sprintf("%03d", iteracion_bayesiana),
    ".pdf"
  )

  pdf(arch_grafico)

  # primera curva
  plot(
    x = tb_ganancias[, envios],
    y = tb_ganancias[, g1],
    type = "l",
    col = "gray",
    xlim = c(0, envg$PARAM$graficar$envios_hasta),
    ylim = c(0, ymax),
    main = paste0(
      "sem: ", qsemillas,
      " Mejor gan prom= ", as.integer(ganancia_suavizada_max)
    ),
    xlab = "Envios",
    ylab = "Ganancia",
    panel.first = grid()
  )

  # las siguientes curvas
  if (qsemillas > 1) {
    for (s in 2:qsemillas)
    {
      lines(
        x = tb_ganancias[, envios],
        y = tb_ganancias[, get(paste0("g", s))],
        col = "gray"
      )
    }
  }

  # finalmente la curva promedio
  lines(
    x = tb_ganancias[, envios],
    y = tb_ganancias[, ganancia_acum],
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
}
#------------------------------------------------------------------------------
# generacion de archivos para Kaggle, cuando future no tiene clase

GenerarKaggle <- function(
    tb_prediccion, modelo_rank,
    iteracion_bayesiana, sem) {
  # genero el vector de cortes
  cortes <- seq(
    from = envg$PARAM$kaggle$envios_desde,
    to = envg$PARAM$kaggle$envios_hasta,
    by = envg$PARAM$kaggle$envios_salto
  )


  # genero los archivos por probabilidad
  for (campo in c("prob"))
  {
    setorderv(tb_prediccion, c(campo), c(-1))
    # grabo los archivos para cada corte
    for (corte in cortes)
    {
      tb_prediccion[, Predicted := 0L]
      tb_prediccion[1:corte, Predicted := 1L]

      nom_submit <- paste0(
        envg$PARAM$experimento,
        "_",
        sprintf("%02d", modelo_rank),
        "_",
        sprintf("%03d", iteracion_bayesiana),
        "_",
        sprintf("%05d", corte),
        substr(campo, 1, 2),
        "_",
        sprintf("%03d", sem),
        ".csv"
      )

      fwrite(tb_prediccion[, list(numero_de_cliente, Predicted)],
        file = nom_submit,
        sep = ","
      )

      if (sem > 1) {
        nom_old <- paste0(
          envg$PARAM$experimento,
          "_",
          sprintf("%02d", modelo_rank),
          "_",
          sprintf("%03d", iteracion_bayesiana),
          "_",
          sprintf("%05d", corte),
          substr(campo, 1, 2),
          "_",
          sprintf("%03d", sem - 1), # old
          ".csv"
        )

        file.rename(
          paste0("./", nom_old),
          paste0("./old/", nom_old)
        )
      }
    }
  }
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
cat( "z593_ZZ_final_semillerio.r  START\n")
action_inicializar() 

# genero las semillas con las que voy a trabajar
#  ninguna de ellas es exactamente la original del alumno
primos <- generate_primes(min = 100000, max = 1000000)
set.seed(envg$PARAM$semilla)
# me quedo con envg$PARAM$semillerio  primos al azar
envg$PARAM$semillas <- sample(primos)[1:envg$PARAM$semillerio]

GrabarOutput()

# leo la salida de la optimizacion bayesiana
# En envg$PARAM$input[1]  tango el nombre del experimento de Hyperparameter Tuning
arch_log <- paste0( "./", envg$PARAM$input[1], "/BO_log.txt")
action_verificar_archivo( arch_log )
tb_log <- fread(arch_log)
setorderv(tb_log, "metrica", envg$PARAM$metrica_order)


# leo el dataset donde voy a entrenar el modelo final
# En envg$PARAM$input[2]  tango el nombre del experimento de TS Training Strategy
arch_dataset <- paste0("./", envg$PARAM$input[2], "/dataset_train_final.csv.gz")
action_verificar_archivo( arch_dataset )
cat("inicio lectura   dataset_train_final.csv.gz\n")
dataset <- fread(arch_dataset)
cat("fin lectura   dataset_train_final.csv.gz\n")
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input[2], "/dataset_metadata.yml" ) )

envg$PARAM$experimento <- carpeta_actual()  # nombre del experimento donde estoy parado

# leo el dataset donde voy a aplicar el modelo final
arch_future <- paste0("./", envg$PARAM$input[2], "/dataset_future.csv.gz")
dfuture <- fread(arch_future)

# logical que me indica si los dtos de future tienen la clase con valores,
# y NO va para Kaggle
# future_con_clase <- dfuture[clase_ternaria == "" | is.na(clase_ternaria), .N] == 0
# future_con_clase <- FALSE
future_con_clase <- dfuture[get(envg$PARAM$dataset_metadata$clase) == "" | is.na(get(envg$PARAM$dataset_metadata$clase)), .N] == 0

if (!future_con_clase) {
  # creo carpeta old, donde voy moviendo el kaggle viejo
  dir.create(paste0("old/"), showWarnings = FALSE)
}

# defino la clase binaria
dataset[, clase01 := ifelse(get(envg$PARAM$dataset_metadata$clase) %in%  envg$PARAM$train$clase01_valor1, 1, 0)]


campos_buenos <- setdiff(colnames(dataset), c(envg$PARAM$dataset_metadata$clase, "clase01"))


dir.create("modelitos", showWarnings = FALSE)

# genero un modelo para cada uno de las modelos_qty MEJORES iteraciones
#  de la Bayesian Optimization
vganancias_suavizadas <- c()

imodelo <- 0L
for (modelo_rank in envg$PARAM$modelos_rank)
{
  imodelo <- imodelo + 1L
  cat("\nmodelo_rank: ", modelo_rank, ", semillas: ")
  envg$OUTPUT$status$modelo_rank <- modelo_rank

  parametros <- as.list(copy(tb_log[modelo_rank]))
  iteracion_bayesiana <- parametros$iteracion_bayesiana


  # creo CADA VEZ el dataset de lightgbm
  cat( "\ncreando  dtrain para rank =", modelo_rank, "\n")
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[, campos_buenos, with = FALSE]),
    label = dataset[, clase01],
    weight = dataset[, ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, 1.0000001, 1.0)],
    free_raw_data = FALSE
  )
  cat( "dtrain para rank =", modelo_rank, " creado\n")

  ganancia <- parametros$ganancia

  # elimino los parametros que no son de lightgbm
  parametros$experimento <- NULL
  parametros$cols <- NULL
  parametros$rows <- NULL
  parametros$fecha <- NULL
  parametros$estimulos <- NULL
  parametros$ganancia <- NULL
  parametros$iteracion_bayesiana <- NULL

  #  parametros$num_iterations  <- 10  # esta linea es solo para pruebas en desarrollo

  if (future_con_clase) {
    tb_ganancias <- as.data.table(list("envios" = 1:1:envg$PARAM$graficar$envios_hasta))
    tb_ganancias[, gan_sum := 0.0]
  }

  # inicializo  tb_prediccion
  tb_prediccion <- dfuture[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_prediccion[, pos := .I]
  tb_prediccion[, semillas := 0L]
  # aqui voy a acumular la probabildad del semillerio
  tb_prediccion[, sum_prob_acumulada := 0]
  # aqui voy a acumular el ranking del semillerio
  tb_prediccion[, sum_rank_acumulado := 0]

  sem <- 0L

  for (vsemilla in envg$PARAM$semillas) # recorro las semillas del Semillerio
  {
    sem <- sem + 1L
    cat( " ", sem, " " )
    envg$OUTPUT$status$sem <- sem
    GrabarOutput()

    setorder(tb_prediccion, pos) # ordeno por el original

    # Utilizo la semilla definida en este script
    parametros$seed <- vsemilla

    nombre_raiz <- paste0(
      sprintf("%02d", modelo_rank),
      "_",
      sprintf("%03d", iteracion_bayesiana)
    )

    arch_modelo <- paste0(
      "./modelitos/modelo_",
      nombre_raiz,
      "_",
      parametros$seed,
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
        grabar_importancia(modelo_final, modelo_rank, iteracion_bayesiana)

        cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
            file = "z-Rcanresume.txt",
            append = TRUE
        )
      }

    } else {
       modelo_final <- lgb.load(filename = arch_modelo)
    }

    # genero la prediccion, Scoring
    prediccion <- predict(
      modelo_final,
      data.matrix(dfuture[, campos_buenos, with = FALSE])
    )

    tb_prediccion[, prob_semilla := prediccion]
    tb_prediccion[, rank_semilla := frank(prediccion, ties.method = "random")]
    tb_prediccion[, sum_prob_acumulada := sum_prob_acumulada + prediccion]
    tb_prediccion[, sum_rank_acumulado := sum_rank_acumulado + frank(prediccion, ties.method = "random")]

    tb_prediccion[, semillas := sem]
    tb_prediccion[, prob := sum_prob_acumulada / semillas]
    tb_prediccion[, ranking := sum_rank_acumulado / semillas]

    fwrite(tb_prediccion[, list(numero_de_cliente, foto_mes, semillas, prob, ranking, clase_ternaria)],
      file = paste0("pred_", nombre_raiz, ".csv"),
      sep = "\t"
    )

    if (!future_con_clase) {
      GenerarKaggle(tb_prediccion, modelo_rank, iteracion_bayesiana, sem)
    }


    if (future_con_clase) {
      setorder(tb_prediccion, -prob_semilla)

      tb_ganancias[, paste0("g", sem) := tb_prediccion[
        1:envg$PARAM$graficar$envios_hasta,
        cumsum(ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, envg$PARAM$train$gan1, envg$PARAM$train$gan0))
      ]]

      setorder(tb_prediccion, -prob)

      tb_ganancias[, ganancia_acum := tb_prediccion[
        1:envg$PARAM$graficar$envios_hasta,
        cumsum(ifelse(get(envg$PARAM$dataset_metadata$clase) %in% envg$PARAM$train$positivos, envg$PARAM$train$gan1, envg$PARAM$train$gan0))
      ]]

      ImprimirGraficos(tb_ganancias, modelo_rank, iteracion_bayesiana, sem)
    }

    # borro y limpio la memoria para la vuelta siguiente del for
    rm(modelo_final)
    gc()
  } # end del for semillas

  rm(tb_ganancias)
  # impresion ganancias
  rm(dtrain)
  rm(parametros)
  gc()
} # end  for  modelo_rank

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
cat( "z593_ZZ_final_semillerio.r  END\n")
