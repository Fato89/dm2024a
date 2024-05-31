# Wofkflow Semillerio  sin Hyperparameter Tuning

##########################
###  EN  CONSTRUCCION  ###
##########################

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")

# creo environment global
envg <- env()

envg$EXPENV <- list()
envg$EXPENV$bucket_dir <- "~/buckets/b1"
envg$EXPENV$exp_dir <- "~/buckets/b1/expw/"
envg$EXPENV$wf_dir <- "~/buckets/b1/flow/"
envg$EXPENV$repo_dir <- "~/dm2024a/"
envg$EXPENV$datasets_dir <- "~/buckets/b1/datasets/"
envg$EXPENV$arch_sem <- "mis_semillas.txt"
envg$EXPENV$messenger <- "~/install/zulip_enviar.sh"


#------------------------------------------------------------------------------
# Error catching

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rabort.txt",
    append = TRUE 
    )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------
# inicializaciones varias

dir.create( envg$EXPENV$exp_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)

#------------------------------------------------------------------------------
# cargo la  "libreria" de los experimentos

exp_lib <- paste0( envg$EXPENV$repo_dir,"/src/lib/exp_lib.r")
source( exp_lib )

#------------------------------------------------------------------------------
# Incorporacion Dataset
# deterministico, SIN random

DT_incorporar_dataset_competencia2024 <- function()
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija

  param_local$meta$script <- "/src/workflow-01/z511_DT_incorporar_dataset.r"

  param_local$archivo <- "competencia_2024.csv.gz"
  param_local$primarykey <- c("numero_de_cliente", "foto_mes" )
  param_local$entity_id <- c("numero_de_cliente" )
  param_local$periodo <- c("foto_mes" )
  param_local$clase <- c("clase_ternaria" )

  param_local$semilla <- NULL  # no usa semilla, es deterministico

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Catastophe Analysis  Baseline
# deterministico, SIN random
# MachineLearning EstadisticaClasica Ninguno

CA_catastrophe_bueno <- function( pinputexps, metodo )
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija

  param_local$meta$script <- "/src/workflow-01/z521_CA_reparar_dataset.r"

  # Opciones MachineLearning EstadisticaClasica Ninguno
  param_local$metodo <- metodo
  param_local$semilla <- NULL  # no usa semilla, es deterministico

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Feature Engineering Intra Mes   Baseline
# deterministico, SIN random

FEintra_bueno <- function( pinputexps )
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija


  param_local$meta$script <- "/src/workflow-01/z531_FE_intrames.r"

  param_local$semilla <- NULL  # no usa semilla, es deterministico

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Data Drifting Baseline
# deterministico, SIN random

DR_drifting_bueno <- function( pinputexps, metodo)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija


  param_local$meta$script <- "/src/workflow-01/z541_DR_corregir_drifting.r"

  # valores posibles
  #  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
  param_local$metodo <- metodo
  param_local$semilla <- NULL  # no usa semilla, es deterministico

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Feature Engineering Historico  Baseline
# deterministico, SIN random

FEhist_bueno <- function( pinputexps)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija


  param_local$meta$script <- "/src/workflow-01/z551_FE_historia.r"

  param_local$lag1 <- TRUE
  param_local$lag2 <- TRUE # no me engraso con los lags de orden 2
  param_local$lag3 <- FALSE # no me engraso con los lags de orden 3

  # no me engraso las manos con las tendencias
  param_local$Tendencias1$run <- TRUE  # FALSE, no corre nada de lo que sigue
  param_local$Tendencias1$ventana <- 6
  param_local$Tendencias1$tendencia <- TRUE
  param_local$Tendencias1$minimo <- TRUE
  param_local$Tendencias1$maximo <- TRUE
  param_local$Tendencias1$promedio <- TRUE
  param_local$Tendencias1$ratioavg <- TRUE
  param_local$Tendencias1$ratiomax <- TRUE

  # no me engraso las manos con las tendencias de segundo orden
  param_local$Tendencias2$run <- FALSE
  param_local$Tendencias2$ventana <- 12
  param_local$Tendencias2$tendencia <- FALSE
  param_local$Tendencias2$minimo <- FALSE
  param_local$Tendencias2$maximo <- FALSE
  param_local$Tendencias2$promedio <- FALSE
  param_local$Tendencias2$ratioavg <- FALSE
  param_local$Tendencias2$ratiomax <- FALSE

  param_local$semilla <- NULL # no usa semilla, es deterministico

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
#  Agregado de variables de Random Forest, corrido desde LightGBM
#  atencion, parmetros para generar variables, NO para buen modelo
#  azaroso, utiliza semilla

FErf_attributes_bueno <- function( pinputexps, ratio, desvio)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z534_FE_rfatributes.r"

  # Parametros de un LightGBM que se genera para estimar la column importance
  param_local$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
  param_local$train$training <- c( 202101, 202102, 202103)

  # parametros para que LightGBM se comporte como Random Forest
  param_local$lgb_param <- list(
    # parametros que se pueden cambiar
    num_iterations = 20,
    num_leaves  = 16,
    min_data_in_leaf = 1000,
    feature_fraction_bynode  = 0.3,

    # para que LightGBM emule Random Forest
    boosting = "rf",
    bagging_fraction = ( 1.0 - 1.0/exp(1.0) ),
    bagging_freq = 1.0,
    feature_fraction = 1.0,

    # genericos de LightGBM
    max_bin = 31L,
    objective = "binary",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE,
    verbosity = -100,
    max_depth = -1L,
    min_gain_to_split = 0.0,
    min_sum_hessian_in_leaf = 0.001,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,

    pos_bagging_fraction = 1.0,
    neg_bagging_fraction = 1.0,
    is_unbalance = FALSE,
    scale_pos_weight = 1.0,

    drop_rate = 0.1,
    max_drop = 50,
    skip_drop = 0.5,

    extra_trees = FALSE
  )


  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Canaritos Asesinos   Baseline
#  azaroso, utiliza semilla

CN_canaritos_asesinos_base <- function( pinputexps, ratio, desvio)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z561_CN_canaritos_asesinos.r"

  # Parametros de un LightGBM que se genera para estimar la column importance
  param_local$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
  param_local$train$positivos <- c( "BAJA+2")
  param_local$train$training <- c( 202101, 202102, 202103)
  param_local$train$validation <- c( 202105 )
  param_local$train$undersampling <- 0.1
  param_local$train$gan1 <- 117000
  param_local$train$gan0 <-  -3000


  # ratio varia de 0.0 a 2.0
  # desvio varia de -4.0 a 4.0
  param_local$CanaritosAsesinos$ratio <- ratio
  # desvios estandar de la media, para el cutoff
  param_local$CanaritosAsesinos$desvios <- desvio

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy  Baseline
#   y solo incluyo en el dataset al 20% de los CONTINUA
#  azaroso, utiliza semilla

TS_strategy_bueno9 <- function( pinputexps )
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z571_TS_training_strategy.r"


  param_local$future <- c(202109)
  param_local$final_train <- c(202107, 202106, 
    202105, 202104, 202103, 202102, 202101, 
    202012, 202011, 202010, 202009, 202008, 202007,
    # 202006 - Excluyo este mes con variables rotas
    202005, 202004, 202003, 202002, 202001,
    201912, 201911,
    # 201910, - Excluyo este mes con variables rotas
    201909, 201908, 201907, 201906,
    # 201905, - Excluyo este mes con variables rotas
    201904, 201903
    )

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# proceso ZZ_final  Semillerio  sin Hyperparameter Tuning

###################
# En construccion #
###################

ZZ_final_semi_sinHT <- function( pinputexps)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z595_ZZ_final_semillerio_sinHT.r"

  # Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
  param_local$modelos_rank <- c(1)
  param_local$metrica_order <- -1  # ordeno por el campo metrica en forma DESCENDENTE
  
  # Que modelos quiero, segun su iteracion_bayesiana de la Bayesian Optimizacion, SIN ordenar
  param_local$modelos_iteracion <- c()

  param_local$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
  param_local$train$positivos <- c( "BAJA+2")
  param_local$train$gan1 <- 117000
  param_local$train$gan0 <-  -3000
  param_local$train$meseta <- 2001

  param_local$kaggle$envios_desde <-  6500L
  param_local$kaggle$envios_hasta <- 14000L
  param_local$kaggle$envios_salto <-   500L
  param_local$kaggle$competition <- "itba-data-mining-2024-a"

  # para el caso que deba graficar
  param_local$graficar$envios_desde <-  8000L
  param_local$graficar$envios_hasta <- 20000L
  param_local$graficar$ventana_suavizado <- 2001L

  # El parametro fundamental de semillerio
  # Es la cantidad de LightGBM's que ensamblo
  # cuanto mas grande mejor, pero asintotico
  param_local$semillerio <- 100

  # para el modelo final NO entreno en todos los datos
  param_local$undersampling <- 0.1

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# A partir de ahora comienza la seccion de Workflows Completos
#------------------------------------------------------------------------------
# Este es el  Workflow Semillerio
# Que predice 202109 donde NO conozco la clase
# y genera archivos para kaggle

###################
# En construccion #
###################

wf_semillerio_sinHT <- function( pnombrewf )
{
  param_local <- exp_wf_init( pnombrewf ) # linea fija

  DT_incorporar_dataset_competencia2024()
  CA_catastrophe_bueno( metodo="MachineLearning")
  FEintra_bueno()
  DR_drifting_bueno(metodo="rank_cero_fijo")
  FEhist_bueno()
  FErf_attributes_bueno()

  ts9 <- TS_strategy_bueno9()

  ZZ_final_semi_sinHT( ts9 )

  return( exp_wf_end() ) # linea fija
}
#------------------------------------------------------------------------------


# llamo al workflow con future = 202109
wf_semillerio_sinHT()
