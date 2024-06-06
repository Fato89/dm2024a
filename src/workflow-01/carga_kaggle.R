# cargar_kaggle
carpeta <- ""
setwd(carpeta)

archivos_carga <- list.files(carpeta,'ZZ\\-.+\\.csv',full.names = TRUE)
map(
  archivos_carga,
  ~{
    l1 <- "#!/bin/bash \n"
    l2 <- "source ~/.venv/bin/activate  \n"
    l3 <- "kaggle competitions submit -c itba-data-mining-2024-a"
    l3 <- paste0( l3, " -f ", .x )
    l3 <- paste0( l3,  " -m ",  "\"", carpeta,  " , ",  .x , "\"",  "\n")
    l4 <- "deactivate \n"
    
    cat( paste0( l1, l2, l3, l4 ) , file = "subir.sh" )
    Sys.chmod( "subir.sh", mode = "744", use_umask = TRUE)
    
    system( "./subir.sh" )
  }
)

