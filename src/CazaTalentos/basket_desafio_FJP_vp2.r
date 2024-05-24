
# set.seed( 980071 )
#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty ){
  vsize <- length(prob)
  return( rowSums( matrix( runif( qty*vsize ), vsize, qty)  <
                   matrix( rep(prob,qty), vsize, qty)  ) )
}

#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000

resultado <- matrix(nrow = 10000,ncol = 2)
lanza_prueba <- 100
pb <- txtProgressBar(min = 0, max = 10000, initial = 0) 

t0  <- Sys.time()

primero_ganador  <- 0

for(i in 1:10000) {

  #diez mil experimentos
  
  total_tiros <- 0
  jugadores  <- c( peloton, mejor ) #intencionalmente el mejor esta al final
  vaciertos <- rep(0, length(jugadores))
  njug <- length(jugadores)
  t <- 0
    
  while (njug > 1) {
    t <- t+1
    vaciertos   <- vaciertos + ftirar(jugadores, 1)
    total_tiros <- total_tiros+length(jugadores)
    
    
    if(t>lanza_prueba){
      min01p <- qbinom(.01, t, max(vaciertos)/t, lower.tail = TRUE)
      jugadores <- jugadores[vaciertos > min01p]
      vaciertos <- vaciertos[vaciertos > min01p]
    }
    
    njug <- length(jugadores)
    # cat(njug,"\n")
  }
  
  setTxtProgressBar(pb, i)
  resultado[i,1] <- mejor %in% jugadores
  resultado[i,2] <- total_tiros
  
  }
close(pb)
cat(mean(resultado[,1],na.rm = T)*100,"% con", mean(resultado[,2],na.rm = T) ,"tiradas promedio \n" )

t1  <- Sys.time()

cat( t1 - t0 )

