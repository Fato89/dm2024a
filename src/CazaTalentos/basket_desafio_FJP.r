
set.seed( 980071 )
#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty ){
  vsize <- length(prob)
  return( rowSums( matrix( runif( qty*vsize ), vsize, qty)  <
                   matrix( rep(prob,qty), vsize, qty)  ) )
}

#defino los jugadores
mejor      <- 0.7
peloton    <- ( 501:599 ) / 1000

#defino distancia de salida
dist_max <- 15
resultado <- matrix(nrow = 10000,ncol = 2)

t0  <- Sys.time()

primero_ganador  <- 0

for(i in 1:10000) {
  #diez mil experimentos
  
  total_tiros <- 0
  jugadores  <- c( peloton, mejor ) #intencionalmente el mejor esta al final
  vaciertos <- rep(0, length(jugadores))
  njug <- length(jugadores)
    
  while (njug > 1) {
    vaciertos  <- vaciertos + ftirar(jugadores, 1)
    total_tiros <- total_tiros+length(jugadores)
    dista <- max(vaciertos) - min(vaciertos)
    
    if (dista > dist_max) {
      jugadores <- jugadores[max(vaciertos)-vaciertos <= dist_max]
      vaciertos <- vaciertos[max(vaciertos)-vaciertos <= dist_max]
    }
    
    njug <- length(jugadores)
  }
  
  resultado[i,1] <- mejor %in% jugadores
  resultado[i,2] <- total_tiros
  
  }

cat(mean(resultado[,1]),"% con", mean(resultado[,2]) ,"tiradas promedio \n" )

t1  <- Sys.time()

cat( t1 - t0 )

