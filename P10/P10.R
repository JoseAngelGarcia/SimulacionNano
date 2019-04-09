library(testit)
tiempo1<-data.frame()

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]               
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

objet <- function(i){
  return(objetivo(p[i,], valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, (rnorm(1, media, desv) / (pesos[i]))+(rnorm(1)/10))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

mutar <-function(i){
  if (runif(1) < pm) {
    return(mutacion(p[i,], n))
    #p <- rbind(p, mutacion(p[i,], n))
  }
}

reproducete <- function(i){
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
  return(list(hijos[1:n], hijos[(n+1):(2*n)]))
}

factibilidad <- function(i){
  return(factible(p[i,], pesos, capacidad))
}
 
library(parallel)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, c("mutar", "mutacion", "reproduccion", "normalizar", "reproducete", "objetivo", "factible"))
tamaño <-seq(50, 100, by= 25)
for (n in tamaño){
    inicio1 <-Sys.time()
    pesos <- generador.pesos(tamaño, 15, 80)
    valores <- generador.valores(pesos, 10, 500)
    capacidad <- round(sum(pesos) * 0.65)
    print(n)
    optimo <- knapsack(capacidad, pesos, valores)
    final1 <- Sys.time()
    #termina optimo
    inicial1 <- Sys.time()
    init <- 200
    p <- poblacion.inicial(n, init)
    tam <- dim(p)[1]
    assert(tam == init)
    pm <- 0.05
    rep <- 50
    tmax <- 50
    mejores <- double()
    clusterExport(cluster, c("n", "pm", "valores", "pesos", "capacidad", "objetivo"))
    for (iter in 1:tmax) {
      p$obj <- NULL
      p$fact <- NULL
      clusterExport(cluster, "p")
      mute <- parSapply(cluster, 1:tam, mutar)
      for(i in 1:length(mute)){ 
        if(!is.null(mute[[i]])){
          p <- rbind(p, mute[[i]])
        }
      }
        clusterExport(cluster, c("tam", "p"))
      S <- parSapply(cluster, 1:rep, reproducete) # una cantidad fija de reproducciones
   
      for (agrego in 1:length(S)) {
        p <- rbind(p, S[[agrego]])
      }
      tam <- dim(p)[1]
      obj <- double()
      fact <- integer()
      clusterExport(cluster, c("p", "tam", "factibilidad", "objet"))
      obj <-  parSapply(cluster, 1:tam, objet)
      fact <- parSapply(cluster, 1:tam, factibilidad)
      p <- cbind(p, obj)
      p <- cbind(p, fact)
      mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
      p <- p[mantener,]
      tam <- dim(p)[1]
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
    termino1<- Sys.time()
    # png(paste("p10", "-", n, "-", iter, "-", replicas, ".png", sep=""), width=600, height=300)
    # plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
    # points(1:tmax, mejores, pch=15)
    # abline(h=optimo, col="green", lwd=3)
    # graphics.off()
    print(paste(mejor, (optimo - mejor) / optimo))
    beneficio <- optimo/as.numeric(final1-inicio1)
    beneficio1 <- mejor/as.numeric(termino1-inicial1)
    tiempo2 <- rbind(tiempo1, c(n, beneficio))
    tiempo1 <- rbind(tiempo1, c(n, beneficio1))
  }
names(tiempo2) <- c("Tamaño", "BeneficioTiempo")
names(tiempo1) <- c("Tamaño", "BeneficioTiempo")
tiempo1$Nivel <- "P"
tiempo2$Nivel <- "O"
completo1 <- rbind(tiempo1, tiempo2)

setEPS()
postscript("beneficiocon.eps")
plot(completo1$BeneficioTiempo~completo1$Nivel+completo1$Tamaño, xlab = "Cantidad de objetos", ylab = "Beneficio", col = c("yellow", "orange"))
legend("bottomleft", title = "Implementación", c("Secuencial", "Paralela"), cex = 0.8, fill = c("Yellow", "Orange"))
graphics.off()

setEPS()
postscript("beneficiosin.eps")
boxplot(completo1$BeneficioTiempo~completo1$Nivel+completo1$Tamaño, xlab = "Cantidad de objetos", ylab = "Beneficio", outline=FALSE, col = c("yellow", "orange"), names = c(rep("100", 2), rep("200", 2), rep("300", 2)))
legend("topright", title = "Implementación", c("Secuencial", "Paralela"), cex = 0.8, fill = c("Yellow", "Orange"))
graphics.off()

tiemposOp<-c(tiemposOp, )