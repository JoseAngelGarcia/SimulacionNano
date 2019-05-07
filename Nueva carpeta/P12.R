
library(doParallel)
library(ggplot2)

binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}
decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

r <- 5
c <- 3
dim <- r * c
tranqui <- 0.99
tope <- 9
digitos <- 0:tope
k <- length(digitos)
replica<-30
tmax<-5000
entrenamiento <- ceiling(0.7 * tmax)
prueba <- tmax - entrenamiento
datos<- data.frame( Réplica= integer(), PNegras=integer(), PGrises=integer(), PBlancas=integer(), Porcentaje=integer())

modelos <- read.csv("modelos2.csv", sep=" ", header=FALSE, stringsAsFactors=F)

#variar las probabilidades 
for (PN in c(0.995,0.8)) {
  for(PG in c(0.75,0.65)){
    for(PB in c(0.001,0.1)){
      modelos[modelos=='n'] <- PN # pixeles negros en plantillas
      modelos[modelos=='g'] <- PG # pixeles grises en plantillas
      modelos[modelos=='b'] <- PB # pixeles blancos en plantillas
      for(replicas in 1:replica){
        print(replicas)
        tasa <- 0.15
        contadores <-vector()
        n <- floor(log(k-1, 2)) + 1
        neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
        for (t in 1:entrenamiento) { # entrenamiento
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,]
          correcto <- binario(d, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
              ajuste <- tasa * (deseada - resultado)
              tasa <- tranqui * tasa
              neuronas[i,] <- w + ajuste * pixeles
            }
          }
        }
clusterExport(cluster, c( "neuronas", "binario", "decimal", "modelos", "tope" ,"k",
                          "dim", "n"))
contadores <-parSapply(cluster, 1:prueba , function(x){
d <- sample(0:tope, 1)
pixeles <- runif(dim) < modelos[d + 1,] 
correcto <-binario(d, n)
 salida <- rep(FALSE, n)
    for (i in 1:n) {
      w <- neuronas[i,]
       deseada <- correcto[i]
        resultado <- sum(w * pixeles) >= 0
         salida[i] <- resultado
          }
          r <- min(decimal(salida, n), k) 
          return(r==d)})
datos<-rbind(datos, data.frame(Replica= replicas, PNegras=PN, PGrises=PG, PBlancas=PB, 
                                       Porcentaje=(sum(contadores)/prueba)*100))
        
      }
    }
  }
}

datos$PGrisesPBlancas <- interaction(datos$PGrises, datos$PBlancas)

ggplot(aes(y = Porcentaje, x = PGrisesPBlancas, fill = PNegras), data = datos, group (PBlancas)) + geom_boxplot() + labs(title=NULL,             # plot title
                                                                                                                         x="Probabilidad Grises.Blancas",      # x-axis label
                                                                                                                        y="Porcentaje %")

