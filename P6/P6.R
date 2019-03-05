l <- 1.5
n <- 50 #numero de agentes
pi <- 0.05 #probabilidad de infeccion al inicio
pr <- 0.02 #probabilidad de recuperacion
v <- l / 30 #velocidad del agente
PV <- seq(0,1,0.1) #probabilidad de vacunacion inicial
r <- 0.1 #umbral
tmax<- 100
Resultados <- data.frame()


for(pv in PV){  #variando la probabilidad de vacunacion 
  for(rep in 1:40){  #con 40 repeticiones
    agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(),
                          estado  = character())
    for (i in 1:n) {
      if(runif(1) < pv){ 
        e <- "R"
      } else if(runif(1) < pi){
        e <- "I"
      } else{
        e <- "S"
      }
            agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                           dx = runif(1, -v, v), dy = runif(1, -v, v),
                                          estado = e))
            levels(agentes$estado) <- c("S", "I", "R") #se determinan los posibles estados
    }
        epidemia <- integer()
      for (tiempo in 1:tmax){ 
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
     
          a$x <- a$x + a$dx
          a$y <- a$y + a$dy
        
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
    }
  
    
    }
    maximo <- max(epidemia)
    porcentaje <- (maximo / n) * 100
    Resultados <- rbind(Resultados, c(pv, rep, maximo, porcentaje))
    print(pv) 
  }
}
colnames(Resultados) <- c("Probabilidad", "Replicas", "Maximos", "Porcentaje")




png("graficaP6.png")
probabilidades <- Resultados$Probabilidad
porcentaje <- Resultados$Porcentaje
boxplot(porcentaje~probabilidades, col = "cornflowerblue", xlab = "Probabilidades de vacunaci\u{F3}n", ylab = "% m\u{E1}ximo de infectados")
graphics.off()

