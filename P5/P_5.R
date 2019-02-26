cant<-c (100, 1000, 10000, 100000, 1000000)
repeticiones<-c(50)
diferencia<-data.frame()
inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) 
desde <- 3
hasta <- 7
parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for (resultado in cant){
  for (cantidad in repeticiones) {
    for (repetir in 1:10) {
      pedazo <- resultado
      cuantos <- cantidad
      montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
      stopImplicitCluster()
      integral <- sum(montecarlo) / (cuantos * pedazo)
      aproximar<-((pi / 2) * integral)
      diferencia<-rbind(diferencia, c(abs(aproximar-0.0488340), resultado, cantidad))
    }
  }
}

names(diferencia)<-c("Diferencia", "Cantidad","Replicas")
boxplot(diferencia$Diferencia~diferencia$Replicas+diferencia$Cantidad, use.cols=FALSE, ylim = c(0, 0.0003), boxwex=0.3, xlab="Tama\u{F1}o de muestra", ylab="Diferencia a la aproximación", names=c(100, 1000, 10000, 100000, 1000000), col=c(rep("blue", 5)))
abline(h=0.01, lty=1, lwd=2, col="yellow")
abline(h=0.001, lty=1, lwd=2, col="orange")
abline(h=0.00001, lty=1, lwd=2, col="green")
abline(h=0.0001, lty=1, lwd=2, col="red")

legend("topright", legend=c("2 dígitos", "3 dígitos", "4 dígitos", "5 dígitos"), title="Exactitud", cex = 0.8, fill = c("yellow", "orange", "red", "green"))
graphics.off()