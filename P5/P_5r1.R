
apropi=function() {
  xs <- runif(replicas, min= -0.5, max= 0.5)
  ys <- runif(replicas, min= -0.5, max= 0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/replicas)*4
  return(mc.pi) 
}

suppressMessages(library(doParallel)) 
registerDoParallel(makeCluster(detectCores() - 1)) 

resultados=data.frame() 

for(replicas in c(10, 100, 1000 , 10000, 100000, 1000000)) { 
  for(i in 1:20) { 
    montecarlo <- foreach(i = 1:500, .combine=c) %dopar% apropi() 
    mc.pi=sum(montecarlo)/500
    diferencia=abs(pi-mc.pi) 
    pii <- pi
    resultados=rbind(resultados,c(replicas,i,pii,mc.pi,diferencia))
  }
}

names(resultados)=c("Muestra","Replicas","Valor de Pi","aprox.pi","Diferencia") 

png("exactPi.png") 
boxplot(data=resultados,diferencia~muestra,xlab="Tamaño de la muestra",ylab="Diferencia de aproximación", col=c(rep("pink", 5)), ylim= c(0, 0.03), main="")

abline(h=0.01, lty=1, lwd=2, col="yellow")
abline(h=0.001, lty=1, lwd=2, col="orange")
abline(h=0.0001, lty=1, lwd=2, col="red")
abline(h=0.00001, lty=1, lwd=2, col="green")


legend("topright", legend=c("2 dígitos", "3 dígitos", "4 dígitos", "5 dígitos"), title="Exactitud", cex = 0.8, fill = c("yellow", "orange", "red", "green"))

graphics.off()


