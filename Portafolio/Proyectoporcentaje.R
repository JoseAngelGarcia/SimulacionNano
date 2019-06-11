library(parallel)

bases = c("A", "U", "G", "C")
codonGlut = c("UGU", "UGC")
locacion= c(1077, 1054, 1068, 1106, 1042, 1064,1084, 1058, 1864)
repeticiones= 30
poblacion= 5000

cluster <- makeCluster(detectCores()- 1)
clusterExport(cluster, "poblacion")
clusterExport(cluster, "repeticiones")
clusterExport(cluster, "bases")
clusterExport(cluster, "codonGlut")
clusterExport(cluster, "cuantosGlut")

pdatos <- data.frame()

for (Locacion in locacion ) {
 clusterExport(cluster, "Locacion")

  enfermos <- 0
  cuantosGlut<- 0
  resultadoP <- parSapply(cluster, 1:repeticiones,
                          function(r) {
                            cuantosglut = 0
                            enfermos = 0
                            for (p in 1:poblacion) { #mod
                              secuencia <- paste(sample(bases, Locacion, replace = TRUE), collapse = "")
                               for(i in 1:(nchar(secuencia) - 2)){   #revisar
                                
                                (codon = substring(secuencia, i, i + 2))
                               
                                (esUGU = codon %in%  codonGlut)
                              
                                if(esUGU){
                                  cuantosGlut = (cuantosGlut + 1)}
                              }
                            return(cuantosGlut)}
                              if (cuantosGlut > 36){
                                enfermos= enfermos + 1}
                              return (enfermos)
                            
                          })
  
  pdatos <- rbind(pdatos, ((resultadoP/poblacion)*100))
}
stopCluster(cluster)
colnames(pdatos)<- c(rep(1:30, 1))
rownames(pdatos)<-c("Monterrey", "Guadalupe", "San Nicolás", "Apodaca", "Escobedo", "San Pedro", "Juárez", "Santa Catarina", "Saltillo")
png("graficaPorcentaje.png", width=900, height=600)
boxplot(data.matrix(pdatos), use.cols=FALSE,main=" ",xlab = "Municipio", ylab = "% de enfermos", col= "blue")
graphics.off()

apilados<- stack(pdatos)
Anova<- aov(values ~ ind, data= apilados)
summary (Anova)