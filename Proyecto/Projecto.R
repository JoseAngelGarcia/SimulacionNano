library(parallel)

bases = c("A", "U", "G", "C")
codonesCist = c("UGC")
repeticiones = 30
poblacion = 10
min = 17

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "bases")
clusterExport(cluster, "codonesCist")
clusterExport(cluster, "poblacion")
Datos<- data.frame()

clusterExport(cluster, "minimo")
for (mínimo in 14:min) {
  cuantosCist <- 0
Resultados <- parSapply (cluster, 1:poblacion, function(r) {
          secuencia <- paste(sample(bases, 1000, replace = TRUE), collapse = "")
          enfermos<- 0 #mayor <- 0
          cuantosCist <- 0
          for (minimo in 1:min){
            for(i in 1:(nchar(secuencia) - 2)){
              (codon = substring(secuencia, i, i + 2))
              (esUGC = codon %in%  codonesCist)
              if(esUGC){ 
               cuantosCist = (cuantosCist + 1)
                      }
                    } 
                      return((cuantosCist))
            if(cuantosCist > 34){
              enfermos = (enfermos + 1)}
            } 
                         })
                    
Datos<- rbind (Datos, Resultados)

print(Datos)


