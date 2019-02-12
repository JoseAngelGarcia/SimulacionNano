primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
primos <- numeric()
for (n in 270000:271190) {
  if (primo(n)) {
    primos <-  c(primos, n)
  }
}
noprimos<- c(270000, 270002, 270003, 270004, 270005, 270006, 270007, 270008, 270009, 270010, 270011, 270012, 270013, 270014, 270015, 270016, 270017, 270018, 270019, 270020, 270021, 270022, 270023, 270024, 270025, 270026, 270027, 270028, 270030, 270032, 270033, 270034, 270035, 270036, 270038, 270039, 270040, 270041, 270042, 270043, 270044, 270045, 270046, 270047, 270048, 270049, 270050, 270051, 270052, 270053, 270054, 270055, 270056, 270057, 270058, 270060, 270061, 270062, 270063, 270064, 270065, 270066, 270067, 270068, 270069, 270070, 270072, 270074, 270075, 270076, 270077, 270078, 270079, 270080, 270081, 270082, 270083, 270084, 270085, 270086, 270087, 270088, 270089, 270090, 270091, 270092, 270093, 270094, 270095, 270096, 270098, 270099, 270100, 270101, 270102, 270103, 270104, 270105, 270106, 270107, 270108)

combinacion<- sort(c(270001, 270029, 270031, 270037, 270059, 270071, 270073, 270097, 270121, 270131, 270133, 270143, 270157, 270163, 270167, 270191, 270209, 270217, 270223, 270229, 270239, 270241, 270269, 270271, 270287, 270299, 270307, 270311, 270323, 270329, 270337, 270343, 270371, 270379, 270407, 270421, 270437, 270443, 270451, 270461, 270463, 270493, 270509, 270527, 270539, 270547, 270551, 270553, 270563, 270577, 270000, 270002, 270003, 270004, 270005, 270006, 270007, 270008, 270009, 270010, 270011, 270012, 270013, 270014, 270015, 270016, 270017, 270018, 270019, 270020, 270021, 270022, 270023, 270024, 270025, 270026, 270027, 270028, 270030, 270032, 270033, 270034, 270035, 270036, 270038, 270039, 270040, 270041, 270042, 270043, 270044, 270045, 270046, 270047, 270048, 270049, 270050, 270051, 270052, 270053)) 

combinacionP<- sort(c(270001, 270029, 270031, 270037, 270059, 270071, 270073, 270097, 270121, 270131, 270133, 270143, 270157, 270163, 270167, 270191, 270209, 270217, 270223, 270229, 270239, 270241, 270269, 270271, 270287, 270299, 270307, 270311, 270323, 270329, 270337, 270343, 270371, 270379, 270407, 270421, 270437, 270443, 270451, 270461, 270463, 270493, 270509, 270527, 270539, 270547, 270551, 270553, 270563, 270577, 270583, 270587, 270593, 270601, 270619, 270631, 270653, 270659, 270667, 270679, 270689, 270701, 270709, 270719, 270737, 270749, 270761, 270763, 270791, 270797, 270799, 270821, 270833, 270841, 270859, 270083, 270084, 270085, 270086, 270087, 270088, 270089, 270090, 270091, 270092, 270093, 270094, 270095, 270096, 270098, 270099, 270100, 270101, 270102, 270103, 270104, 270105, 270106, 270107, 270108))

combinacionNP<- sort(c(270000, 270002, 270003, 270004, 270005, 270006, 270007, 270008, 270009, 270010, 270011, 270012, 270013, 270014, 270015, 270016, 270017, 270018, 270019, 270020, 270021, 270022, 270023, 270024, 270025, 270026, 270027, 270028, 270030, 270032, 270033, 270034, 270035, 270036, 270038, 270039, 270040, 270041, 270042, 270043, 270044, 270045, 270046, 270047, 270048, 270049, 270050, 270051, 270052, 270053, 270054, 270055, 270056, 270057, 270058, 270060, 270061, 270062, 270063, 270064, 270065, 270066, 270067, 270068, 270069, 270070, 270072, 270074, 270075, 270076, 270077, 270078, 270079, 270080, 270081, 270082,270001, 270029, 270031, 270037, 270059, 270071, 270073, 270097, 270121, 270131, 270133, 270143, 270157, 270163, 270167, 270191, 270209, 270217, 270223, 270229, 270239, 270241, 270269, 270271, 270287))



 
replicas <- 20
datos<-data.frame(matrix(vector(), 0, 3))
datos1<-data.frame()
suppressMessages(library(doParallel))

  for(nucleo in 1:detectCores()){
    registerDoParallel(nucleo)
    npn <-  numeric()
    npd <-  numeric()
    npa<-  numeric()
    tn <-  numeric()
    tnd<-  numeric()
    tna <-  numeric()
    pn <-  numeric()
    pd <-  numeric()
    pa <-  numeric()
    
    for (r in 1:replicas) {
      npn <- c(npn, system.time(foreach(n = combinacionNP, .combine=c) %dopar% primo(n))[3]) #primos 25% menor a mayor
      npd <- c(npd, system.time(foreach(n = rev(combinacionNP), .combine=c) %dopar% primo(n))[3]) # primos 25% mayor a menor
      npa <- c(npa, system.time(foreach(n = sample(combinacionNP), .combine=c) %dopar% primo(n))[3]) # primos 25% aleatorio
      tn <- c(tn, system.time(foreach(n = combinacion, .combine=c) %dopar% primo(n))[3]) # 50p-50np menor a mayor
      tnd <- c(tnd, system.time(foreach(n = rev(combinacion), .combine=c) %dopar% primo(n))[3]) # 50p-50np menor a mayor
      tna <- c(tna, system.time(foreach(n = sample(combinacion), .combine=c) %dopar% primo(n))[3]) # 50p-50np aleatorio
      pn <- c(pn, system.time(foreach(n = combinacionP, .combine=c) %dopar% primo(n))[3]) #primos 75% mayor a menor
      pd <- c(pd, system.time(foreach(n = rev(combinacionP), .combine=c) %dopar% primo(n))[3]) #primos 75% mayor a menor
      pa <- c(pa, system.time(foreach(n = sample(combinacionP), .combine=c) %dopar% primo(n))[3]) #primos 75% aleatorio
      

}
datos<-rbind(datos, as.numeric(npn), as.numeric(npd), as.numeric(npa), as.numeric(tn), as.numeric(tnd), as.numeric(tna), as.numeric(pn), as.numeric(pd), as.numeric(pa))
stopImplicitCluster()
  }
for (fila in 1:(9*(detectCores()))){ 
  datos1 <- rbind(datos1, mean(as.numeric(datos[fila,])))
}
datos1<-as.matrix(datos1)
datos1<-t(matrix(datos1, nrow = 9, ncol = detectCores()))

datos1<-as.data.frame(datos1)
colnames(datos1)<-c("25%Primos Mayor a menor", "25%Primos Menor a mayor", "25%Primos Aleatorio", "50%Primos Menor a mayor  ", "50%Primos Mayor a menor", "50%Primos Aleatorio", "75%Primos Menor a mayor", "75%Primos Mayor a menor", "75%Primos Aleatorio")
rownames(datos1)<-c("1 núcleo", "2 núcleos", "3 núcleos", "4 núcleos")


png( filename = "teoría de colas.png", units = "px", height = 1600, width = 1600, res = 300)
barplot(as.matrix(t(datos1)), beside = T , col= rainbow(9) ,main = "Teoría de Colas", ylab = "Promedio de Tiempo", xlab ="núcleos lógicos", ylim = (c(0, max(as.matrix(t(datos1))))) )
legend("topright", c("25%Primos Mayor a menor", "25%Primos Menor a mayor", "25%Primos Aleatorio", "50%Primos Menor a mayor  ", "50%Primos Mayor a menor", "50%Primos Aleatorio", "75%Primos Menor a mayor", "75%Primos Mayor a menor", "75%Primos Aleatorio"), fill = rainbow(9), cex = 0.5)

graphics.off()