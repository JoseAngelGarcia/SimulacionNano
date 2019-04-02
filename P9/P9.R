n <- 50

p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n)) #m agrega masa
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
mmax <- max(p$m)
mmin <- min(p$m)
p$m <- (p$m - mmin) / (mmax - mmin)
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                   col = colores)))
graphics.off()
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  fxm <- 0
  fym <- 0
  fxt <- 0
  fyt <-0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dirm <- (-1)^(1 + 1 * (mi < mj)) 
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    factorm <- dirm * mi*mj / (dx^2 + dy^2)  
    fx <- (fx - dx * factor) 
    fy <- (fy - dy * factor) 
    fxm <- (fxm - dx * factorm) 
    fym <- (fym - dy * factorm)
    fxt <- fx + fxm 
    fyt <- fy + fym 
  }
  return(c(fxt, fyt))
}

fuerzam <- function(i) { #Solo regresa fuerzas calculadas con masa
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  fxm <- 0
  fym <- 0
  fxt <- 0
  fyt <-0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dirm <- (-1)^(1 + 1 * (mi < mj))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factorm <- dirm * mi*mj / ((dx^2 + dy^2) + eps)
    fxm <- (fxm - dx * factorm) 
    fym <- (fym - dy * factorm)
    fxt <- fx + fxm
    fyt <- fy + fym
  }
  return(c(fxt, fyt))
}

fuerzac <- function(i) { #Solo regresa fuerzas calculadas con carga
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  fxm <- 0
  fym <- 0
  fxt <- 0
  fyt <-0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dirm <- (-1)^(1 + 1 * (mi < mj))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- (fx - dx * factor) 
    fy <- (fy - dy * factor) 
    fxt <- fx + fxm
    fyt <- fy + fym
  }
  return(c(fxt, fyt))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
tmax <- 100
deltat <- c()
deltam <- c()
deltac <- c()
for (iter in 1:tmax) {
  fm <- foreach(i = 1:n, .combine=c) %dopar% fuerzam(i) 
  fc <- foreach(i = 1:n, .combine=c) %dopar% fuerzac(i) 
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i) 
  delta <- 0.02 / max(abs(f)) 
  deltam <- 0.02 / max(abs(fm)) 
  deltac <- 0.02 / max(abs(fc))
    deltat <- c(deltat,delta) 
  deltamc <- c(deltamc,deltam)
  deltacc <- c(deltacc,deltac) 
 
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  
}
stopImplicitCluster()

library(ggplot2)
a <- data.frame(iter=seq(1,100,1), t=deltat, dm=deltamdata, dc=deltacdata)
gg <- ggplot(a, aes(x=iter,color=d))
gg + geom_line(aes(y=d), color="yellow", size=1) + 
geom_line(aes(y=dm), color="black", size=1) + 
geom_line(aes(y=dc), color="deepskyblue", size=1) + 
scale_color_discrete(name = "Fuerzas", labels = c("yellow", "black", "deepskyblue")) +
ylab("Velocidad") + 
xlab("Paso") +
theme_light()


