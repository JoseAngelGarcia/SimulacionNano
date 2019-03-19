g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- 3
paso <- 0.25
replicas <- 15
coordenadas <- data.frame("X"=0, "Y"=0, "T"=0, "R" = 0)

replica <- function(t, coor) {
  currX <- runif(1, low, high) 
  currY <- runif(1, low, high)
  bestX <- currX
  bestY <- currY
  for (tiempo in 1:t) {
    deltaX <- runif(1, 0, paso)
    deltaY <- runif(1, 0, paso)
    left <- currX - deltaX
    right <- currX + deltaX
    down <- currY - deltaY
    up <- currY + deltaY
    while(sum(c(left, right, down, up) < low) !=0 || sum(c(left, right, down, up) > high) != 0){
      currX <- runif(1, low, high) 
      currY <- runif(1, low, high)
      deltaX <- runif(1, 0, paso)
      deltaY <- runif(1, 0, paso)
      left <- currX - deltaX
      right <- currX + deltaX
      down <- currY - deltaY
      up <- currY + deltaY 
    }
    if (g(left, currY) > g(right, currY)) {
      currX <- left
    } else {
      currX <- right
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    if (g(currX, down) > g(currX, up)) {
      currY <- down
    } else {
      currY <- up
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    if (g(left, up) > g(right, up)) {
      currX <- left
      currY <- up
    } else {
      currX <- right
      currY <- up
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    if (g(left, down) > g(right, down)) {
      curryX <- left
      currY <- down
    } else {
      curryX <- right
      currY <- up
    }
    if (g(currX, currY) > g(bestX, bestY)) {
      bestX <- currX
      bestY <- currY
    }
    b = data.frame("X" = bestX, "Y" = bestY, "T" = tiempo, "R" = i)
    coor <- rbind(coor, b)
  }
  best = c(bestX, bestY)
  return(coor)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
valores <- seq(low, high, by = paso)
x <- rep(valores, each = length(valores))
y <- rep(valores, length(valores))
z <- foreach(i = x, j = y, .combine=c) %dopar% g(i,j)
resultados2 <- data.frame(x, y, z)
Pot <- 2
for (pot in Pot) {
  tmax <- 10^pot
  resultados <- foreach(i = 1:replicas, .combine="rbind") %dopar% replica(tmax, coordenadas)
  resultados <- data.frame(resultados)
}
stopImplicitCluster()
resultados <- resultados[!(resultados$T == 0),]
library(ggplot2)
for (i in 1:tmax) {
  sS <- subset(resultados, T == i)
  ggplot(resultados2, aes(x = x, y = y)) + geom_tile(aes(fill=z)) + ggtitle("Paso ")+
    scale_fill_gradient2(name = "",low = "azure2", mid = "cornflowerblue", high = "darkblue", midpoint=(min(z)+max(z))/2, breaks = c(seq(floor(min(z)), 0, by = 0.5))) +
    scale_x_continuous("", breaks = seq(low, high, by = 1)) + scale_y_continuous("", breaks = seq(low, high, by = 1)) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 20)) + geom_point(data = sS, aes(x= X, y= Y, color = as.factor(R)), size = 4, shape = 16, stroke = 2) + scale_color_hue(l=80, c=150, guide = FALSE) +
    theme_minimal(base_size = 14)
}
 