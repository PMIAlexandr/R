installed.packages("Cairo")
installed.packages("visualize")
library(Cairo)
library(Cairo)
oscillation <- function(rf_1,rf_2,rn_1,rn_2) #функция
{
  set.seed(9)
  n <- 300
  x <- rf(n,rf_1,rf_2)
  e <- rnorm(n,rn_1,rn_2)
  y <- 5 + 3*x + e
  layout(matrix(2:1,1 , 2, byrow = T))
  hist(x, col="peachpuff", border="black", prob = TRUE, xlab="x",ylab ="Density", main = "histogram of x", ylim=c(0,0.8)) #histogram of x
  lines(density(x), col="red", lwd =2) # цвет и толщина линии
  curve(df(x,3, 112), col = "blue",lwd = 2, add = T)
  hist(e, col="peachpuff", border="black", prob = TRUE, xlab="x", ylab ="Density", main = "histogram of e") #histogram of e
  lines(density(e), col="red", lwd =2) # цвет и толщина линии
  curve(dnorm(e,mean = mean (e), sd = sd(e)), col = "blue",lwd = 2, add = T, xname = "e")
  dev.copy(tiff, filename = "plot-ex9.png")# сохранение графика, как картинку
  dev.off() # 9 упражнение
  max(x) # максимальное x
  min(x) # минимальное X
  R <- (max(x)-min(x))
  mean(x) # среднее x
  Vx <- R/mean(x)*100 # коэффициент осцилляции 
  Vx
  max(y) # максимальное Y
  min(y) # минимальное X
  R <- (max(y)-min(y))
  mean(y) # среднее y
  Vy <- R/mean(y)*100 # коэффициент осцилляции 
  Vy
  qFrame <- data.frame(Perem = c("x","y"),Oscillation = c(Vx,Vy))
  qFrame
}
