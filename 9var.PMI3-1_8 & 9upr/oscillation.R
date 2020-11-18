installed.packages("Cairo")
installed.packages("visualize")
library(Cairo)
library(Cairo)# аргументы rf_1 и rf_2 - это степени свободы df1 и df2. rn_1 - среднее, и rn_2 - стандартное отклонение
oscillation <- function(rf_1,rf_2,rn_1,rn_2) #функция
{
  set.seed(9)
  n <- 300 #число наблюдений
  x <- rf(n,rf_1,rf_2) #распределение Фишера
  e <- rnorm(n,rn_1,rn_2) #нормальное распределение
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
  min(x) # минимальное x
  R <- (max(x)-min(x)) #размах вариации
  mean(x) # среднее x
  Vx <- R/mean(x)*100 # коэффициент осцилляции 
  Vx
  max(y) # максимальное y
  min(y) # минимальное y
  R <- (max(y)-min(y)) #размах вариации
  mean(y) # среднее y
  Vy <- R/mean(y)*100 # коэффициент осцилляции 
  Vy
  qFrame <- data.frame(Perem = c("x","y"),Oscillation = c(Vx,Vy)) # Perem - чтобы было понятно по x или y коэффициент осцилляции. Vx коэффициент осцилляции x, Vy коэффициент осцилляции y
  qFrame
}