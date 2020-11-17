library(Cairo)
raspf <- function(rf_1,rf_2,rn_1,rn_2) #функция
  {
set.seed(9)
n <- 300
x <- rf(n,rf_1,rf_2)
e <- rnorm(n,rn_1,rn_2)
y <- 5 + 3*x + e
layout(matrix(c(2,1,1,2,1,1,0,3,3),3,3, byrow = T))
plot(x,y)
boxplot(x)
boxplot(e,horizontal = T)
dev.copy(tiff, filename = "plot-ex8.png") #сохранение графика, как картинку
dev.off()
  }

