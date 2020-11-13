installed.packages("cairo")
installed.packages("visualize")
library("cairo")
library(visualize)
set.seed(9)
x <- rf(300,3,112)
e <- rnorm(300,0,14)
y <- 5+3*x+e
plot (x,y)
dev.copy(tiff, filename = "plot-ex5.png")#5 упражнение
dev.off()
############################################################
layout(matrix(2:1,1 , 2, byrow = T))
hist(x, col="peachpuff", border="black", prob = TRUE, xlab="x",ylab ="Density", main = "histogram of x")
lines(density(x), col="red", lwd =2)
curve(df(x,3, 112), col = "blue",lwd = 2, add = T)
hist(y, col="peachpuff", border="black", prob = TRUE, xlab="x", ylab ="Density", main = "histogram of y")
lines(density(y), col="red", lwd =2)
curve(dnorm(y,mean = mean (y), sd = sd(y)), col = "blue",lwd = 2, add = T, xname = "y") # 6 упражнение
############################################################
layout(matrix(1:1,1 , 1, byrow = T))
c2005 <- c(15806.5, 7077.1, 3026.1)
c2010 <- c(28300.9, 10097.4, 13556.5)
c2013 <- c(50236.5, 16401.8, 19329.2)
c2017 <- c(61885.8, 9761.8, 32808.0)
mat <- cbind(c2005, c2010, c2013, c2017)
mat
barplot(mat, col = "steelblue",
        xlab = "годы",
        ylab = "социально-культурные мероприятия",) #7 упражнение
dev.off()

