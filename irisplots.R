plot(iris$Petal.Length ~ iris$Petal.Width, pch=c(2,3,4)[iris$Species], col=c("red","black","blue")[iris$Species])
legend("topleft", legend=levels(iris$Species), col=c("red","black","blue"), pch=c(2,3,4))
boxplot(iris$Sepal.Width ~ iris$Species)
