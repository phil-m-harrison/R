data("iris")
irisdf<-as.data.frame(iris)

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
g<-runif(nrow(iris))
irisr <- iris[order(g),] 
#irisr<-as.data.frame(lapply(irisr[,-5], FeatureScaling))
#g<-runif(nrow(iris))
#iris2 <- iris[order(g),] 
#irisr$Species<-iris2[,5]
m4 <- naiveBayes(as.factor(Species)~., data=irisr[1:75,], method="class")
p4<-predict(m4, irisr[76:150,]) 
p4
table(actual=irisr[76:150,5], predicted=p4)
qplot(Sepal.Length,Petal.Length, data=irisr[1:75,], colour=Species)
qplot(Sepal.Length,Petal.Length, data=irisr[76:150,], colour=Species)
