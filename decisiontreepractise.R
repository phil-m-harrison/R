install.packages("rpart")
install.packages("randomForest")
install.packages("rattle")
library(rpart)
library(randomForest)
library(rattle)

data("iris")

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

NormalisedData<-as.data.frame(lapply(iris[,-5], FeatureScaling))
NormalisedData$Species<-iris$Species
tree <- rpart(Species ~ . , data=NormalisedData)

print(tree$cptable)

forest <- randomForest(Species ~ . , data=NormalisedData)

print(forest)

fancyRpartPlot(tree)


Glass<-read.csv("C:/Users/Admin/Documents/glass.csv", header=FALSE)
GlassnoID<-Glass[,-1]
NormalisedGlass<-as.data.frame(lapply(GlassnoID[,-10], FeatureScaling))
NormalisedGlass$Type<-as.factor(Glass$V11)

tree <- rpart(Type ~ . , data=NormalisedGlass)

print(tree$cptable)

forest <- randomForest(Type ~ . , data=NormalisedGlass)

print(forest)

fancyRpartPlot(tree)
