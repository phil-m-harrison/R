library(class)
library(ggplot2)
library(e1071)
install.packages("mlbench")
Glass<-read.csv("C:/Users/Admin/Documents/glass.csv", header=FALSE)
#names(rawglass)<-c("ID","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type")

#datanoID<-rawglass[,-1]
#datanoType<-datanoID[,1:9]
data(Glass, package="mlbench")
FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }

NormalisedData<-as.data.frame(lapply(Glass, FeatureScaling))

index<-1:nrow(Glass)

data_index<-sample(index, trunc(length(index)/3))

Data_training<-Glass[-data_index,]

Data_test<-Glass[data_index,]

svm.model <- svm(Type ~ ., data = Data_training, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, Data_test[,-10])

table(pred = svm.pred, true = Data_test[,10])

