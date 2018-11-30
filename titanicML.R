library(class)
library(ggplot2)
library(e1071)

data("Titanic")
Titanic_df<-as.data.frame(Titanic)
repeating_sequence=rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq)
Titanic_dataset<-Titanic_df[repeating_sequence,]
Titanic_dataset$Freq<-NULL

Naive_Bayes_Model<-naiveBayes(Survived ~., data=Titanic_dataset)
#m<-predict(Naive_Bayes_Model, as.data.frame(Titanic))

NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
table(NB_Predictions,Titanic_dataset$Survived)


svm.model <- svm(Survived ~ ., data = Titanic_dataset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, Titanic_dataset[,-4])

table(pred = svm.pred, true = Titanic_dataset[,4])

