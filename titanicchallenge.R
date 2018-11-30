testdata<-read.csv("C:/Users/Admin/Documents/test.csv")
trainingdata<-read.csv("C:/Users/Admin/Documents/train.csv")
testtrueresults<-read.csv("C:/Users/Admin/Documents/gender_submission.csv")

fixedtrain<-trainingdata[,-1]
fixedtrain<-fixedtrain[,-3]
fixedtrain<-fixedtrain[,-7]
fixedtrain<-fixedtrain[,-7]
fixedtrain<-fixedtrain[,-7]

fixedtrain$Sex<-as.integer(fixedtrain$Sex)
fixedtrain$Pclass<-as.integer(fixedtrain$Pclass)
fixedtrain$Embarked<-as.integer(fixedtrain$Embarked)
str(fixedtrain)


fixedtrain[is.na(fixedtrain)] <- 30


fixedtest<-testdata[,-1]
fixedtest<-fixedtest[,-2]
fixedtest<-fixedtest[,-6]
fixedtest<-fixedtest[,-6]
fixedtest<-fixedtest[,-6]

fixedtest$Sex<-as.integer(fixedtest$Sex)
fixedtest$Pclass<-as.integer(fixedtest$Pclass)
fixedtest$Embarked<-as.integer(fixedtest$Embarked)
str(fixedtest)

fixedtest[is.na(fixedtest)] <- 30
FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
Normalisedtrain<-as.data.frame(lapply(fixedtrain, FeatureScaling))
Normalisedtest<-as.data.frame(lapply(fixedtest, FeatureScaling))
Normalisedtrain$Survived<-as.factor(Normalisedtrain$Survived)


svm.model <- svm(Survived ~ ., data = Normalisedtrain, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, Normalisedtest)

table(pred = svm.pred, true = testtrueresults[,2])

svm.model

Normalisedtest$Survived<-svm.pred

predictions<-as.data.frame(testtrueresults[,1])
predictions$Survived<-svm.pred
write.csv(predictions,"predictions.csv")


