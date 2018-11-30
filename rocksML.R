data("rock")
rocks<-as.data.frame(rock)

rocksnoPERM<-as.data.frame(rocks[,-4])

FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
g<-runif(nrow(rocks))
rocksr <- rocks[order(g),] 
NormalisedRocks<-as.data.frame(lapply(rocksr, FeatureScaling))
NormalisedRocks$perm<-NULL
NormalisedRocks$perm<-rocksr$perm

trainingdata<-NormalisedRocks[1:33,]
testingdata<-NormalisedRocks[34:48,]

#SVM
svm.model <- svm(as.factor(perm) ~ ., data = trainingdata, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testingdata[,-4])

table(pred = svm.pred, true = testingdata[,4])

#NB

nb.model<-naiveBayes(as.factor(perm)~., data=trainingdata, method="class")
nb.pred<-predict(nb.model,testingdata)

table(pred=nb.pred, true=testingdata[,4])

qplot(area, peri, data=rock, colour=perm)

install.packages('rgl')

library(rgl)

plot3d(rock$area, rock$peri, rock$shape, type="s", size=1, col=as.integer(rock$perm))
