install.packages("class")
install.packages("ggplot2")
library(class)
library(ggplot2)

RawData<-read.csv("C:/Users/Admin/Downloads/water-treatment.csv", stringsAsFactors = FALSE, header = FALSE)

names(RawData) <- c("Date","Q-E","ZN-E","PH-E","DBO-E","DQO-E","SS-E","SSV-E","SED-E","COND-E","PH-P","DBO-P",
                    "SS-P","SSV-P","SED-P","COND-P","PH-D","DBO-D","DQO-P","SS-D","SSV-D","SED-D","COND-D","PH-S",
                    "DBO-S","DQO-S","SS-S","SSV-S","SED-S","COND-S","RD-DBO-P","RD-SS-P","RD-SED-P","RD-DBO-S",
                    "RD-DQO-S","RD-DBO-G","RD-DQO-G","RD-SS-G","RD-SED-G")

datanoID<-RawData[,-1]
#datanoDiagnosis<-datanoID[,-1]
FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
datatobenormalised<-as.data.frame(datanoID$'COND-P')
datatobenormalised$'COND-D'<-datanoID$'COND-D'
NormalisedData<-as.data.frame(lapply(datatobenormalised, FeatureScaling))

ggplot(NormalisedData, aes(x=datanoID..COND.P.,y=COND.D)) + 
  geom_density2d() +
  ggtitle("Input conductivity into primary plant vs secondary plant") + 
  xlab("Primary Plant input conductivity") +
  ylab("Secondary Plant input conductivity")