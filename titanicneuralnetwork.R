## Installation

# install latest miniconda
# install latest RTools.exe
# install latest R version

install.packages("devtools")
library(devtools)
install.packages("keras")
library(keras)

# pip install tensorflow
# pip install keras

## Data Wrangling

# Load the data from the website
titanictestdata<-read.csv("C:/Users/Admin/Documents/test.csv")
titanictrainingdata<-read.csv("C:/Users/Admin/Documents/train.csv")
testtrueresults<-read.csv("C:/Users/Admin/Documents/gender_submission.csv")

# Split the raw data into suitable groupings
# x = Input (pixel values), y = Output (number categories)
titanictrainingvalues <- titanictrainingdata[,-2]
titanictrainingsurvived <- titanictrainingdata[,2]
titanictestvalues<- titanictestdata
testprices <- testtrueresults[,2]

#clean training data set
titanictrainingvalues<-titanictrainingvalues[,-1]
titanictrainingvalues<-titanictrainingvalues[,-2]
titanictrainingvalues<-titanictrainingvalues[,-6]
titanictrainingvalues<-titanictrainingvalues[,-6]
titanictrainingvalues<-titanictrainingvalues[,-6]

titanictrainingvalues$Sex<-as.integer(titanictrainingvalues$Sex)
titanictrainingvalues$Pclass<-as.integer(titanictrainingvalues$Pclass)
titanictrainingvalues$Embarked<-as.integer(titanictrainingvalues$Embarked)

titanictrainingvalues[is.na(titanictrainingvalues)] <- 30

#clean test data set
titanictestvalues<-titanictestdata[,-1]
titanictestvalues<-titanictestvalues[,-2]
titanictestvalues<-titanictestvalues[,-6]
titanictestvalues<-titanictestvalues[,-6]
titanictestvalues<-titanictestvalues[,-6]

titanictestvalues$Sex<-as.integer(titanictestvalues$Sex)
titanictestvalues$Pclass<-as.integer(titanictestvalues$Pclass)
titanictestvalues$Embarked<-as.integer(titanictestvalues$Embarked)

titanictestvalues[is.na(titanictestvalues)] <- 30

#normalise all values
FeatureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) }
titanictrainingvalues<-as.data.frame(lapply(titanictrainingvalues, FeatureScaling))
titanictestvalues<-as.data.frame(lapply(titanictestvalues, FeatureScaling))
# Reshape into matrices
titanictrainingvalues <- data.matrix(titanictrainingvalues, rownames.force = NA)
titanictestvalues <- data.matrix(titanictestvalues, rownames.force = NA)

# Turns the integers into classes suitable for Keras' processing
titanictrainingsurvived <- to_categorical(titanictrainingsurvived, 2)
testprices <- to_categorical(testprices, 2)

## Build the Neural Network

# Base network creation - used as scaffolding to attach other layers
neural_network <- keras_model_sequential()

# Add layers with the pipe (%>%) operator
neural_network %>% 
  layer_dense(units = 18, activation = 'sigmoid', input_shape = c(6)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 9, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 4, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')

# Compile the model

neural_network %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Train the model, recording performance in a variable

history <- neural_network %>% fit(
  titanictrainingvalues, titanictrainingsurvived, 
  epochs = 100, batch_size = 3, 
  validation_split = 0.2
)

# View the history
# Only needed if Keras interactive graph doesn't load

plot(history)

# Evaluate performance

neural_network %>% evaluate(titanictestvalues, testprices)

# Generate predictions

x<-neural_network %>% predict_classes(titanictestvalues)
testprices<-testtrueresults[,2]
table(pred = x, true = testprices)
write.csv(x, "neuraltitanic2.csv")
