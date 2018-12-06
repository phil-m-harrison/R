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


# Load data in using cleaned data from housing prices forest tree .r file
# Split the raw data into suitable groupings
trainingvalues <- cleaneddata

testvalues<- cleanedtestdata
#testprices <- data$test$y

#make all variables integers
trainingvalues[,] <- lapply(trainingvalues[,], as.numeric)
testvalues[,] <- lapply(testvalues[,], as.numeric)
#put data sets in matrices
trainingvalues <- data.matrix(trainingvalues)
testvalues <- data.matrix(testvalues)


#separate house prices from training set
trainingprices <- trainingvalues[,39]
trainingprices<-t(trainingprices)
trainingvalues<-trainingvalues[,-39]

#normalize both matrices
trainingvalues<-normalize(trainingvalues, axis=1)
testvalues<-normalize(testvalues, axis = 1)
trainingprices<-normalize(trainingprices, axis=1)

# Turns the integers into classes suitable for Keras' processing
#trainingprices <- to_categorical(trainingprices, 1460)
#testprices <- to_categorical(y_test, 10)

## Build the Neural Network

# Base network creation
neural_network <- keras_model_sequential()

# Add layers
neural_network %>% 
  layer_dense(units = 76, activation = 'sigmoid', input_shape = c(38)) %>% 
  layer_dropout(rate = 0.45) %>% 
  layer_dense(units = 38, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 19, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 9, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 4, activation = 'sigmoid') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile

neural_network %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_rmsprop(lr = 0.001),
  metrics = metric_binary_accuracy
)

# Train the model

history <- neural_network %>% fit(
  trainingvalues, trainingprices, 
  epochs = 20, batch_size = 8, 
  validation_split = 0.1
)


# Generate predictions

neural_network %>% predict_classes(testvalues)
