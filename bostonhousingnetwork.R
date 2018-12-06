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
data <- dataset_boston_housing()

# Split the raw data into suitable groupings
# x = Input (pixel values), y = Output (number categories)
x_train <- data$train$x
y_train <- data$train$y
x_test <- data$test$x
y_test <- data$test$y

#normalise via feature scaling as data frames
x_train<-normalize(x_train)
x_test<-normalize(x_test)
# Reshape into matrices
x_train<-data.matrix(x_train)
x_test<-data.matrix(x_test)

## Build the Neural Network

# Base network creation - used as scaffolding to attach other layers
neural_network <- keras_model_sequential()

# Add layers with the pipe (%>%) operator
neural_network %>% 
  layer_dense(units = 26, activation = 'relu', input_shape = c(13)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 13, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 6, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1, activation = 'linear')

# Compile the model

neural_network %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = metric_binary_accuracy
)

# Train the model, currently errors due to 'Evaluation error: arguments imply differing number of rows: 1, 0.'

history <- neural_network %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 6, 
  validation_split = 0.2
)

# View the history
# Only needed if Keras interactive graph doesn't load

plot(history)

# Evaluate performance

neural_network %>% evaluate(x_test, y_test)

# Generate predictions

neural_network %>% predict_classes(x_test)

table(pred=neural_network %>% predict_classes(x_test), actual=data$test$y)
