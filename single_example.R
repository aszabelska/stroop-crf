### Stand-alone script demonstrating the high R2 in training, low R2 in test.
library(randomForest)
library(party)

# setwd() if not using the .rproj file

set.seed(12345)

# load the non-turk dataset
data <- read.csv("./data/Data NomTurk.csv", stringsAsFactors = F)
names(data)[1] <- "StroopEffect"

data <- na.omit(data)

# Generate train/test
rows <- sample(nrow(data)) #creates randomized vector same length as data
data_randomized <- data[rows,] #randomizes df to index from 'rows'
split <- round(nrow(data)*.80) #creates index to split the file into 4/5 1/5, rounded
train <- data_randomized[1:split,] #first 4/5 to train
test <- data_randomized[(split+1):nrow(data),] #remaining 1/5 to test

# fit the conditional forest
forest_model <- cforest(StroopEffect ~ ., 
                        data = train, 
                        controls = cforest_control(teststat = "quad", 
                                                   testtype = "Univ", 
                                                   mincriterion = 0.95, 
                                                   ntree = 500,
                                                   mtry = 5,
                                                   replace = FALSE,
                                                   fraction = 0.632))

# Extract predictions
model_prediction_train <- predict(forest_model) #oob
model_prediction_test <- predict(forest_model, newdata = test)

# Evaluate
cor(train$StroopEffect, model_prediction_train)^2
cor(test$StroopEffect, model_prediction_test)^2

# EDIT: Was overfitting. As per Stella's email, we need to change the cforest 'mincriterion' argument to .95 (was previously 0 in all versions)