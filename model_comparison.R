# Open the .Rproj file in RStudio to automatically setwd()
# Otherwise set the working directory to the folder with this script in it.

library(randomForest)
library(party)

testing <- TRUE

seed <- 12345


if (testing) {
  output_file <- "./data/testing_output.csv"
  loops <- 1
  model_list <- c("crf", "rf")
  tree_list <- c(500)
  try_list <- c(5)
  proportion_list <- c(0.632)
} else {
  output_file <- "./data/simulation_output.csv"
  loops <- 20
  model_list <- c("crf", "rf")
  tree_list <- c(50, seq(100, 500, 100), 1000)
  try_list <- seq(3, 8, 1)
  proportion_list <- c(0.632, seq(0.3, 0.9, 0.05))
}

set.seed(seed)

# load the non-turk dataset
data <- read.csv("./data/Data NomTurk.csv", stringsAsFactors = F)
names(data)[1] <- "StroopEffect"

# remove missing rows
data <- na.omit(data)

# include the names of the important info
run_names <- c("model_id", "num_trees", "num_try", "sample_prop", "seed")
#run_info <- c(curr_model, num_trees, num_tries, curr_prop, seed)

# write names
tmp <- names(data)
tmp[1] <- "variance_accounted"
write(paste(c(run_names, tmp), collapse=", "), file=output_file, append=F)

for (loop in 1:loops) {
  for (num_tries in try_list) {
    for (num_trees in tree_list) {
      for (curr_prop in proportion_list) {
        for (curr_model in model_list) {
          
          if (curr_model == "rf") {
            # fit the random forest model
            forest_model <- randomForest(StroopEffect ~ ., 
                                         data = data, 
                                         importance = TRUE,
                                         ntree = num_trees,
                                         mtry = num_tries,
                                         replace = FALSE,
                                         sampsize = ceiling(curr_prop * nrow(data)))
            
            # estimate variable importance for rf
            variable_importance <- importance(forest_model, type=1)
          } else if (curr_model == "crf") {
            # fit the conditional forest
            forest_model <- cforest(StroopEffect ~ ., 
                                    data = data, 
                                    controls = cforest_control(teststat = "quad", 
                                                               testtype = "Univ", 
                                                               mincriterion = 0, 
                                                               ntree = num_trees,
                                                               mtry = num_tries,
                                                               replace = FALSE,
                                                               fraction = curr_prop))
            
            # estimate variable importance for crf
            variable_importance <- varimp(forest_model)
          }
          
          # extract predictions
          model_prediction <- predict(forest_model)
          
          # evaluate the model, pull out R^2
          variance_account <- cor(data$StroopEffect, model_prediction)^2
          
          # include the information for this run
          run_info <- c(curr_model, num_trees, num_tries, curr_prop, seed)
          
          # write output
          write(paste(c(run_info, variance_account, variable_importance), 
                      collapse=", "), 
                file=output_file, append=T)
        }    
      }
    }
  }
}

###############################################################################
### Rick trying holdout validation with a loop across many seeds           ####
### This is not quite k-folds                                              ####
###############################################################################

seeds <- 100

# initialize objects
variance_account1 <- NA
variance_account2 <- NA
variance_account3 <- NA

for (seed in 1:seeds){
  # Here's a simple method to randomize train and test samples. This
  # exact method is modified from one DataCamp uses, but there are dozens
  # of ways to code this same procedure.
  # Set seed for reproducibility
  set.seed(seed)
  rows <- sample(nrow(data)) #creates randomized vector same length as data
  data_randomized <- data[rows,] #randomizes df to index from 'rows'
  split <- round(nrow(data)*.80) #creates index to split the file into 4/5 1/5, rounded
  train <- data_randomized[1:split,] #first 4/5 to train
  test <- data_randomized[(split+1):nrow(data),] #remaining 1/5 to test
  
  ##### fit the random forest model
  forest_model <- randomForest(StroopEffect ~ ., 
                               data = train, 
                               importance = TRUE,
                               ntree = 500,
                               mtry = 5,
                               replace = FALSE,
                               sampsize = ceiling(.632 * nrow(train)))
  
  # estimate variable importance for rf
  variable_importance <- importance(forest_model, type=1)
  
  # extract predictions
  model_prediction <- predict(forest_model, newdata = test)
  
  # evaluate the model, pull out R^2
  variance_account1[seed] <- cor(test$StroopEffect, model_prediction)^2
  
  ####### fit the conditional forest
  forest_model <- cforest(StroopEffect ~ ., 
                          data = train, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = 0, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  
  # estimate variable importance for crf
  variable_importance <- varimp(forest_model)
  
  # extract predictions
  model_prediction <- predict(forest_model, newdata=test)
  
  # evaluate the model, pull out R^2
  variance_account2[seed] <- as.numeric(cor(test$StroopEffect, model_prediction)^2)
  
  ######## Compare to linear regression
  # Build the model predicting stroop from all other variables
  model1 <- lm(data=train,StroopEffect~.)
  summary(model1) # same model, so same results
  
  model_prediction <- predict(model1, newdata = test) # note the "test" dataset must have all the columns used in train
  
  # evaluate the model, pull out R^2
  variance_account3[seed] <- cor(test$StroopEffect, model_prediction)^2
}

# Average r squared across the trials for
# RF
mean(variance_account1)
# CRF
mean(variance_account2)
# Linear regression
mean(variance_account3)

merged_rsq_test <- data.frame(variance_account1,variance_account2,variance_account3)
names(merged_rsq_test) <- c("rf","crf","lm")
write.csv(merged_rsq_test, "onefifth_rsquared_test.csv")


###############################################################################
### Rick comparing variable importance between train and test set          ####
###                                                                        ####
###############################################################################

library(randomForest)
library(party)

# load the non-turk dataset
data <- read.csv("./data/Data NomTurk.csv", stringsAsFactors = F)
names(data)[1] <- "StroopEffect"

# remove missing rows
data <- na.omit(data)

seeds <- 10

# initialize objects
variance_account1 <- NA
variance_account2 <- NA
variance_account3 <- NA
variable_importance1 <- NA
variable_importance2 <- NA
variable_importance_test1 <- NA
variable_importance_test2 <- NA

for (seed in 1:seeds){
  # Here's a simple method to randomize train and test samples. This
  # exact method is modified from one DataCamp uses, but there are dozens
  # of ways to code this same procedure.
  # Set seed for reproducibility
  set.seed(seed)
  rows <- sample(nrow(data)) #creates randomized vector same length as data
  data_randomized <- data[rows,] #randomizes df to index from 'rows'
  split <- round(nrow(data)*.80) #creates index to split the file into 4/5 1/5, rounded
  train <- data_randomized[1:split,] #first 4/5 to train
  test <- data_randomized[(split+1):nrow(data),] #remaining 1/5 to test
  
  ##### fit the random forest model to train
  forest_model <- randomForest(StroopEffect ~ ., 
                               data = train, 
                               importance = TRUE,
                               ntree = 500,
                               mtry = 5,
                               replace = FALSE,
                               sampsize = ceiling(.632 * nrow(train)))
  
  ##### fit the random forest model to test
  forest_model_test <- randomForest(StroopEffect ~ ., 
                               data = test, 
                               importance = TRUE,
                               ntree = 500,
                               mtry = 5,
                               replace = FALSE,
                               sampsize = ceiling(.632 * nrow(test)))
  
  # estimate variable importance for rf
  variable_importance1[seed] <- list(importance(forest_model, type=1))
  variable_importance_test1[seed] <- list(importance(forest_model_test, type=1))
    
  # extract predictions
  model_prediction <- predict(forest_model, newdata = test)
  
  # evaluate the model, pull out R^2
  variance_account1[seed] <- cor(test$StroopEffect, model_prediction)^2
  
  ####### fit the conditional forest train
  forest_model <- cforest(StroopEffect ~ ., 
                          data = train, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = 0, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  ####### fit the conditional forest test
  forest_model_test <- cforest(StroopEffect ~ ., 
                          data = test, 
                          controls = cforest_control(teststat = "quad", 
                                                     testtype = "Univ", 
                                                     mincriterion = 0, 
                                                     ntree = 500,
                                                     mtry = 5,
                                                     replace = FALSE,
                                                     fraction = 0.632))
  
  # estimate variable importance for crf
  variable_importance2[seed] <- list(varimp(forest_model))
  variable_importance_test2[seed] <- list(varimp(forest_model_test))
  
  # extract predictions
  model_prediction <- predict(forest_model, newdata=test)
  
  # evaluate the model, pull out R^2
  variance_account2[seed] <- as.numeric(cor(test$StroopEffect, model_prediction)^2)
  
  ######## Compare to linear regression
  # Build the model predicting stroop from all other variables
  model1 <- lm(data=train,StroopEffect~.)
  summary(model1) # same model, so same results
  
  model_prediction <- predict(model1, newdata = test) # note the "test" dataset must have all the columns used in train
  
  # evaluate the model, pull out R^2
  variance_account3[seed] <- cor(test$StroopEffect, model_prediction)^2
}

# Average r squared across the trials for
# RF
mean(variance_account1)
# CRF
mean(variance_account2)
# Linear regression
mean(variance_account3)

merged_rsq_test <- data.frame(variance_account1,variance_account2,variance_account3)
names(merged_rsq_test) <- c("rf","crf","lm")
write.csv(merged_rsq_test, "onefifth_rsquared_test.csv")

vi1 <- as.data.frame(variable_importance1)
vi_test1 <- as.data.frame(variable_importance_test1)
vi2 <- as.data.frame(variable_importance2)
vi_test2 <- as.data.frame(variable_importance_test2)

names(vi1) <- paste("seed", as.character(rep(1:seed)), sep="")
names(vi_test1) <- paste("seed", as.character(rep(1:seed)), sep="")
names(vi2) <- paste("seed", as.character(rep(1:seed)), sep="")
names(vi_test2) <- paste("seed", as.character(rep(1:seed)), sep="")

rf_cor <- NA
crf_cor <- NA
for(seed in 1:seeds){
train_test_cor_rf <- cor.test(vi1[[seed]], vi_test1[[seed]], method="spearman")
train_test_cor_crf <- cor.test(vi2[[seed]], vi_test2[[seed]], method="spearman")
rf_cor[seed] <- train_test_cor_rf$estimate
crf_cor[seed] <- train_test_cor_crf$estimate
}

mean(rf_cor)
mean(crf_cor)
