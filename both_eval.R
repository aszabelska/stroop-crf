#!/usr/bin/env Rscript
library(randomForest)
library(party)

testing <- TRUE

seed <- 12345


if (testing) {
  output_file <- "testing_output.csv"
  loops <- 1
  model_list <- c("crf", "rf")
  tree_list <- c(500)
  try_list <- c(5)
  proportion_list <- c(0.632)
} else {
  output_file <- "simulation_output.csv"
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
                                                               mincriterion = 0.95, 
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


