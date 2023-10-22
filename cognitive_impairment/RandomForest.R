# PLEASE MAKE SURE TO INSTALL PACKAGES IF YOU HAVIENT
# install.packages("randomForest")
# install.packages("readr")
# install.packages("tidymodels")
# install.packages("caret")
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(caret))

# Function to save the trained random forest model
save_rf_model <- function(trained_model, model_dir) {
  # Create the directory if it doesn't exist within the "trained_model" directory
  directory <- file.path("trained_model", model_dir)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Generate a unique model filename
  model_filename <- file.path(directory, paste0("trained_rf_model.rds"))
  
  # Save the model
  saveRDS(trained_model, model_filename)
}

# Function to load the trained random forest model
load_rf_model <- function(model_filename) {
  if (file.exists(model_filename)) {
    # Load the saved random forest model
    loaded_model <- readRDS(model_filename)
    return(loaded_model)
  } else {
    cat("Model file not found. Please provide a valid model filename.")
    return(NULL)
  }
}

########################################### Model training function ###################################

train_model <- function(data) {
  # Set the seed for reproducibility
  set.seed(123)

  # Convert MMSE_class_2 to factor with consistent levels
  data$MMSE_class_2 <- factor(data$MMSE_class_2)
  
  # Split the data into a training set (80%) and a testing set (20%)
  cat("Spliting the dataset into training & testing...\n\n")
  splitIndex <- createDataPartition(data$MMSE_class_2, p = 0.8, list = FALSE)
  training_data <- data[splitIndex, ]
  testing_data <- data[-splitIndex, ]
  
  # Train the Random Forest model
  cat("Training the model...\n\n")
  model <- randomForest(MMSE_class_2 ~ ., data = training_data)
  cat("Training completed \n\n")

  # Save the trained random forest 
  cat("Saving the trained model...\n\n")
  save_rf_model(model, "rf_model")
  cat("Saved model successfully at trained_mode/rf_model/trained_rf_model \n\n")


  # Use the trained model to make predictions on the testing data
  predictions <- predict(model, newdata = testing_data)
  
  # Evaluate the model's performance
  actual_values <- testing_data$MMSE_class_2
  confusion_matrix <- confusionMatrix(predictions, actual_values)
  print("Confusion Matrix: ")
  print(confusion_matrix)

  # Extract and print F1-score
  f1_score <- confusion_matrix$byClass["F1"]
  cat("F1-Score:", f1_score, "\n\n")
}

#######################################################################################################


###################################### Prediction function ############################################

make_predictions <- function(test_data, trained_model) {
  # Use the trained model to make predictions on new data
  predictions <- predict(trained_model, newdata = test_data)
  return (predictions)
}

########################################################################################################

####################################### Main script ####################################################

if (file.exists("preprocessed_data/preprocessed_data.csv")) {

  cat("################################# TRAINING-RF-MODEl STEP #######################################\n\n")
  
  data <- read.csv("preprocessed_data/preprocessed_data.csv")
  
  # Model training
  train_model(data)

  cat("################################# TRAINING-RF-MODEl COMPLETED ##################################\n\n")

  cat("################################# PREDICTION WITH RF-MODEL STEP #################################\n\n")

  # Load the test dataset
  test_data <- read.csv('data/dementia_test.csv')

   # Load the saved logistic regression model
  loaded_rf_model <- load_rf_model("trained_model/lr_model/trained_lr_model.rds")
  
  # Check if the model was loaded successfully
  if (!is.null(loaded_rf_model)) {
    # Prediction
    predictions <- make_predictions(test_data, loaded_rf_model)
    predicted_class <- as.character(predictions)
    
    cat("Model's prediction of the patient record: ", predicted_class, "\n\n")
  } else {
    cat("Logistic regression model not loaded. Please check the model file path.\n\n")
  }

  cat("######################################### BYE BYE 👋 ###########################################\n\n")

} else {
  cat("preprocessed_data.csv doesn't exist, please run pre-processing.r file \n")
}

#######################################################################################################
