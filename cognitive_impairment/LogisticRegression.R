# PLEASE MAKE SURE TO INSTALL PACKAGES IF YOU HAVIENT
# install.packages("tidymodels")
# install.packages("caret")

suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(caret))

# Function to save the trained random forest model
save_lr_model <- function(trained_model, model_dir) {
  # Create the directory if it doesn't exist within the "trained_model" directory
  directory <- file.path("trained_model", model_dir)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Generate a unique model filename
  model_filename <- file.path(directory, paste0("trained_lr_model.rds"))
  
  # Save the model
  saveRDS(trained_model, model_filename)
}

# Function to load the trained random forest model
load_lr_model <- function(model_filename) {
  if (file.exists(model_filename)) {
    # Load the saved random forest model
    loaded_model <- readRDS(model_filename)
    return(loaded_model)
  } else {
    cat("Model file not found. Please provide a valid model filename.")
    return(NULL)
  }
}

########################################### Model training function ####################################################

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

  # Train the logistic regression model
  cat("Training the model...\n\n")
  model <- train(MMSE_class_2 ~ ., data = training_data, method = "glm", family = binomial)

  cat("Training completed \n\n")

    # Save the trained random forest 
  cat("Saving the trained model...\n\n")
  save_lr_model(model, "lr_model")
  cat("Saved model successfully at trained_mode/lr_model/trained_lr_model.rds \n\n")

  # Make predictions on the testing set
  predictions <- predict(model, newdata = testing_data)

  # Evaluate the model's performance (you can use different metrics)
  confusion_matrix <- confusionMatrix(predictions, testing_data$MMSE_class_2)
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

  cat("################################# TRAINING-LR-MODEl COMPLETED ##################################\n\n")

  cat("################################# PREDICTION WITH LR-MODEL STEP #################################\n\n")

  # Load the test dataset
  test_data <- read.csv('data/dementia_test.csv')

  # Load the saved logistic regression model
  loaded_lr_model <- load_lr_model("trained_model/lr_model/trained_lr_model.rds")

  # Check if the model was loaded successfully
  if (!is.null(loaded_lr_model)) {
    # Prediction
    predictions <- make_predictions(test_data, loaded_lr_model)
    predicted_class <- as.character(predictions)
    
    if (predicted_class == "Cognitively Impaired") {
      cat("Hey, this person might need some help, [Cognitively Impaired]\n\n")
    } else {
      cat("Model's prediction of the patient record: ", predicted_class, "\n\n")
    }
  } else {
    cat("Logistic regression model not loaded. Please check the model file path.\n\n")
  }
  
  cat("######################################### BYE BYE 👋 ###########################################\n\n")

} else {
  cat("preprocessed_data.csv doesn't exist, please run pre-processing.r file \n")
}

#######################################################################################################
