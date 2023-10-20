# Source the utility function files
source("utils/VisualizationFunctions.R")
source("utils/PreprocessingFunctions.R")

data_file <- "data/data.csv"
preprocessed_output_dir <- "preprocessed_data"
visualize_output_dir <- "visualization"

#################################### Main ########################################
if (file.exists(data_file)) {

  cat("####################################### PRE-PROCESSING STEP ######################################\n\n")

  # Read data
  data <- read.csv(data_file)

  # Call Preprocessing function
  data <- preprocess_data(data, preprocessed_output_dir)

  cat("################################ PRE-PROCESSING STEP COMPLETED ###################################\n\n")


  cat("####################################### DATA-VISUALIZATION STEP ##################################\n\n")

  # Pass Preprocessed Data to visualization function
  visualize_data(data, visualize_output_dir)

  cat("################################ DATA-VISUALIZATION STEP COMPLETED ###############################\n\n")

  cat("############# NOW NEXT YOU CAN RUN THE INDIVIDUAL MODELS TO TEST FOR RESULT 👋 ###################\n\n")


} else {
  cat("data.csv doesn't exist, please make sure it exists inside /data\n")
}
##################################################################################
