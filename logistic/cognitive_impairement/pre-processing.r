library(caret)
library(dplyr)
library(ggplot2)


# Preprocessing function
preprocess_data <- function(data) {
  cat("Pre-processing & cleaning your dataset, please wait...", "\n")

  ############################################### Pre-processing Start ##############################################
  # Checking if data contains "?" -> Fill with NA
  data[data == "?"] <- NA

  # Converting the variable types
  data$Age <- as.numeric(data$Age) # [Age] : converting from integer to num
  data$MNAa_tot <- as.numeric(data$MNAa_tot) # [MNAa_tot]: converting from integer to num
  data$Education_ID <- as.factor(data$Education_ID) # Education_ID is considered factor (0, 1, 2, 3, 4)
  data$Mobility <- as.factor(data$Mobility) # Mobilit is considered factor (0, 1, 2, 3, 4)
  data$MNAa_q3 <- as.factor(data$MNAa_q3) # MNAa_q3 is considered factor (0, 1, 2)
  data$Endocrine.Disease_Hyperlipidaemia <- as.factor(data$Endocrine.Disease_Hyperlipidaemia) # Endocrine.Disease_Hyperlipidaemia is considered factor (0, 1)

  # Improving the readability of the output (result)
  data$MMSE_class_2 <- ifelse(test = data$MMSE_class_2 == 0, yes = "Cognitively Healthy", no = "Cognitively Impaired")
  data$MMSE_class_2 <- as.factor(data$MMSE_class_2)
  
  
  # Group data by Age and count the occurrences
  age_counts <- data %>%
    group_by(Age) %>%
    summarise(count = n())
  
  # Save the summarized age counts to a text file
  write.table(age_counts, "age_counts.txt", quote = FALSE, row.names = FALSE, col.names = TRUE)
  

  ################################################ Pre-procesing Completed #################################################




  ################################################# Data Summary ###########################################################
  # Print the total number of rows in the data frame
  cat("Total dataset: ", nrow(data), "\n")

  # Check for missing values in the entire dataset
  missing_values <- sum(is.na(data))
  cat("Number of missing values in the dataset:", missing_values, "\n")

  ############################################## Data Summary End ##########################################################

  # Specify the directory where you want to save the file
  output_directory <- "preprocessed_data"

  # Check if the directory exists, and if not, create it
  if (!file.exists(output_directory)) {
    dir.create(output_directory)
  }

  # Save the preprocessed data to a CSV file in the specified directory
  write.csv(data, file.path(output_directory, "preprocessed_data.csv"), row.names = FALSE)

  cat("Pre-processing completed, generated preprocessed data at", file.path(output_directory, "preprocessed_data.csv"), "\n")
}



################################################# Main script ###############################################################

data <- read.csv("data/data.csv")

# Preprocessing
data <- preprocess_data(data)

################################################ Main Script End ############################################################