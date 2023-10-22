############################################## Function to count cognitively impaired and healthy individuals ##############################################################

count_cognitive_status <- function(data) {
  result <- data %>%
    group_by(MMSE_class_2) %>%
    summarise(Count = n())

  cognitively_impaired_count <- result$Count[result$MMSE_class_2 == "Cognitively Impaired"]
  cognitively_healthy_count <- result$Count[result$MMSE_class_2 == "Cognitively Healthy"]

  return(list(Cognitively_Impaired = cognitively_impaired_count, Cognitively_Healthy = cognitively_healthy_count))
}

#############################################################################################################################################################################


####################################################################### Function to preprocess data ###########################################################################

preprocess_data <- function(data_file, preprocessed_output_dir) {

  # Check if the output directory exists, and if not, create it
  if (!dir.exists(preprocessed_output_dir)) {
    dir.create(preprocessed_output_dir)
  }

  # Summary of data before preprocessing
  cat("Structure of data before data-preprocesing: \n\n")
  str(data)
  cat("\n\n")

  cat("Pre-processing & cleaning your dataset, please wait...\n\n")

  # Check for "?" vlaue, replace it with NA
  data[data == "?"] <- NA

  # Check for missing values and impute or remove them
  data <- na.omit(data)

  # Transform variables to the appropriate data types
  data$Age <- as.numeric(data$Age)
  data$MNAa_tot <- as.numeric(data$MNAa_tot)
  data$Education_ID <- as.factor(data$Education_ID)
  data$Mobility <- as.factor(data$Mobility)
  data$MNAa_q3 <- as.factor(data$MNAa_q3)
  data$Endocrine.Disease_Hyperlipidaemia <- as.factor(data$Endocrine.Disease_Hyperlipidaemia)

  # Encode the target variable
  data$MMSE_class_2 <- factor(ifelse(data$MMSE_class_2 == 0, "Cognitively Healthy", "Cognitively Impaired"))

  # Summary of data after preprocessing
  cat("Structure of data after data-preprocesing: \n\n")
  str(data)
  cat("\n\n")

  # Summary of dataset
  cat("Here are some summary on the datasets: \n")
  cat("1. Total number of datasets:", nrow(data), "\n")

  counts <- count_cognitive_status(data)
  cat("2. Cognitively Impaired Count:", counts$Cognitively_Impaired, "\n")
  cat("3. Cognitively Healthy Count:", counts$Cognitively_Healthy, "\n\n")

  cat("4. Dataset-sample\n")
  print(head(data))
  cat("\n\n")

  # Count cognitively impaired and healthy individuals
  cat("Count of Cognitively Impaired and Healthy Individuals:\n")
  table(data$MMSE_class_2)
  cat("\n")

  ############################################################ Age ####################################################################################################
  
  # Overall count of by grouping age
  age_gcount <- data %>%
    group_by(Age) %>%
    summarise(count = n())
  write.table(age_gcount, file.path(preprocessed_output_dir, "Age_gcount[General].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated Age count for whole dataset => preprocessed_data/Age_gcount[General].txt\n\n")

  # Age group counts [Cognitively Impaired]
  age_ci_gcount <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(Age) %>%
    summarise(count = n())
  write.table(age_ci_gcount, file.path(preprocessed_output_dir, "Age_gcount[Cognitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated Age count for who are cognitively impaired => preprocessed_data/Age_gcount[Cognitively Impaired].txt\n\n")

  ######################################################################################################################################################################



  ########################################################################## bmi ########################################################################################
 
  # Total count by grouping bmi
  bmi_gcount <- data %>%
    group_by(bmi) %>%
    summarise(count = n())
  write.table(bmi_gcount, file.path(preprocessed_output_dir, "bmi_gcount[General].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated bmi count for whole dataset => preprocessed_data/bmi_gcount[General].txt\n\n")

  # Summarize bmi group counts who are [Cognitively Impaired]
  bmi_ci_gcount <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(bmi) %>%
    summarise(count = n())
  write.table(bmi_ci_gcount, file.path(preprocessed_output_dir, "bmi_gcount[Cognitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated bmi count for who are cognitively impaired => preprocessed_data/bmi_gcount[Cognitively Impaired].txt\n\n")

  #########################################################################################################################################################################



  ############################################################# Education_ID ###############################################################################################
  
  # Count by grouping Education_ID
  Education_ID_gcount <- data %>%
    group_by(Education_ID) %>%
    summarise(count = n())
  write.table(Education_ID_gcount, file.path(preprocessed_output_dir, "Education_ID_gcount[Genral].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated Education_ID count for whole dataset => preprocessed_data/Education_ID_gcount[Genral].txt\n\n")


  # Filter by "Cognitively Impaired" then count the data by grouped with Education_ID
  Education_ID_ci_gcount <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(Education_ID) %>%
    summarise(count = n())
  write.table(Education_ID_ci_gcount, file.path(preprocessed_output_dir, "Education_ID_gcount[Congnitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated Education_ID count who are cognitively impaired => preprocessed_data/Education_ID_gcount[Congnitively Impaired].txt\n\n")

  #############################################################################################################################################################################



  ####################################################################### Mobility ############################################################################################
  
  # Filter by "Cognitively Impaired" then count the data by grouped with Mobility
  Mobility_count <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(Mobility) %>%
    summarise(count = n())
  write.table(Mobility_count, file.path(preprocessed_output_dir, "Mobility_gcount[Cognitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated Mobility counts who are cognitively impaired => preprocessed_data/Mobility_gcount[Cognitively Impaired].txt\n\n")

  ###############################################################################################################################################################################



  ################################################################## Endocrine.Disease_Hyperlipidaemi ###########################################################################
  
  # Filter by "Cognitively Impaired" then count the data by grouped with Endocrine.Disease_Hyperlipidaemia
  Endocrine.DH_count <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(Endocrine.Disease_Hyperlipidaemia) %>%
    summarise(count = n())
  write.table(Endocrine.DH_count , file.path(preprocessed_output_dir, "Endocrine.DH_gcount[Cognitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated Endocrine.DH counts who are cognitively impaired => preprocessed_data/Endocrine.DH_gcount[Cognitively Impaired].txt\n\n")

  ################################################################################################################################################################################



  ########################################################################## MNAa_tot #############################################################################################

  # Filter by "Cognitively Impaired" then count the data by grouped with MNAa_tot_count
  MNAa_tot_count <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(MNAa_tot) %>%
    summarise(count = n())
  write.table(MNAa_tot_count , file.path(preprocessed_output_dir, "MNAa_tot_gcount[Congnitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated MNAa_tot counts who are cognitively impaired => preprocessed_data/MNAa_tot_gcount[Congitively Impaired].txt\n\n")

  ##################################################################################################################################################################################




   ########################################################################## MNAb_tot #############################################################################################

  # Filter by "Cognitively Impaired" then count the data by grouped with MNAb_tot_count
  MNAb_tot_count <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(MNAb_tot) %>%
    summarise(count = n())
  write.table(MNAb_tot_count , file.path(preprocessed_output_dir, "MNAb_tot_gcount[Congnitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated MNAa_tot counts who are cognitively impaired => preprocessed_data/MNAb_tot_gcount[Congitively Impaired].txt\n\n")

  ##################################################################################################################################################################################




  ########################################################################## MNAa_q3 #############################################################################################

  # Filter by "Cognitively Impaired" then count the data by grouped with MNAa_tot_count
  MNAa_q3_count <- data %>%
    filter(MMSE_class_2 == "Cognitively Impaired") %>%
    group_by(MNAa_q3) %>%
    summarise(count = n())
  write.table(MNAa_q3_count , file.path(preprocessed_output_dir, "MNAa_q3_gcount[Congnitively Impaired].txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated MNAa_tot counts who are cognitively impaired => preprocessed_data/MNAa_q3_gcount[Congitively Impaired].txt\n\n")

  ##################################################################################################################################################################################




  #################################################################### Save preprocessed data to a CSV file #########################################################################

  write.csv(data, file.path(preprocessed_output_dir, "preprocessed_data.csv"), row.names = FALSE)
  cat("Pre-processing completed, generated preprocessed data at", file.path(preprocessed_output_dir, "preprocessed_data.csv"), "\n\n")

  ####################################################################################################################################################################################


  return (data)
}
