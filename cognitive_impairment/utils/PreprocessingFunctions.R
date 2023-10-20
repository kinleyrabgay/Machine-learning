# Function to count cognitively impaired and healthy individuals
count_cognitive_status <- function(data) {
  result <- data %>%
    group_by(MMSE_class_2) %>%
    summarise(Count = n())

  cognitively_impaired_count <- result$Count[result$MMSE_class_2 == "Cognitively Impaired"]
  cognitively_healthy_count <- result$Count[result$MMSE_class_2 == "Cognitively Healthy"]

  return(list(Cognitively_Impaired = cognitively_impaired_count, Cognitively_Healthy = cognitively_healthy_count))
}


# Function to preprocess data
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

  # Data preprocessing
  data[data == "?"] <- NA
  data$Age <- as.numeric(data$Age)
  data$MNAa_tot <- as.numeric(data$MNAa_tot)
  data$Education_ID <- as.factor(data$Education_ID)
  data$Mobility <- as.factor(data$Mobility)
  data$MNAa_q3 <- as.factor(data$MNAa_q3)
  data$Endocrine.Disease_Hyperlipidaemia <- as.factor(data$Endocrine.Disease_Hyperlipidaemia)
  data$MMSE_class_2 <- factor(ifelse(data$MMSE_class_2 == 0, "Cognitively Impaired", "Cognitively Healthy"))

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

  # Summarize age group counts
  age_group_counts <- data %>%
    group_by(Age) %>%
    summarise(count = n())
  write.table(age_group_counts, file.path(preprocessed_output_dir, "age_group_counts.txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated age_grouped counts => preprocessed_data/age_group_counts.txt\n\n")

  # Summarize bmi group counts
  bmi_group_counts <- data %>%
    group_by(bmi) %>%
    summarise(count = n())
  write.table(bmi_group_counts, file.path(preprocessed_output_dir, "bmi_group_counts.txt"), quote = FALSE, row.names = FALSE, col.names = TRUE)
  cat("Generated bmi_grouped counts => preprocessed_data/bmi_group_counts.txt\n\n")



  # Save preprocessed data to a CSV file
  write.csv(data, file.path(preprocessed_output_dir, "preprocessed_data.csv"), row.names = FALSE)
  cat("Pre-processing completed, generated preprocessed data at", file.path(preprocessed_output_dir, "preprocessed_data.csv"), "\n\n")

  return (data)
}
