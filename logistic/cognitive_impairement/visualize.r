library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
# library(tidymodels)

# Visualization function
visualize_data <- function(data) {

  # Specify the directory where you want to save the file
  output_directory <- "visualization"

  # Check if the directory exists, and if not, create it
  if (!file.exists(output_directory)) {
    dir.create(output_directory)
  }

  # Summary
  summary(data)

  ########################################### Age Vs MMSE_class_2 ###########################################

  cat("Generating AGE vs MMSE insights...... \n")
  p <- ggplot(data, aes(Age, fill = MMSE_class_2)) +
    geom_bar() +
    coord_flip()
  age_vs_MMSE_class_2_plot <- file.path(output_directory, "age_vs_MMSE_class_2_plot.pdf")
  ggsave(age_vs_MMSE_class_2_plot, plot = p)
  cat("Completed generating the insight at => visualization/age_vs_MMSE_class_2_plot \n")

  ###########################################################################################################


  ########################################### Age Count Insights ############################################

  cat("Generating Age Count Stats insights inside data....\n")

  # Check if the age_counts.txt file exists
  if (file.exists("age_counts.txt")) {
    age_counts <- read.table("age_counts.txt", header = TRUE)
    p <- ggplot(age_counts, aes(x = Age, y = count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Age Counts", x = "Age", y = "Count") +
      theme_minimal() +
      geom_text(aes(label = count), vjust = -0.5, size = 2)

    # Specify the output file path and save the plot
    age_count_plot <- file.path(output_directory, "age_counts_plot.pdf")
    ggsave(age_count_plot, plot = p)
    cat("Completed generating the insight at => visualization/age_counts_plot\n")
  } else {
    cat("The age_counts.txt file does not exist. Please run pre-processing.r file.\n")
  }

  ############################################### End #######################################################




  # Your data visualization code here
  # You can create plots using ggplot2 or other libraries

  # Pairwise scatter plot
  # pairs(data[, sapply(data, is.numeric)], col = data$Cognitive.Impairment, pch = 16, main = "Pairwise Scatter Plots")
  # pairs(data[, c("Age", "Education_ID", "Mobility", "body_height", "MNAa_q3", "body_weight", "MNAb_tot", "waist", "bmi", "MNAa_tot")], col = data$Cognitive.Impairment, pch = 16, main = "Pairwise Scatter Plots")


  # Histogram
  # numeric_columns <- c("Age", "Education_ID", "Mobility", "body_height", "MNAa_q3", "body_weight", "MNAb_tot", "waist", "bmi", "MNAa_tot")
  # data %>%
  #   gather(key = "Variable", value = "Value", numeric_columns) %>%
  #   ggplot(aes(x = Value, fill = Cognitive.Impairment)) +
  #   geom_histogram(binwidth = 5) +
  #   facet_wrap(~Variable, scales = "free") +
  #   labs(title = "Histograms of Numerical Features") +
  #   theme_minimal()


  # Correlation heatmap
  # correlation_matrix <- cor(data[, numeric_columns])
  # corrplot::corrplot(correlation_matrix, method = "color")


  # Bar plot
  # data %>%
  #   group_by(Endocrine.Disease_Hyperlipidaemia) %>%
  #   summarise(count = n()) %>%
  #   ggplot(aes(x = Endocrine.Disease_Hyperlipidaemia, y = count)) +
  #   geom_bar(stat = "identity") +
  #   labs(title = "Counts of Endocrine Disease (Hyperlipidaemia)")

  

  # Box plot
  # data %>%
  #   ggplot(aes(x = Cognitive.Impairment, y = Age)) +
  #   geom_boxplot() +
  #   labs(title = "Box Plot of Age by Cognitive Impairment")


}

################################################# Main script #################################################

if (file.exists("preprocessed_data/preprocessed_data.csv")) {
  data <- read.csv("preprocessed_data/preprocessed_data.csv")
  
  # Visualization
  visualize_data(data)
} else {
  cat("preprocessed_data.csv don't exit, please run pre-processing.r file \n")
}

################################################################################################################