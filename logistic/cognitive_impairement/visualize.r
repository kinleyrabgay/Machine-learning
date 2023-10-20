library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
# library(tidymodels)

# Visualization function
visualize_data <- function(data) {

  # Summary statistic
  summary(data)
  ggplot(data, aes(Age, fill = MMSE_class_2)) +
    geom_bar() +
    coord_flip()

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

############################ Main script #############################

data <- read.csv("preprocessed_data/preprocessed_data.csv")

# Visualization
visualize_data(data)
