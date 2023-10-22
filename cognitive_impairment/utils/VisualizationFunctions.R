suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

################################################################## Function to create and save a pie chart ###########################################################

create_pie_chart <- function(data, visualize_output_dir) {
  cat("Generating MMSE Stats insights...\n")

  count_data <- data %>%
    summarise(Cognitively_Impaired_Count = sum(MMSE_class_2 == "Cognitively Impaired"),
              Cognitively_Healthy_Count = sum(MMSE_class_2 == "Cognitively Healthy"))

  pie_data <- data.frame(
    Group = c("Cognitively Impaired", "Cognitively Healthy"),
    Count = c(count_data$Cognitively_Impaired_Count, count_data$Cognitively_Healthy_Count)
  )

  pie_chart <- ggplot(pie_data, aes(x = "", y = Count, fill = Group)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = "Distribution of MMSE Results") +
    scale_fill_manual(values = c("skyblue", "lightgray")) +
    theme_void() +
    geom_text(aes(label = Count, y = cumsum(Count) - 0.5 * Count), color = "black", size = 4, fontface = "bold") +
    theme(plot.title = element_text(hjust = 0.5))

  pie_chart_file <- file.path(visualize_output_dir, "cognitive_impairment_pie_chart.pdf")
  ggsave(pie_chart_file, plot = pie_chart)

  cat("Completed generating the insight =>", pie_chart_file, "\n\n")
}

#######################################################################################################################################################################




################################################################### Function to create and save a bar plot ############################################################

create_bar_plot <- function(data, visualize_output_dir) {
  cat("Generating AGE vs MMSE insights...\n")

  p <- ggplot(data, aes(Age, fill = MMSE_class_2)) +
    geom_bar() +
    coord_flip() +
    labs(title = "Distribution of MMSE Results by Age") +
    theme(plot.title = element_text(hjust = 0.5))

  bar_plot_file <- file.path(visualize_output_dir, "age_vs_MMSE_class_2_plot.pdf")
  ggsave(bar_plot_file, plot = p)

  cat("Completed generating the insight =>", bar_plot_file, "\n\n")
}

#######################################################################################################################################################################




################################################################# Function to create and save a bar plot for age counts #################################################

create_age_count_plot <- function(data, visualize_output_dir) {
  cat("Generating Age Count Stats insights...\n")

  age_counts <- data %>%
    group_by(Age) %>%
    summarise(count = n())

  p <- ggplot(age_counts, aes(x = Age, y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Age Counts", x = "Age", y = "Count") +
    theme_minimal() +
    geom_text(aes(label = count), vjust = -0.5, size = 2) +
    theme(plot.title = element_text(hjust = 0.5))

  age_count_plot <- file.path(visualize_output_dir, "age_counts_plot.pdf")
  ggsave(age_count_plot, plot = p)

  cat("Completed generating the insight =>", age_count_plot, "\n\n")
}

#####################################################################################################################################################################




##################################### Function to create and save a bar plot for age group vs Cognitively Impaired count ############################################

create_age_group_MMSE_plot <- function(data, visualize_output_dir) {
  cat("Generating Count of Cognitively Impaired Individuals by Age...\n")

  age_counts <- data %>%
    group_by(Age) %>%
    summarise(Cognitively_Impaired_Count = sum(MMSE_class_2 == "Cognitively Impaired"))

  age_plot <- ggplot(age_counts, aes(x = Age, y = Cognitively_Impaired_Count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(data = subset(age_counts, Cognitively_Impaired_Count > 0), aes(label = Cognitively_Impaired_Count), vjust = -0.5, size = 2) +
    labs(title = "Count of Cognitively Impaired Individuals by Age", x = "Age", y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  age_group_MMSE_plot <- file.path(visualize_output_dir, "age_group_MMSE.pdf")
  ggsave(age_group_MMSE_plot, plot = age_plot)

  cat("Completed generating the insight =>", age_group_MMSE_plot, "\n\n")
}

#######################################################################################################################################################################




###################################### Function to create and save a bar plot for MNAa_tot group vs Cognitively Impaired count #########################################

create_MNAa_tot_vs_MMSE_plot <- function(data_file, visualize_output_dir) {
  cat("Generating Count of Cognitively Impaired Individuals by MNAa_tot...\n")

  # Read data from the file
  data <- read.table(data_file, header = TRUE)

  # Create a bar plot
  p <- ggplot(data, aes(x = MNAa_tot, y = count)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Bar Graph of MNAa_tot vs. Count",
        x = "MNAa_tot",
        y = "Count")

  ggsave(filename = file.path(visualize_output_dir, "MNAa_tot_vs_Count.png"), plot = p)
  cat("Completed generating the insight =>", file.path(visualize_output_dir, "MNAa_tot_vs_Count.png"), "\n\n")
}

#########################################################################################################################################################################




####################################################### Age Vs Cognitive Status Plot ####################################################################################

create_age_plot_and_save <- function(data_file, visualize_output_dir) {
  cat("Generating Count of Cognitively Impaired Individuals by Age...\n")
  
  # Read data from the file
  age_data <- read.table(data_file, header = TRUE)

  # Create a scatter plot
  scatter_plot <- ggplot(data = age_data, aes(x = Age, y = count)) +
    geom_point() +
    labs(
      title = "Age vs Cognitive Status",
      x = "Age",
      y = "Count"
    )

  # Define the output file path and name based on visualize_output_dir
  output_file <- file.path(visualize_output_dir, "age_grouped_ci_scatter_plot.pdf")

  # Save the scatter plot as a PDF file in the specified directory
  ggsave(filename = output_file, plot = scatter_plot, device = "pdf")

  cat("Completed generating the insight =>", output_file, "\n\n")
}

##########################################################################################################################################################################




####################################################### Bmi Vs Cognitive Status Plot ####################################################################################

create_bmi_plot_and_save <- function(data_file, visualize_output_dir) {
  cat("Generating Count of Cognitively Impaired Individuals by bmi...\n")
  
  # Read data from the file
  bmi_data <- read.table(data_file, header = TRUE)

  # Create a scatter plot
  scatter_plot <- ggplot(data = bmi_data, aes(x = bmi, y = count)) +
    geom_point() +
    labs(
      title = "Bmi vs Cognitive Status",
      x = "Bmi",
      y = "Count"
    )

  # Define the output file path and name based on visualize_output_dir
  output_file <- file.path(visualize_output_dir, "bmi_grouped_ci_scatter_plot.pdf")

  # Save the scatter plot as a PDF file in the specified directory
  ggsave(filename = output_file, plot = scatter_plot, device = "pdf")

  cat("Completed generating the insight =>", output_file, "\n\n")
}

##########################################################################################################################################################################




##################################### Function to create and save a bar plot for MNAa_tot group vs Cognitively Impaired count ############################################

create_MNA_tot_vs_MMSE_plot <- function(data_file, visualize_output_dir) {
  cat("Generating Count of Cognitively Impaired Individuals by MNAa_q3...\n")

  # Read data from the file
  MNAa_q3_data <- read.table(data_file, header = TRUE)

  # Create a bar plot
  MNAa_q3_plot <- ggplot(MNAa_q3_data, aes(x = MNAa_q3, y = count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    geom_text(aes(label = count), vjust = -0.5, size = 3) +
    labs(title = "Count of Cognitively Impaired Individuals by MNAa_q3", x = "MNAa_q3", y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  # Define the output file path and name based on visualize_output_dir
  MNAa_q3_vs_MMSE_plot <- file.path(visualize_output_dir, "MNAa_q3_vs_MMSE_plot.pdf")

  # Save the plot as a PDF file in the specified directory
  ggsave(filename = MNAa_q3_vs_MMSE_plot, plot = MNAa_q3_plot, device = "pdf")

  cat("Completed generating the insight =>", MNAa_q3_vs_MMSE_plot, "\n\n")
}

#######################################################################################################################################################################




############################################# Function to visualize data ##################################################################################################

visualize_data <- function(data, visualize_output_dir) {
  # Specify the directory where you want to save the file
  if (!dir.exists(visualize_output_dir)) {
    dir.create(visualize_output_dir)
  }

  create_pie_chart(data, visualize_output_dir)
  create_bar_plot(data, visualize_output_dir)
  create_age_count_plot(data, visualize_output_dir)
  create_age_group_MMSE_plot(data, visualize_output_dir)
  create_age_plot_and_save("./preprocessed_data/Age_gcount[Cognitively Impaired].txt", visualize_output_dir)
  create_MNAa_tot_vs_MMSE_plot("./preprocessed_data/MNAa_tot_gcount[Congnitively Impaired].txt",visualize_output_dir)
  create_bmi_plot_and_save("./preprocessed_data/bmi_gcount[Cognitively Impaired].txt",visualize_output_dir)
  create_MNA_tot_vs_MMSE_plot("./preprocessed_data/MNAa_q3_gcount[Congnitively Impaired].txt",visualize_output_dir)
}

###########################################################################################################################################################################