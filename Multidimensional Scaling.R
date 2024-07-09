# Install and load necessary libraries
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(readr)
library(pheatmap)

# Load the dataset
data_filepath <- "C:/Users/nihar/OneDrive/Desktop/Bootcamp/SCMA 632/DataSet/icecream.csv"
icecream_data <- read_csv(data_filepath)

# Display the first few rows of the dataset
head(icecream_data)

# Check the structure of the dataset
str(icecream_data)

# Select only the numeric columns for MDS
icecream_data_numeric <- icecream_data %>%
  select(-Brand)

# Verify the cleaned data
str(icecream_data_numeric)
summary(icecream_data_numeric)

# Compute the distance matrix
icecream_dist <- dist(icecream_data_numeric)

# Apply Multidimensional Scaling (MDS)
mds_fit <- cmdscale(icecream_dist, k = 2)  # k = 2 for 2D plot

# Create a data frame with MDS results
mds_data <- as.data.frame(mds_fit)
colnames(mds_data) <- c("Dim1", "Dim2")
mds_data$Sample <- icecream_data$Brand

# Plot the MDS results
mds_plot <- ggplot(mds_data, aes(x = Dim1, y = Dim2, label = Sample)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  labs(title = "MDS Plot of Ice Cream Dataset",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme_minimal()

# Show the MDS plot
print(mds_plot)

# Create a heatmap of the distance matrix
heatmap_data <- as.matrix(icecream_dist)
rownames(heatmap_data) <- icecream_data$Brand
colnames(heatmap_data) <- icecream_data$Brand

# Plot the heatmap
heatmap_plot <- pheatmap(heatmap_data,
                         clustering_distance_rows = icecream_dist,
                         clustering_distance_cols = icecream_dist,
                         display_numbers = TRUE,
                         fontsize_number = 8,
                         main = "Heatmap of Ice Cream Distance Matrix")