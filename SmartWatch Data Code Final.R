# Install necessary packages. The 'install.packages()' function is used to install packages that are not already installed.
install.packages(c("readxl", "dplyr", "ggplot2", "cluster", "factoextra",
                   "openxlsx", "reshape2", "mice", "fmsb"))

# Load libraries to use their functions in this analysis.
library(readxl)     # Reading Excel files
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(cluster)    # Clustering algorithms
library(factoextra) # Clustering evaluation and visualization
library(openxlsx)   # Exporting results to Excel
library(reshape2)   # Heatmap visualization
library(mice)       # Missing value imputation
library(fmsb)        # Radar charts

# Import the dataset from an Excel file
df <- read_excel(file.choose())

# View the structure of the dataset
View(df)

# Display summary statistics of the dataset
summary(df)

# Check for missing values
# Count missing values in the dataframe
sum(is.na(df))
# Impute missing values using Predictive Mean Matching (pmm)
df <- mice(df, m = 1, method = 'pmm', maxit = 5) %>% complete()
# Check the column names after imputation
colnames(df)

# Define the relevant columns for analysis based on product features and demographics
likert_vars <- c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness",
                 "Athlete", "Style", "AmznP", "Female", "Degree", "Income", "Age")
# Select these columns from the dataset
df_selected <- df %>% select(all_of(likert_vars))

# Detect outliers in the numerical variables
for (var in likert_vars) {
  print(paste("Outliers in", var, ":", boxplot.stats(df_selected[[var]])$out))
}

# Standardize the data to have a mean of 0 and standard deviation of 1
df_scaled <- scale(df_selected)

# View the standardized dataset
View(df_scaled)

# Determine the optimal number of clusters using different methods
# Elbow Method: Visualize the total within-cluster sum of squares (WSS) for different numbers of clusters
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters")

# Silhouette Method: Measure how similar an object is to its own cluster compared to other clusters
fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  ggtitle("Silhouette Analysis for Optimal Clusters")

# Gap Statistic: Compare the total within intra-cluster variation for different numbers of clusters with their expected values under null reference distribution
set.seed(123) # Ensure reproducibility by setting the random seed
gap_stat <- clusGap(df_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Hierarchical Clustering
# Calculate the Euclidean distance matrix
distance <- dist(df_scaled, method = 'euclidean')
# Perform hierarchical clustering using Ward.D2 method
hc.w <- hclust(distance, method = 'ward.D2')

# Plot the dendrogram to visualize the cluster structure
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")

# Add rectangles to highlight the clusters
rect.hclust(hc.w, k = 4, border = c("red", "blue", "green", "purple"))

# Optional: Determine the optimal number of clusters using the Elbow Plot for hierarchical clustering
x <- 1:10
sort_height <- sort(hc.w$height, decreasing = TRUE)
y <- sort_height[1:10]
plot(x, y, type = "b", main = "Elbow Plot", xlab = "Number of Clusters", ylab = "Height")
lines(x, y, col = "blue")

# Cut the dendrogram to create 4 clusters
cluster <- cutree(hc.w, k = 4)

# Display the number of observations in each cluster
table(cluster)

# Add the cluster assignments to the original dataframe
df$Cluster_Hierarchical <- as.factor(cluster)

# View the updated dataframe
View(df)

# Hierarchical Cluster Profiling
# Calculate the proportions of each cluster
proportions <- table(df$Cluster_Hierarchical) / nrow(df) * 100
print(proportions)

# Calculate mean values of variables in each hierarchical cluster
segments_hierarchical <- df %>%
  group_by(Cluster_Hierarchical) %>%
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))
print(segments_hierarchical)

# Save the hierarchical cluster profiling results to an Excel file
write.xlsx(segments_hierarchical, "segments_hierarchical.xlsx")

# Assign cluster names
df$Target_Group <- case_when(
  df$Cluster_Hierarchical == 1 ~ "Tech-Savvy Professionals",
  df$Cluster_Hierarchical == 2 ~ "Fitness Enthusiasts",
  df$Cluster_Hierarchical == 3 ~ "Budget-Conscious Users",
  df$Cluster_Hierarchical == 4 ~ "Luxury Seekers"
)

# Radar Chart for Segment Visualization
# Prepare max/min values for scaling radar chart
max_min <- data.frame(
  Wellness = c(7, 1),
  TaskMgm = c(7, 1),
  Style = c(7, 1),
  Income = c(max(segments_hierarchical$Income_mean, na.rm = TRUE), min(segments_hierarchical$Income_mean, na.rm = TRUE)),
  Age = c(max(segments_hierarchical$Age_mean, na.rm = TRUE), min(segments_hierarchical$Age_mean, na.rm = TRUE))
)
# Prepare radar data using correct names from segments_hierarchical
radar_data <- rbind(
  max_min,
  segments_hierarchical %>%
    select(Wellness_mean, TaskMgm_mean, Style_mean, Income_mean, Age_mean) %>%
    rename(Wellness = Wellness_mean, TaskMgm = TaskMgm_mean, Style = Style_mean, Income = Income_mean, Age = Age_mean) %>%
    replace(is.na(.), 0)
)
radarchart(radar_data, axistype = 2,
           pcol = c("red", "blue", "green", "purple"),
           plwd = 2,
           plty = 1,
           title = "Radar Chart for Market Segments (Hierarchical Clustering)",
           vlcex = 0.8)
legend("topright", legend = paste("Cluster", 1:4), col = c("red", "blue", "green", "purple"), pch = 20, cex = 0.8)
# # Identify most lucrative segment
# Sort clusters by Avg_Income, Avg_Wellness, and Avg_Style to identify the most lucrative segment
most_lucrative <- segments_hierarchical %>%
  arrange(desc(Income_mean), desc(Wellness_mean), desc(Style_mean))
print(most_lucrative[1, ])

# Competitor Analysis
competitor_data <- data.frame(
  Brand = c("Intel", "Apple", "Samsung"),
  Wellness_Features = c(8, 9, 7),
  Price_Competitiveness = c(7, 5, 6),
  Innovation = c(9, 10, 8),
  Market_Reach = c(6, 10, 9)
)

# Visualize competitor comparison
melted_competitor_data <- melt(competitor_data, id.vars = "Brand")
ggplot(melted_competitor_data, aes(x = Brand, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Competitor Comparison: Intel vs. Apple vs. Samsung", y = "Score")

# Partner Recommendation Logic
# Recommend a strategic partner based on the most lucrative segment's characteristics
best_segment <- most_lucrative[1, ]
if (nrow(best_segment) > 0) {
  wellness_value <- best_segment$Wellness_mean
  if (!is.na(wellness_value) && wellness_value > 5) {
    partner_recommendation <- "Aetna (Health Focus)"
  } else if (!is.na(best_segment$TaskMgm_mean) && best_segment$TaskMgm_mean > 5) {
    partner_recommendation <- "Amazon (Alexa AI Focus)"
  } else {
    partner_recommendation <- "Google (Android Wear Integration)"
  }
} else {
  partner_recommendation <- "No valid segment found"
}
print(paste("Recommended Partner for Intel:", partner_recommendation))

# SWOT data for potential partners
SWOT_data <- data.frame(
  Partner = c("Aetna", "Amazon", "Google"),
  Strengths = c("Health expertise", "AI-powered features", "Integration with Android"),
  Weaknesses = c("Limited consumer brand power", "Privacy concerns", "Competing smartwatch brands"),
  Opportunities = c("Growing wellness market", "Expanding AI in wearables", "Wear OS adoption"),
  Threats = c("Regulatory issues", "Strong competition", "Fragmentation in Android ecosystem")
)
print(SWOT_data)

# Perform PCA for dimensionality reduction
pca_result <- prcomp(df_selected, scale = TRUE)
summary(pca_result)
