# load the required packages
library(dplyr)
library(stats)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)
library(plotly)

# Load the Mall Customers Data
customers <- read.csv("Mall_Customers.csv", stringsAsFactors = TRUE)

# ============================== PRE-PROCESSING ================================

# dimensions of Mall Customers dataset
dim(customers)

# Have a peek at our dataset
head(customers, n=10) #shows the first six lines of the Mall Customers Data

# Structure of the dataset
str(customers)

# list types for each columns in the dataset
sapply(customers, class)

# check for missing values
colSums(is.na(customers))

#checking for duplicates
anyDuplicated(customers)

# statistical summary of the dataset
summary(customers)

# Rename column 'Genre' to 'Gender' using dplyr
customers <- customers %>% 
  rename(Gender = Genre, 
         Annual_Income = Annual.Income..k..,
         Spending_Score = Spending.Score..1.100.)
customers

# ========================= EXPLORTORY DATA ANALYSIS ===========================

#plotting categorical attributes

#bar plot for gender distribution
ggplot(customers, aes(Gender, fill = Gender)) + 
  geom_bar() + xlab("Gender") + 
  ylab("Count") + ggtitle("Gender: Male or Female") + 
  scale_fill_manual(values = c("#91ba4f", "#396336")) +
  theme_classic()+ 
  geom_text(stat='count',
            aes(label=paste0(round(after_stat(prop*100), digits=1), "%"),group=1),
            vjust=-0.4,
            size=4)

#plotting numerical attributes

# Histogram for each numerical Attribute
customers %>%
  gather(Attributes, value, c(3,4,5)) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE, bins = 10) +
  facet_wrap(~Attributes, scales="free") +
  labs(x="Values", y="Frequency",
       title="Mall Customers Distribution (Numerical Attributes)") +
  scale_fill_manual(values = c("#91ba4f", "#373E40","#396336")) +
  theme_bw()

# Correlation between numerical attributes
figsize <- options(repr.plot.width = 12, repr.plot.height = 8)

age <- customers %>%
  ggplot(aes(x = Age, y = Annual_Income, color = Gender)) +
  geom_point() +
  labs(
    x = "Ages",
    y = "Annual_Income(k$)",
    title = "Correlation between the Annual Income and Age"
  )

spending <- customers %>%
  ggplot(aes(x = Age, y = Spending_Score, color = Gender)) +
  geom_point() +
  labs(
    x = "Age",
    y = "Spending_Score(1-100)",
    title = "Correlation between the Spending Score and Age"
  )

annual <- customers %>%
  ggplot(aes(x = Annual_Income, y = Spending_Score, color = Gender)) +
  geom_point() +
  labs(
    x = "Annual_Income(k$)",
    y = "Spending_Score(1-100)",
    title = "Correlation between Annual Income and Spending Score"
  )

cowplot::plot_grid(age, spending, annual, labels = "AUTO", ncol = 2, nrow = 2)

#Correlation between categorical attributes
figsize <- options(repr.plot.width=12, repr.plot.height=8)

gender_age <- customers %>%
  ggplot(aes(x=forcats::fct_reorder(Gender, Age, .fun=median, .desc=TRUE),
             y=Annual_Income,
             fill=Gender)) +
  geom_boxplot(show.legend = TRUE) +
  labs(x = "", y = "Age",
       title = "Distribution of Age by Gender") + 
  scale_fill_manual(values = c("#91ba4f", "#396336"))

gender <- customers %>%
  ggplot(aes(x=forcats::fct_reorder(Gender, Annual_Income, .fun=median, .desc=TRUE),
             y=Annual_Income,
             fill=Gender)) +
  geom_boxplot(show.legend = TRUE) +
  labs(x = "", y = "Annual_Income(k$)",
       title = "Distribution of Annual Income(k$) by Gender") +
  scale_fill_manual(values = c("#91ba4f", "#396336")) 

gender_spend <- customers %>%
  ggplot(aes(x=forcats::fct_reorder(Gender, Spending_Score, .fun=median, .desc=TRUE),
             y=Spending_Score,
             fill=Gender)) +
  geom_boxplot(show.legend = TRUE) +
  labs(x = "", y = "Spending_Score(1-100)",
       title = "Distribution of Spending Score by Gender") +
  scale_fill_manual(values = c("#91ba4f", "#396336")) +
  theme_bw()

cowplot::plot_grid(gender_age, gender, gender_spend,labels="AUTO", ncol = 2, nrow = 2)

# Calculate and # Print the correlation matrix
correlation_matrix <- cor(customers[, c("Age", "Annual_Income", "Spending_Score")])
print(correlation_matrix)


# Create a correlation plot
corrplot(correlation_matrix, method = "color", addCoef.col = "black", tl.cex = 0.7)

# correlation plot
ggpairs(customers[2:5], aes(color = Gender))

# Preprocess the data: Encode Gender as a numeric variable
customers$Gender <- as.numeric(factor(customers$Gender)) # Male = 2, Female = 1

# Summary statistics
summary(customers)

# ================ MODEL BUILDING - K MEANS CLUSTERING =========================

# Selecting Age and Annual Income features for clustering
features1 <- customers[, c("Age", "Annual_Income")]

# Determine the optimal number of clusters using the 'elbow method' (WCSS)
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(features1, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow method to find the optimal number of clusters
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within-Cluster Sum of Squares", main = "Elbow method")

# Specify the number of clusters (k)
k <- 5  # Update with the chosen optimal number of clusters

# Perform k-means clustering on the mall customer dataset
set.seed(6)  # For reproducibility
kmeans_result <- kmeans(features1, centers = k)

# Add cluster assignments to the original data
features1$Cluster <- kmeans_result$cluster

# Converting the "Cluster" column into a factor
features1$Cluster <- factor(features1$Cluster)

# Visualize the clusters with modified color
a <- ggplot(features1, 
            aes(x = Age, y = Annual_Income, color = Cluster)) +
  geom_point() +
  scale_color_brewer(palette = "Set2") +  # Set cluster colors
  labs(x = "Age", y = "Annual Income, $", color = "Cluster")

# Display the plot
ggplotly(a)


# Selecting Annual Income and Spending Score features for clustering
features2 <- customers[, c("Annual_Income", "Spending_Score")]

# Determine the optimal number of clusters using the 'elbow method' (WCSS)
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(features2, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow method to find the optimal number of clusters
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within-Cluster Sum of Squares", main = "Elbow method")
abline(v = k, col = "red", lty = 2)
abline(v = k, col = "red", lty = 2, lwd = 2)  # Add a thicker dashed line for emphasis
legend("topright", legend = "Optimal k", col = "red", lty = 2)

# Specify the number of clusters (k)
k <- 5  # Update with the chosen optimal number of clusters

# Perform k-means clustering on the mall customer dataset
set.seed(8)  # For reproducibility
kmeans_result <- kmeans(features2, centers = k)

# Add cluster assignments to the original data
features2$Cluster <- kmeans_result$cluster

# Converting the "Cluster" column into a factor
features2$Cluster <- factor(features2$Cluster)


# Visualize the clusters for Annual Income vs Spending score
b <- ggplot(features2, 
            aes(x = Annual_Income, y = Spending_Score, color = factor(Cluster))) +
  geom_point() +
  labs(title = "K-Means Cluster Profiles,\nAnnualIncome vs. Spending Score",
       x = "Annual Income, $", y = "Spending Score", color = "Cluster")

ggplotly(b)


# Selecting Age and Spending Score features for clustering
features3 <- customers[, c("Age", "Spending_Score")]

# Determine the optimal number of clusters using the 'elbow method' (WCSS)
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(features3, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plot the elbow method to find the optimal number of clusters
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within-Cluster Sum of Squares", main = "Elbow method")
#abline(v = k, col = "red", lty = 2)
#abline(v = k, col = "red", lty = 2, lwd = 2)  # Add a thicker dashed line for emphasis
#legend("topright", legend = "Optimal k", col = "red", lty = 2)

# Specify the number of clusters (k)
k <- 4  # Update with the chosen optimal number of clusters

# Perform k-means clustering on the mall customer dataset
set.seed(10)  # For reproducibility
kmeans_result <- kmeans(features3, centers = k)

# Add cluster assignments to the original data
features3$Cluster <- kmeans_result$cluster

# Converting the "Cluster" column into a factor
features3$Cluster <- factor(features3$Cluster)


# Visualize the clusters for Age vs Spending score
c <- ggplot(features3, 
            aes(x = Age, y = Spending_Score, color = factor(Cluster))) +
  geom_point() +
  labs(title = "K-Means Cluster Profiles,\nAge vs. Spending Score",
       x = "Age", y = "Spending Score", color = "Cluster")

ggplotly(c)


# Selecting features for clustering
features <- customers[, c("Age", "Annual_Income", "Spending_Score")]

# I choose not to scale or standardize this data because the scale of the 
# features represent physical quantities might be inherently meaningful and 
#important for the analysis. 

#Elbow Method
# Determine the optimal number of clusters using the 'elbow method' (WCSS)
wcss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(features, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Find the optimal number of clusters 

# Plot the elbow method to find the optimal number of clusters
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", 
     ylab = "Within-Cluster Sum of Squares", main = "Elbow method")


# The optimal number of clusters is k = 5

#K-means with 5 cluster
# Specify the number of clusters (k)
k <- 5

# Perform k-means clustering on the mall customer dataset
set.seed(2)  # For reproducibility
kmeans_result <- kmeans(features, centers = k)

# Check the class of kmeans_result
class_kmeans_result <- class(kmeans_result)

class_kmeans_result

# Add cluster assignments to the original data
features$Cluster <- kmeans_result$cluster

# Save the k-means model to a file
saveRDS(kmeans_result, "kmeans_result.rds")

# Display the cluster assignments
cat("Cluster Assignments:\n")
print(head(features))


# Interpret the clusters (you can customize this part based on your data)
cluster_summary <- features %>%
  group_by(Cluster) %>%
  summarize(
    Avg_Age = mean(Age),
    Avg_Annual_Income = mean(Annual_Income),
    Avg_Spending_Score = mean(Spending_Score),
    Count = n()
  )

cat("Cluster Summaries:\n")
print(cluster_summary)

cluster_summary1 <- features %>%
    summarize(
    Avg_Ann= mean(Annual_Income))
cluster_summary1
    
# Converting the "Cluster" column into a factor
features$Cluster <- factor(features$Cluster)


# Create a 3D scatter plot
plot_3d <- plot_ly(data = features, 
                   x = ~Annual_Income, 
                   y = ~Spending_Score, 
                   z = ~Age, 
                   color = ~Cluster,
                   text = ~paste("Age: ", Age, 
                                 "<br>Annual Income (k$): ", Annual_Income, 
                                 "<br>Spending Score: ", Spending_Score,
                                 "<br>Cluster: ", Cluster),
                   hoverinfo = "text",  # Show only custom text in the tooltip
                   colors = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a"), # Add more colors if you have more clusters
                   marker = list(size = 5),
                   type = "scatter3d", 
                   mode = "markers") %>%
  layout(scene = list(
    xaxis = list(title = "Annual Income"),
    yaxis = list(title = "Spending Score"),
    zaxis = list(title = "Age")
  ))

# Display the plot
plot_3d

# ================ MAKING PREDICTION USING UNSEEN DATA =========================

# Load the k-means model from the file
loaded_model <- readRDS("kmeans_result.rds")

library(clue)

# Function to generate new random data
generate_random_data <- function(n) {
  set.seed(123)  # Set seed for reproducibility
  data.frame(
    Age = sample(18:70, n, replace = TRUE),
    Annual_Income = sample(20:150, n, replace = TRUE),
    Spending_Score = sample(1:100, n, replace = TRUE)
  )
}

# Generate new random data with the same features
new_data <- generate_random_data(100)

# Predict clusters for the new data using cl_predict
new_data$Cluster <- cl_predict(loaded_model, new_data)

# Display the first few rows of the new data with cluster assignments
cat("New Data with Cluster Assignments:\n")
print(head(new_data))

