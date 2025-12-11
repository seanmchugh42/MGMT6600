# Load packages
library(readr)
library(tidyverse)
library(psych)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(factoextra)
library(cluster)
library(caret)
library(Seurat)
library(class)

# Read in datasets and view them
dataset1 = read_csv("Downloads/Student Stress Factors (2).csv")
dataset2 = read_csv("Downloads/Mental_Health_and_Social_Media_Balance_Dataset.csv")

View(dataset1)
View(dataset2)

# Get rid of spaces in column names
colnames(dataset1) <- c("sleep_quality", "headaches", "academic_performance", "study_load", "extracurricular_activities", "stress")
colnames(dataset2) <- c("ID", "age", "gender", "screen_time", "sleep_quality", "stress", "days_without_social_media", "exercise", "social_media", "happiness")

# Summary statistics for both datasets
describe(dataset1)
describe(dataset2)

# Histograms for dataset 1
dataset1 %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "grey70", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distributions of Variables in Dataset1",
       x = "Value", y = "Count")

# Histograms for dataset 2
dataset2 %>%
  select(age, screen_time, sleep_quality, stress, days_without_social_media, exercise, happiness) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "grey70", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distributions of Key Numeric Variables in Dataset2",
       x = "Value", y = "Count")

# Dataset 1 scatterplot: sleep_quality vs stress
ggplot(dataset1, aes(x = sleep_quality, y = stress)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sleep Quality vs Stress (Dataset1)")

# Dataset 1 scatterplot: study_load vs stress
ggplot(dataset1, aes(x = study_load, y = stress)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Study Load vs Stress (Dataset1)")

# Dataset 2 scatterplot: screen_time vs happiness
ggplot(dataset2, aes(x = screen_time, y = happiness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Screen Time vs Happiness (Dataset2)")

# Dataset 2 scatterplot: stress vs happiness
ggplot(dataset2, aes(x = stress, y = happiness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Stress vs Happiness (Dataset2)")

# Dataset 1 correlations
num1 <- dataset1 %>% select(where(is.numeric))
cor1 <- cor(num1, use = "pairwise.complete.obs")
cor1

# Dataset 2 correlations
num2 <- dataset2 %>% select(where(is.numeric))
cor2 <- cor(num2, use = "pairwise.complete.obs")
cor2

# Dataset 1 PCA
X1 <- dataset1 %>% select(sleep_quality, headaches, academic_performance,
                          study_load, extracurricular_activities, stress)

pca1 <- prcomp(X1, center = TRUE, scale. = TRUE)
summary(pca1)             # variance explained
pca1$rotation             # loadings

# Scree plot
plot(pca1, type = "l", main = "Scree Plot for PCA (Dataset1)")

# Biplot 
autoplot(pca1, data = dataset1, colour = "stress",
         loadings = TRUE, loadings.colour = "blue",
         loadings.label = TRUE, loadings.label.size = 3,
         main = "PCA Biplot of Stress Factors")

# Scatterplot of first two PCs
pc_scores1 <- as.data.frame(pca1$x)
ggplot(pc_scores1, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.7) +
  labs(title = "PC1 vs PC2 (Dataset1)")

# Scale dataset 1 numeric features
X1_scaled <- scale(X1)

set.seed(123)

# Elbow curve to pick k
wss1 <- sapply(1:6, function(k) {
  kmeans(X1_scaled, centers = k, nstart = 25)$tot.withinss
})

qplot(1:6, wss1, geom = "line") +
  labs(title = "Elbow Plot for k-means (Dataset1)",
       x = "Number of clusters (k)", y = "Total within-cluster SS")

# Choose k = 3 (example based on elbow)
set.seed(123)
km1 <- kmeans(X1_scaled, centers = 3, nstart = 25)
km1$size
km1$centers

# Silhouette plot for k=3
sil1 <- silhouette(km1$cluster, dist(X1_scaled))
fviz_silhouette(sil1)

# Cluster visualization
fviz_cluster(km1, data = X1_scaled,
             main = "k-means Clusters (Dataset1)")

# Summarize clusters in original scale
cluster_summary1 <- dataset1 %>%
  mutate(cluster = factor(km1$cluster)) %>%
  group_by(cluster) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
cluster_summary1

X2 <- dataset2 %>%
  select(screen_time, sleep_quality, stress,
         days_without_social_media, exercise, happiness)

X2_scaled <- scale(X2)

set.seed(123)
wss2 <- sapply(1:6, function(k) {
  kmeans(X2_scaled, centers = k, nstart = 25)$tot.withinss
})

qplot(1:6, wss2, geom = "line") +
  labs(title = "Elbow Plot for k-means (Dataset2)")

# k=3 example
set.seed(123)
km2 <- kmeans(X2_scaled, centers = 3, nstart = 25)
km2$centers
sil2 <- silhouette(km2$cluster, dist(X2_scaled))
fviz_silhouette(sil2)
fviz_cluster(km2, data = X2_scaled,
             main = "k-means Clusters (Dataset2)")

cluster_summary2 <- dataset2 %>%
  mutate(cluster = factor(km2$cluster)) %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
cluster_summary2

# 70/30 train/test split and run regression on trained dataset 1
set.seed(123)
train_idx1 <- createDataPartition(dataset1$stress, p = 0.7, list = FALSE)
train1 <- dataset1[train_idx1, ]
test1  <- dataset1[-train_idx1, ]

model1 <- lm(stress ~ sleep_quality + headaches + academic_performance +
               study_load + extracurricular_activities,
             data = train1)

summary(model1)

# Diagnostic graphs
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# Predict using test set
pred1 <- predict(model1, newdata = test1)

# Performance metrics: RMSE, R², MAE
postResample(pred1, test1$stress)

# Simple residual vs fitted plot 
aug1 <- data.frame(fitted = fitted(model1), resid = resid(model1))
ggplot(aug1, aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Model1: Residuals vs Fitted", x = "Fitted values", y = "Residuals")

# 70/30 train/test split and run regression on trained dataset 2
set.seed(123)
train_idx2 <- createDataPartition(dataset2$happiness, p = 0.7, list = FALSE)
train2 <- dataset2[train_idx2, ]
test2  <- dataset2[-train_idx2, ]

model2 <- lm(happiness ~ screen_time + sleep_quality + stress +
               days_without_social_media + exercise,
             data = train2)

summary(model2)

# Diagnostic graphs
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# Predict using test set
pred2 <- predict(model2, newdata = test2)

# Performance metrics: RMSE, R², MAE
postResample(pred2, test2$happiness)

# Create binary label: high stress vs low stress
median_stress <- median(dataset1$stress, na.rm = TRUE)
dataset1$stress_high <- ifelse(dataset1$stress > median_stress, 1, 0)
dataset1$stress_high <- factor(dataset1$stress_high, levels = c(0,1),
                               labels = c("low", "high"))

# 70/30 train/test split
set.seed(123)
idx_knn <- createDataPartition(dataset1$stress_high, p = 0.7, list = FALSE)
train_knn <- dataset1[idx_knn, ]
test_knn  <- dataset1[-idx_knn, ]

# Predictors to use
feature_cols <- c("sleep_quality", "headaches", "academic_performance",
                  "study_load", "extracurricular_activities")

X_train <- scale(train_knn[, feature_cols])
X_test  <- scale(test_knn[, feature_cols],
                 center = attr(X_train, "scaled:center"),
                 scale  = attr(X_train, "scaled:scale"))

y_train <- train_knn$stress_high
y_test  <- test_knn$stress_high

# kNN with k = 3
set.seed(123)
knn3 <- knn(train = X_train, test = X_test, cl = y_train, k = 3)

confusionMatrix(knn3, y_test)

# Tune k
k_values <- seq(1, 15, 2)
acc <- sapply(k_values, function(k) {
  pred <- knn(train = X_train, test = X_test, cl = y_train, k = k)
  mean(pred == y_test)
})

# Accuracy metric
accuracy_df <- data.frame(k = k_values, accuracy = acc)

# kNN accuracy vs. k line graph
ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(title = "kNN Accuracy vs k", x = "k", y = "Accuracy")

