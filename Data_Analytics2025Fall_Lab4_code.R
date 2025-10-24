##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("~/Downloads")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))

###
# creating another dataframe from wine dataset that contains the columns from 2 to 14
X <- wine[,2:14]
Y <- wine[,1]

## Compute the PCs and plot the dataset using the 1st and 2nd PCs.

Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = F)

principal_components <- princomp(Xc)

summary(principal_components)

principal_components$loadings

# using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)

# Following the same steps except with scaled PCA
Xs <- scale(Xmat, center = T, scale = T)

principal_components2 <- princomp(Xs)

summary(principal_components2)

principal_components2$loadings

library(caret)

# using autoplot() function to plot the components
autoplot(principal_components2, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)
## Identify the variables that contribute the most to the 1st PC and 2nd PCs.
# Color intensity, alcohol, proline, total phenols, flavanoids, Od280/od315 of diluted wines, and hue contribute the most

## Train a classifier model (e.g. kNN) to predict the wine type using all the variables in the original dataset.
split.rat <- 0.7
train.indexes <- sample(150,split.rat*150)

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

mod.knn <- train(Type~., data=train, method="knn")

### 1 round cross-validation
knn.train.true <- train[,1]
knn.test.true <- test[,1]

knn.train.predicted <- predict(mod.knn,train[,-1])
knn.test.predicted <- predict(mod.knn,test[,-1])

train.cm = as.matrix(table(Actual = knn.train.true$Type, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm))/nrow(train)
train.accuracy

test.cm = as.matrix(table(Actual = knn.test.true$Type, Predicted = knn.test.predicted))
test.cm
test.accuracy <- sum(diag(test.cm))/nrow(test)
test.accuracy

##Train a classifier model to predict the wine type using the data projected onto the first 2 PCs (scores in the princomp function’s return object)
pc_scores <- as.data.frame(principal_components2$scores[, 1:2])
colnames(pc_scores) <- c("PC1","PC2")
pc_data <- data.frame(Type = as.factor(wine$Type), pc_scores)

train.pc <- pc_data[train.indexes, ]
test.pc  <- pc_data[-train.indexes, ]

mod.knn <- train(Type ~ PC1 + PC2, data = train.pc, method = "knn")

knn.train.true <- train.pc[, 1]; knn.test.true <- test.pc[, 1]
knn.train.predicted <- predict(mod.knn, train.pc[, -1])
knn.test.predicted  <- predict(mod.knn,  test.pc[, -1])

train.cm <- as.matrix(table(Actual = knn.train.true, Predicted = knn.train.predicted))
test.cm  <- as.matrix(table(Actual = knn.test.true,  Predicted = knn.test.predicted))

## Compare the 2 classification models using contingency tables and precision/recall/F1 metrics.
train.cm <- as.matrix(table(Actual = train.pc$Type, Predicted = knn.train.predicted))
train.cm
train.accuracy <- sum(diag(train.cm)) / nrow(train.pc)
train.accuracy

test.cm <- as.matrix(table(Actual = test.pc$Type, Predicted = knn.test.predicted))
test.cm
test.accuracy <- sum(diag(test.cm)) / nrow(test.pc)
test.accuracy

knn.cm.full <- confusionMatrix(knn.test.predicted, test.pc$Type, mode = "prec_recall")
knn.byclass <- as.data.frame(knn.cm.full$byClass)

knn.precision_macro <- mean(knn.byclass$Precision)
knn.recall_macro    <- mean(knn.byclass$Recall)
knn.f1_macro        <- mean(knn.byclass$F1)

knn.precision_macro
knn.recall_macro
knn.f1_macro