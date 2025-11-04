#Lab 5
library(caret)
library(e1071)
library(GGally)
library(class)

#Read dataset
dataset <- read.csv("~/Downloads/wine.data")
names(dataset) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
dataset$Type <- as.factor(dataset$Type)
View(dataset)

#Split train/test
N <- nrow(dataset)
train.indexes <- sample(N,0.8*N)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

#Separate x (features) & y (Type)
X <- dataset[,2:14]
Y <- dataset[,1]

#SVM training model - linear kernel
svm.mod0 <- svm(Type ~ Alcohol + Magnesium, data = train, kernel = 'linear')
svm.mod0

plot(svm.mod0, data = train, formula = Alcohol~Magnesium, svSymbol = "x", dataSymbol = "o")


train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))
cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

#SVM training model - linear kernel precision, recall, f1 scores
data.frame(precision, recall, f1)

make.grid = function(X, n = 75) {
  grange = apply(X, 2, range)
  X1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  X2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(Alcohol = X1, Magnesium = X2)
}

X <- train[,c(2,6)]
Y <- as.numeric(train$Type)
Y[Y==2] <- -1

xgrid = make.grid(X)
# xgrid[1:10,]

ygrid = predict(svm.mod0, xgrid)

plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)

points(X, col = Y + 3, pch = 19)
points(X[svm.mod0$index,], pch = 5, cex = 2)

#SVM test model - linear kernel
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

#SVM test model - linear kernel precision, recall, f1 scores
data.frame(precision, recall, f1)


#Second SVM train model - polynomial kernel
svm.mod1 <- svm(Type ~ Alcohol+Magnesium, data = train, kernel = 'radial')

plot(svm.mod1, train, Alcohol~Magnesium)

train.pred <- predict(svm.mod1, train)

ygrid = predict(svm.mod1, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(X, col = Y + 1, pch = 19)
points(X[svm.mod0$index,], pch = 5, cex = 2)
points(X, col = Y + 3, pch = 19)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) 
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) 
p = rowsums / n 
q = colsums / n 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

#Second SVM train model - polynomial kernel precision, recall, f1 scores
data.frame(precision, recall, f1)

#Second SVM testing model - polynomial kernel
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

#Second SVM testing model - polynomial kernel precision, recall, f1 scores
data.frame(precision, recall, f1)

#Tuned SVM - polynomial kernel to find optimum C and gamma values
gamma.range <- seq(0.1,10, .1)
gamma.range

Cost.range <- seq(1,20, 1)
Cost.range

tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial',gamma = gamma.range, cost = Cost.range)
tuned.svm

#kNN model 
split.rat <- 0.7
train.indexes <- sample(150,split.rat*150)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

mod.knn <- train(Type~Alcohol + Magnesium, data=train, method="knn")

pred.knn <- predict(mod.knn, newdata = test)

cm <- table(Actual = test$Type, Predicted = pred.knn)
cm

n = sum(cm) 
nc = nrow(cm) 
diag = diag(cm) 
rowsums = apply(cm, 1, sum) 
colsums = apply(cm, 2, sum) 
p = rowsums / n 
q = colsums / n 

accuracy <- sum(diag)/n
accuracy

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

#kNN model precision, recall, f1 scores
data.frame(precision, recall, f1)

#In summary, the SVM testing using a polynomial kernel performed the best, as it showed high consistency, few classification errors, and strong performance across all classes, suggested by type 2's almost perfect balance (.93 across precision, recall and f1) , with type 1 (f1 = .85) and type 3 (f1 = 67) also performing well. SVM testing with a linear kernel produced good results, with precision and recll being strong across all three types. It is balanced, identifying most true positives while keeping false positives low, suggesting performance is consistent and reliable across all 3 types. The kNN model performed the worst, as its precision and recall are consistetly lower than the SVM models, dropping to .57 and .13, respectively, for type 3. The model struggles to correctly detect certain types of wine, especially for type 3. It produces more false positives and misses more true cases, suggesting poorer generalization