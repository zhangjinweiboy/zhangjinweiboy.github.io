## 1.线性回归

#加载训练集和数据集
#识别特征和响应变量，值必须是数字和numpy数组
x_train <- input_variables_values_training_datasets
y_train <- target_variables_values_training_datasets
x_test <- input_variables_values_test_datasets
x <- cbind(x_train,y_train)
# 使用训练集训练模型并检查分数
linear <- lm(y_train ~ ., data = x)
summary(linear)
# 输出预测
predicted= predict(linear,x_test) 


## 2.逻辑回归

x <- cbind(x_train,y_train)
# 使用训练集训练模型并检查分数
logistic <- glm(y_train ~ ., data = x,family='binomial')
summary(logistic)
# 输出预测
predicted= predict(logistic,x_test)



## 3.决策树

x <- cbind(x_train,y_train)
# grow tree 
library(rpart)
fit <- rpart(y_train ~ ., data = x,method="class")
summary(fit)
# 输出预测
predicted= predict(fit,x_test)



## 4.支持向量机

x <- cbind(x_train,y_train)
# Fitting model
library(e1071)
fit <-svm(y_train ~ ., data = x)
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)


## 5.朴素贝叶斯

x <- cbind(x_train,y_train)
# Fitting model
library(e1071)
fit <-naiveBayes(y_train ~ ., data = x)
summary(fit)
# 输出预测
predicted= predict(fit,x_test)


## 6.K-邻近

x <- cbind(x_train,y_train)
# Fitting model
library(knn)
fit <-knn(y_train ~ ., data = x,k=5)
summary(fit)
# 输出预测
predicted= predict(fit,x_test)


## 7.K-均值


library(cluster)
fit <- kmeans(X, 3) # 5 cluster solution


## 8.随机森林

x <- cbind(x_train,y_train)
# Fitting model
library(randomForest)
fit <- randomForest(Species ~ ., x,ntree=500)
summary(fit)
# 输出预测
predicted= predict(fit,x_test)


## 9.PCA

library(stats)

pca <- princomp(train, cor = TRUE)

train_reduced  <- predict(pca,train)

test_reduced  <- predict(pca,test)



## 10.Gradient Boosing 和 AdaBoost

x <- cbind(x_train,y_train)

# Fitting model
library(caret)
fitControl <- trainControl( method = "repeatedcv", number = 4, repeats = 4)

fit <- train(y ~ ., data = x, method = "gbm", trControl = fitControl,verbose = FALSE)

predicted= predict(fit,x_test,type= "prob")[,2] 
