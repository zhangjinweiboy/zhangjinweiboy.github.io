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

