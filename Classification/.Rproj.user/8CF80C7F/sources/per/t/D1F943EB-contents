#Importing the DataSet
dataset = read.csv('breast_cancer.csv')
dataset = dataset[,2:11]
# x = dataset[,1:10]
# y = dataset[,11]

#Sampling 
dataset$Class = factor(dataset$Class,
                       levels = c(2,4),
                       labels = c(0, 1))

#Splitting the Testing and Training Data
library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
training_set[1:9] = scale(training_set[1:9])
test_set[1:9] = scale(test_set[1:9])



#Classifier
classifier = glm(formula = Class ~ .,
                 family = binomial,
                 data = training_set)

#Predicting the test set
prob_pred = predict(classifier, type = 'response', newdata = test_set[1:9])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Confusion Matrix
cf = table(test_set[,10], y_pred)
n = sum(cf)
diag = diag(cf)
accuracy = (sum(diag)/n)*100


install.packages('caret')
library(caret)
train_control = trainControl(method = "cv", number = 1)
Kfold = train(Class ~ .,
              data = training_set,
              trControl = train_control,
              method = "glm",
              family = binomial())

foldpred = predict(Kfold, newdata = test_set[1:9])
kfoldcf = table(test_set[,10], foldpred)
n2 = sum(kfoldcf)
diag2 = diag(kfoldcf)
kfoldaccuracy = (sum(diag2)/n2)*100
kfoldaccuracy
accuracy
