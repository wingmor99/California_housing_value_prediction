require(tidyverse)
set.seed(123)
# load the dataset 
housing <- read.csv(file.choose())
# show the types of data
str(housing)
# convert the ocean_proximity attribute to factor
housing$ocean_proximity <- factor(housing$ocean_proximity)
# convert the dataset to numeric 
housing <- data.frame(map(housing, as.numeric))
# is there na?
colSums(is.na(housing))

# remove the rows which contains na
housing <- housing[-which(is.na(housing$total_bedrooms)),]
# assign the sample index
ind <- sample(2, nrow(housing), c(0.7, 0.3), replace = TRUE)
train <- housing[ind==1,]
test <- housing[ind==2,]

# Lasso regression ------------
require(glmnet)

# Change the train and test to matrix
train_matrix <- model.matrix(~.,train)[,2:11]
test_matrix <- model.matrix(~., test)[,2:11]

# cross validation with the lasso regression
cv.lasso <- cv.glmnet(x=train_matrix[,1:9], y = train_matrix[,10], alpha = 1, nlambda = 1000)
minlambda <- cv.lasso$lambda.min
# plot the MSE and lambda
plot.cv.glmnet(cv.lasso)
# build the model with lambda which minimize the MSE
model_lasso <- glmnet(x=train_matrix[,1:9], y=train_matrix[,10], lambda = cv.lasso$lambda.min)
# Which factors influence the value???
model_lasso$beta

# predict the value based on Lasso regression
pred <- predict(model_lasso, newx = test_matrix[,1:9])

# plot the distance between predictions and actuals
plot(pred-test_matrix[,10])

# Build a function to calculate Mean-Square-Error
MSE <- function(x, y) {
  return(mean((x - y))^2)
}

# MSE of lasso regression
MSE_lasso <- MSE(pred, test_matrix[,10])

## lasso regression after normalization -----------
#housing_scale <- scale(housing[,1:9])
## add the column back to the housing_scale
#housing_scale <- cbind(housing_scale,housing[,10])
#train_scale <- model.matrix(~., data.frame(housing_scale[ind==1,] ))[,2:11]
#test_scale <- model.matrix(~., data.frame(housing_scale[ind==2,] ))[,2:11]
#cv.lasso.scale <- cv.glmnet(x=train_scale[,1:9], y=train_scale[,10], alpha=1,nlambda=1000)
#plot.cv.glmnet(cv.lasso.scale)
#model_lasso_scale <- glmnet(x=train_scale[,1:9], y=train_scale[,10], lambda = cv.lasso.scale$lambda.min)
#pred_scale <- predict(model_lasso_scale, newx = test_scale[,1:9])
#lasso_scale_MSE <- MSE(pred_scale, test_scale[,10])
#plot(pred - test_matrix[,10])

# Regression Trees ----------
require(rpart)
require(rpart.plot)

# build the decision tree model
model_tree <- rpart(median_house_value~., data = train, method = 'anova')

# plot the decision tree
rpart.plot(model_tree)

# predic based on decision tree model
pred_tree <- predict(model_tree, test[,1:9])

# plot the distance between predictions and actuals
plot(pred_tree - test[,10])

# Calculate the MSE of decision tree
tree_MSE <- MSE(pred_tree, test[,10])

# predic housing price range=============
quant <- quantile(housing$median_house_value, seq(from=0, to=1, by=0.1)) %>% rev()
# change the price to a price range
for (i in 1:10) {
  if (i == 10) {
    housing$median_house_value[housing$median_house_value > 11] <- 1
  }
  else {
    housing$median_house_value[housing$median_house_value > quant[i + 1]] <- 11 - i
  }
}

# set the targets to factors
housing$median_house_value <- factor(housing$median_house_value)

# split the train and test data
train <- housing[ind == 1, ]
test <- housing[ind == 2, ]


# naive Bayes===========
require(e1071)


# build the naive bayes classifier model
model_Bayes <- naiveBayes(median_house_value~., data=train)

# predict
pred_class <- predict(model_Bayes, newdata = test[,1:9])

# print the confusion matrix
table(pred_class, test$median_house_value)

### heat map
# Decision Tree --------
require(rpart)

# use the decision tree to classify
model_Dtree <- rpart(median_house_value~., data=train, method = 'class')

# predict
pred_DT <- predict(model_Dtree, newdata = test[,1:9])

# find the the highest probability to one classifier
class_DT <- apply(pred_DT, 1, which.max)

# print the confusion matrix
table(class_DT, test$median_house_value)

# Random FOrest ----------
require(randomForest)
model_RF <- randomForest(x= as.matrix(train[,1:9]), y=train[,10], ntree = 100)

# predict
pred_RF <- predict(model_RF, test[,1:9])

# confusion matrix
table(pred_RF, test$median_house_value)
