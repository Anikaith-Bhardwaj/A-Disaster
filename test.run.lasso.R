# Import database
library(readr)
disaster <- read_csv("C:/Users/anika/Downloads/afghan_ND_data.csv", 
                           col_types = cols(Persons_killed = col_number(), 
                                            Persons_injured = col_number(), Families_affected = col_number(), 
                                            Individuals_affected = col_number(), 
                                            Houses_damaged = col_number(), Houses_destroyed = col_number()))
#View(afghan_ND_data)

#### Model for predicting number of individuals affected ####

#remove columns we don't need:
# we will do this randomly, so set seed for reproducability
set.seed(69)
disaster.clean=na.omit(disaster[-c(1,717,903,1498,2073,2304), -c(1:8,15)])
sample.nums = sample(1:2039,replace = F)

#split training and test data
disaster.train=disaster.clean[sample.nums, ]
disaster.test=disaster.clean[-sample.nums, ]

# import glment to implement LASSO
library(glmnet)

#model matrix
train.matrix=(data.matrix(disaster.train[, c(1:3,5:6)]))
y= disaster.train$Individuals_affected

#predict lambda
cv.model=cv.glmnet(x=train.matrix,y=y,alpha=1)
plot(cv.model)
best.lambda=cv.model$lambda.min

#best model using lambda
best.model=glmnet(x=train.matrix,y=y,alpha=1,lambda=best.lambda)

#testing the predictive power of the model
test.matrix=data.matrix(disaster.test[, c(1:3,5:6)])
predict(best.model, s=best.lambda, newx=test.matrix)
y.predicted= predict(best.model, s=best.lambda, newx=train.matrix)

#test model MAE
library(Metrics)
mae(y,y.predicted)
mape(y,y.predicted)
mse(y,y.predicted)

prediction.test=matrix(data=c(0,0,0,0,7),nrow = 1,ncol=5)

predict(best.model,s=best.lambda,newx = prediction.test)

#### Model for predicting number of houses damaged ####

#remove columns we don't need:
# we will do this randomly, so set seed for reproducability
set.seed(69)
house.clean=na.omit(disaster[-c(1,717,903,1498,2073,2304), -c(1:8,15)])
house.nums = sample(1:2039,replace = F)

#split training and test data
house.train=house.clean[house.nums, ]
house.test=house.clean[-house.nums, ]

# import glment to implement LASSO
library(glmnet)

#model matrix
house.matrix.train=(data.matrix(house.train[, c(1:4,6)]))
house.y= house.train$Houses_damaged

#predict lambda
cv.model=cv.glmnet(x=house.matrix.train,y=y,alpha=1)
plot(cv.model)
house.best.lambda=cv.model$lambda.min

#best model using lambda
house.best.model=glmnet(x=house.matrix.train,y=house.y,alpha=1,lambda=house.best.lambda)

#testing the predictive power of the model
house.matrix.test=data.matrix(house.test[, c(1:4, 6)])
predict(house.best.model, s=house.best.lambda, newx=house.matrix.test)
house.y.predicted= predict(house.best.model, s=house.best.lambda, newx=house.matrix.train)

#test model MAE
library(Metrics)
mae(house.y,house.y.predicted)
mape(house.y,house.y.predicted)
mse(house.y,house.y.predicted)

prediction.test.house=matrix(data=c(0,0,29,230,0),nrow = 1,ncol=5)

predict(house.best.model,s=house.best.lambda,newx = prediction.test.house)

