##Ridge Regression

lambda <- 10^seq( from = 5, to = -5, length = 100) #Fallende Folge! vgl. ?glmnet

ridge.cv.out <- cv.glmnet(
  x     = X.train,
  y     = y.train,
  alpha = 0,
  lambda = lambda
)


ridge.cv.out
plot(ridge.cv.out)
ridge.best.lambda <- ridge.cv.out$lambda.min
ridge.best.lambda

ridge.fit <- glmnet(
  x      = X.train,
  y      = y.train,
  alpha  = 0,
  lambda = ridge.best.lambda
)

coef(ridge.fit)

#train 5,13
ridge.prediction.train <- predict(
  object = ridge.fit,
  newx   = X.train
)
ridge.prediction.train

mqa.ridge.train <- mean( (ridge.prediction.train - y.train)^2 )
mqa.ridge.train 

#test 4,768
ridge.prediction.test <- predict(
  object = ridge.fit,
  newx   = X.test
)
ridge.prediction.test

mqa.ridge.test <- mean( (ridge.prediction.test - y.test)^2 )
mqa.ridge.test 