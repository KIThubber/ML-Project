########### lin-reg mit nicht skalierten Daten ############

set.seed(42)

lm.fit <- lm( 
  formula =  G_average ~ .,  
  data    = traindata
)


####nice to know
summary(lm.fit)
sum.fit <- as.data.frame(summary(lm.fit)$coefficients)

sum.fit.ordered <- sum.fit[order(abs(sum.fit$Estimate)),]
sum.fit.ordered

####Trainingsfehler
train.mqa.lm <- mean(
  (traindata$G_average - predict(lm.fit, newdata=traindata))^2
)

train.mqa.lm


######Testfehler
test.mqa.lm <- mean(
  (testdata$G_average - predict(lm.fit, newdata=testdata))^2
)

test.mqa.lm



#####cross validation
cv.error <- cv.glm(
  data    = traindata,
  glmfit  = lm.fit,
  K       = 10
)


cv.error
cv.error$delta[1]




########lin-reg mit skalierten Daten
set.seed(42)

lm_scaled.fit <- lm( 
  formula =  G_average ~ .,  
  data    = traindata_scaled
)

cv.error <- cv.glm(
  data    = traindata,
  glmfit  = lm_scaled.fit,
  K       = 10
)

cv.error$delta

summary(lm_scaled.fit)
sum.fit <- as.data.frame(summary(lm_scaled.fit)$coefficients)

sum.fit.ordered_scaled <- sum.fit[order(abs(sum.fit$Estimate)),]
sum.fit.ordered_scaled

####Trainingsfehler
train.mqa.lm_scaled <- mean(
  (traindata_scaled$G_average - predict(lm_scaled.fit, newdata_scaled=traindata_scaled))^2
)

train.mqa.lm_scaled


######Testfehler
test.mqa.lm_scaled <- mean(
  (testdata_scaled$G_average - predict(lm_scaled.fit, newdata=testdata_scaled))^2
)

test.mqa.lm_scaled









