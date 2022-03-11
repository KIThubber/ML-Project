######polynominale Regression
axdegree <- 3
cv.errors          <- rep(0,maxdegree)
glm.polynomial.fit <- vector(mode="list", length=8)


colnames <- toString(colnames(port))
formula <- paste('G_average ~ poly(', colnames, ',degree=i, raw=TRUE)')

memory.limit(9999999)


for (i in 1:maxdegree){
  
  glm.polynomial.fit[[i]] <- glm(   
    formula = formula,  
    data    = traindata
  )
  
  cv.error <- cv.glm(
    data    = traindata,
    glmfit  = glm.polynomial.fit[[i]],
    K       = 5
  )
  
  cv.errors[[i]] <- cv.error$delta[1]  
}
plot(x=port$G_average, port$age, col="red")

degree <- 1:maxdegree
plot(x = degree, y = cv.errors)
lines(x = degree, y = cv.errors)