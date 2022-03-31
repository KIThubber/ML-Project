######polynominale Regression
set.seed(42)
maxdegree <- 4
cv.errors          <- rep(0,maxdegree)
glm.polynomial.fit <- vector(mode="list", length=8)


colnames <- toString(colnames(port))
formula <- paste('G_average ~ poly(', colnames, ',degree=i, raw=TRUE)')
formula

memory.limit(9999999)

nummerischecolnames <- paste(toString(colnames(cordata)))
nummerischecolnames

port$health

paste(nummerischecolnames)

for (i in 1:maxdegree){
  
  glm.polynomial.fit[[i]] <- glm(   
    formula = G_average ~ poly(nummerischecolnames, degree=i, raw=TRUE),  
    data    = traindata
  )
  
  cv.error <- cv.glm(
    data    = traindata,
    glmfit  = glm.polynomial.fit[[i]],
    K       = 5
  )
  
  cv.errors[[i]] <- cv.error$delta[1]  
}

glm.polynomial.fit[2]

plot(x=port$G_average, port$age, col="red")

degree <- 1:maxdegree
plot(x = degree, y = cv.errors)
lines(x = degree, y = cv.errors)

View(port)

