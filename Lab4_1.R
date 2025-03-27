library(MASS)
library(neuralnet)
library(boot)

set.seed(500)
data <- Boston
apply(data,2,function(x) sum(is.na(x)))

#impartim setul de date in 2 subseturi: antrenare(75%) si testare (25%)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

#estimam un model de regresie liniara
lm.fit <- glm(medv~., data=train)
summary(lm.fit)

#folosind modelul estimat calculam valorile variabilei raspuns pentru setulde date de test
pr.lm <- predict(lm.fit,test)

#calculam MSE pentru setul de date de test
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm


#pregatim datele pentru a fi utilizate cu o retea neuronala
maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#impartim setul de date in doua subseturi: unul de antrenare si altul de test
train_ <- scaled[index,]
test_ <- scaled[-index,]

#construim reteaua neuronala si estimam parametrii
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),rep=10, linear.output=T)

#reprezentare grafica a retelei
plot(nn)

#aplicam reteaua neuronala pentru setul de date de test
pr.nn <- neuralnet::compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

#calculam MSE pentru setul de date de test
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

#comparatie intre performatele modelului liniar si al retelei neuronale
par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Valori reale vs prezise NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Valori reale vs prezise de lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


plot(test$medv,pr.nn_,col='red',main='Valori reale vs prezise NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


#validare incrucisata cu K=10

#incepem cu modelul liniar
set.seed(500)
lm.fit <- glm(medv~.,data=data)
cross_valid.lm <- cv.glm(data,lm.fit,K=10)

cross_valid.lm$delta[1]

#acum trecem la reteaua neuronala

cv.error <- NULL
k <- 10
library(plyr)
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,rep=10, hidden=c(5,3),linear.output=T)
  
  pr.nn <- neuralnet::compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)


cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan', border='blue',names='CV error (MSE)',
        main='Eroarea pentru RNA',horizontal=TRUE)
