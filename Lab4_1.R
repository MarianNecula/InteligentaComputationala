library(ISLR)
library(neuralnet)
library(boot)

set.seed(123)
data <- Carseats
apply(data,2,function(x) sum(is.na(x)))

# vizualizam setul de date
View(data)


#optiunea 1 excludem datele non-numerice
data <-data[,c(1:6,8,9)]

#impartim setul de date in 2 subseturi: antrenare(75%) si testare (25%)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

#estimam un model de regresie liniara
lm.fit <- glm(Sales~., data=train)
summary(lm.fit)

#folosind modelul estimat calculam valorile variabilei raspuns pentru setul de date de test
pr.lm <- predict(lm.fit,test)

#calculam MSE pentru setul de date de test
MSE.lm <- sum((pr.lm - test$Sales)^2)/nrow(test)
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
f <- as.formula(paste("Sales ~", paste(n[!n %in% "Sales"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(4,3),rep=10,linear.output=T)

#reprezentare grafica a retelei
plot(nn)

#aplicam reteaua neuronala pentru setul de date de test
pr.nn <- neuralnet::compute(nn,test_)

pr.nn_ <- pr.nn$net.result*(max(data$Sales)-min(data$Sales))+min(data$Sales)
test.r <- (test_$Sales)*(max(data$Sales)-min(data$Sales))+min(data$Sales)

#calculm MSE pentru setul de date de test
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

#comparatie intre performatele modelului liniar si al retelei neuronale
par(mfrow=c(1,2))

plot(test$Sales,pr.nn_,col='red',main='Valori reale vs prezise NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$Sales,pr.lm,col='blue',main='Valori reale vs prezise de lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

par(mfrow=c(1,1))
plot(test$Sales,pr.nn_,col='red',main='Valori reale vs prezise NN',pch=18,cex=0.7)
points(test$Sales,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))





#optiunea 2 - transformam valorile non-numerice in valori numerice
set.seed(123)
data <- Carseats

#transformam coloanele factor in character
data$ShelveLoc <- as.character(data$ShelveLoc)
data$Urban <- as.character(data$Urban)
data$US <- as.character(data$US)


#valori unice pe coloana SelvefLoc
unique(data$ShelveLoc)
#conventie: bad = 0, medium  = 1 good = 2
indexBad <- which(data$ShelveLoc=="Bad")
data[indexBad,'ShelveLoc'] <- 0
indexMedium<-which(data$ShelveLoc=="Medium")
data[indexMedium,'ShelveLoc'] <- 1
indexGood<-which(data$ShelveLoc=="Good")
data[indexGood,'ShelveLoc'] <- 2

#conventie yes = 1 no = 0
indexYes <- which(data$Urban =="Yes")
data[indexYes,'Urban'] <- 1

indexNo <- which(data$Urban =="No")
data[indexNo,'Urban'] <- 0

indexYes <- which(data$US =="Yes")
data[indexYes,'US'] <- 1

indexNo <- which(data$US =="No")
data[indexNo,'US'] <- 0

#transformam cele  3 coloane in tipul numeric
data$ShelveLoc <- as.numeric(data$ShelveLoc)
data$Urban <- as.numeric(data$Urban)
data$US <- as.numeric(data$US)

#impartim setul de date in 2 subseturi: antrenare(75%) si testare (25%)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]

#estimam un model de regresie liniara
lm.fit <- glm(Sales~., data=train)
summary(lm.fit)

#folosind modelul estimat calculam valorile variabilei raspuns pentru setul de date de test
pr.lm <- predict(lm.fit,test)

#calculam MSE pentru setul de date de test
MSE.lm <- sum((pr.lm - test$Sales)^2)/nrow(test)
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
f <- as.formula(paste("Sales ~", paste(n[!n %in% "Sales"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(7), rep = 10, linear.output=T)

#reprezentare grafica a retelei
plot(nn)

#aplicam reteaua neuronala pentru setul de date de test
pr.nn <- neuralnet::compute(nn,test_)

pr.nn_ <- pr.nn$net.result*(max(data$Sales)-min(data$Sales))+min(data$Sales)
test.r <- (test_$Sales)*(max(data$Sales)-min(data$Sales))+min(data$Sales)

#calculm MSE pentru setul de date de test
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))

#comparatie intre performatele modelului liniar si al retelei neuronale
par(mfrow=c(1,2))

plot(test$Sales,pr.nn_,col='red',main='Valori reale vs prezise NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$Sales,pr.lm,col='blue',main='Valori reale vs prezise de lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

par(mfrow=c(1,1))
plot(test$Sales,pr.nn_,col='red',main='Valori reale vs prezise NN',pch=18,cex=0.7)
points(test$Sales,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


#validare incrucisata cu K=10
#incepem cu modelul liniar
set.seed(123)
lm.fit <- glm(Sales~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]


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
  
  nn <- neuralnet(f,data=train.cv,hidden=c(7), rep =10,linear.output=T)
  
  pr.nn <- neuralnet::compute(nn,test.cv)
  pr.nn <- pr.nn$net.result*(max(data$Sales)-min(data$Sales))+min(data$Sales)
  
  test.cv.r <- (test.cv$Sales)*(max(data$Sales)-min(data$Sales))+min(data$Sales)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

mean(cv.error)


cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan', border='blue',names='CV error (MSE)',
        main='Eroarea pentru RNA',horizontal=TRUE)
