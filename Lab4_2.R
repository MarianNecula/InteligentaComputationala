library(neuralnet)

#setul de date de antrenament
TKS=c(20,10,30,20,80,30)
CSS=c(90,20,40,50,50,80)
Job=c(1,0,0,0,1,1)
df=data.frame(TKS,CSS,Job)

#construim reteaua neuronala
nn=neuralnet(Job~TKS+CSS,data=df, hidden=3, act.fct = "logistic", linear.output = FALSE)

plot(nn)


#setul de date de test
TKS_test=c(30,40,85)
CSS_test=c(85,50,40)
Job_test=c(1,0,1)
test=data.frame(TKS_test,CSS_test)

#utilizam reteaua pentru a prezice valorile variabilei Job
predicted_job=compute(nn,test)
predicted_job$net.result
#calculam eroarea de predictie
MSE.nn <- sum((predicted_job$net.result - Job_test)^2)/nrow(test)
MSE.nn

# transformam valorile obtinute in 1 si 0
prob <- predicted_job$net.result
pred <- ifelse(prob > 0.5, 1, 0)
pred

TKS_candidat<-c(80, 30)
CSS_candidat<-c(30, 70)
candidati<-data.frame(TKS_candidat, CSS_candidat)
angajat<-compute(nn, candidati)
angajat

angajat <-ifelse(as.numeric(angajat$net.result) > 0.5, 1,0)
angajat