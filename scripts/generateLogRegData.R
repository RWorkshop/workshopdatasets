n=250
N=15000

Y = sample(c(0,1),n,replace=T,prob=c(0.75,0.25))

AICs = numeric(N)
correct =numeric(N)

for(i in 1:N){
i=3538
set.seed(i)
X1 <- sample(100:300,n,replace=T)
X2 <- round(rnorm(n,100,10),3)
X3 <- sample(200:600,n,replace=T)
X4 <- round(rnorm(n,280,50),3)
X5 <- round(rnorm(n,150,25),3)

myData <-data.frame(Y,X1,X2,X3,X4,X5)

AICs[i] <- AIC(glm(Y~X1+X2+X3+X4+X5,data=myData,family="binomial"))


glmFit   <- glm(Y~X1+X2+X3+X4+X5,data=myData,family="binomial")
predicts <- floor(2*predict(glmFit,myData,type="response"))

correct[i] <- sum(Y==predicts)
}
