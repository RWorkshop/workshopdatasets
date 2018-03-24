n=1600

AgeGroups <- c("18to25","26to40","41to65","over65")

AgeGroup <- sample(AgeGroups,n,replace=T)

Districts <- c("Dublin3","Dublin5","Dublin7","Dublin9","Dublin11")

District <- sample(Districts,n,replace=T,prob=c(0.10,0.20,0.20,0.25,0.25) )

Opinion <- 1:7

Opinion  <- sample(Opinion,n,replace=T,prob=c(0.05,0.10,0.15,0.15,0.25,0.20,0.10) )

OpinionLabel <- c("StronglyAgainst","Against","Sceptical","Neutral","Favorable","Supportive","StroglySupportive")

Opinion <- factor(OpinionLabel[Opinion],levels = OpinionLabel)

Sex <- sample(c("Male","Female"),n,replace=T,prob=c(0.49,0.51) )

i=1

while(chisq.test(Opinion,District)$p.value>0.05 | chisq.test(Opinion,AgeGroup)$p.value>0.05){
cat(i)
i=i+1

Opinion <- 1:7

Opinion  <- sample(Opinion,n,replace=T,prob=c(0.05,0.10,0.15,0.15,0.25,0.20,0.10) )

OpinionLabel <- c("StronglyAgainst","Against","Sceptical","Neutral","Favorable","Supportive","StronglySupportive")

Opinion <- factor(OpinionLabel[Opinion],levels = OpinionLabel)

}




MetroData <- data.frame(District,Sex,AgeGroup,Opinion)
