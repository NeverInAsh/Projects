data.dir <- "~/Analytics Vidhya Competition/Time Series_Finance/Data/"
data.train <- read.csv(paste0(data.dir,"Train.csv"),stringsAsFactors = F)
data.test <- read.csv(paste0(data.dir,"Test.csv"),stringsAsFactors = F)
str(data.train)
library(stringr)
library(foreach)
library(doSNOW)
c1 <- makeCluster(2)
registerDoSNOW(c1)

date.time.df <- foreach(i=1:nrow(data.train),.packages=c("foreach","stringr"),.combine=rbind)%dopar%{
  Date <- unlist(str_extract_all(data.train$Datetime[i],
                "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+"))
  Time <- as.numeric(unlist(str_extract_all(data.train$Datetime[i],
                                 "[[:digit:]]+"))[4])
  data.frame(Date,Time)
}

#Making them as date objects
date.time.df[,1] <- as.Date(date.time.df$Date,format='%d-%m-%Y')
data.train <- cbind(data.train,date.time.df)

#Test Frame

date.time.df.test <- foreach(i=1:nrow(data.test),.packages=c("foreach","stringr"),.combine=rbind)%dopar%{
  Date <- unlist(str_extract_all(data.test$Datetime[i],
                                 "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+"))
  Time <- as.numeric(unlist(str_extract_all(data.test$Datetime[i],
                                            "[[:digit:]]+"))[4])
  data.frame(Date,Time)
}

#Making them as date objects
date.time.df.test[,1] <- as.Date(date.time.df.test$Date,format='%d-%m-%Y')
data.test <- cbind(data.test,date.time.df.test)
#Vector containing elements which are diff. from train data in no of days
diff.test <- unique(data.test$Date - data.train$Date[1])


#I plan to model two varibles as predictor variables for the count
#1) diff:- diff in days from start of operation
#2) divide hour of day in 4 classes 
diff <- rep(NA,nrow(data.train)/24)
no.days <- (nrow(data.train)/24)
data.train$diff <- rep(0,nrow(data.train))
for(i in 1:no.days-1){
  data.train$diff[(i*24+1):((i+1)*24)] <- rep(i,24)
}
  
count.day <- foreach(i=1:no.days-1,.combine = c)%do%{
  sum(data.train$Count[(i*24+1):((i+1)*24)])
}

day <- unique(data.train$diff)

#Classification



data.train$Class <- rep(NA,nrow(data.train))
for(i in 1:nrow(data.train)){
  if(data.train$Time[i]>=0 & data.train$Time[i]<=5){
    data.train$Class[i] <- "A"
  }
  else if(data.train$Time[i]>=6 & data.train$Time[i]<=11){
    data.train$Class[i] <- "B"
  }
  else if(data.train$Time[i]>=12 & data.train$Time[i]<=17){
    data.train$Class[i] <- "C"
  }
  else if(data.train$Time[i]>=18 & data.train$Time[i]<=23){
    data.train$Class[i] <- "D"
  }
}

class.vec <- seq(1,nrow(data.train),6)
#now let us assign weigths to each class
weights<- foreach(i=class.vec,.combine = c)%dopar%{
  if(data.train$Class[i]=="A"){
    weight <- sum(data.train$Count[i:(i+5)])
  }
  else if(data.train$Class[i]=="B"){
    weight <- sum(data.train$Count[i:(i+5)])
  }
  else if(data.train$Class[i]=="C"){
    weight <- sum(data.train$Count[i:(i+5)])
  }
  else if(data.train$Class[i]=="D"){
    weight <- sum(data.train$Count[i:(i+5)])
  }  
  weight
}

weight.df <- foreach(i=0:(length(count.day)-1),.combine = rbind)%do%{
  weights[(i*4+1):((i+1)*4)]/count.day[i+1]
}
#weight.class :- Avg of all the weights of a particualr col
weight.class <- colMeans(weight.df)
#lm/df : data frame consisting of predictor and output variable
lm.df <- data.frame(Count=log(count.day),Diff=unique(data.train$diff))
lm1 <- lm(Count~Diff, data = lm.df)
lm1$coefficients


#Developing the test set
predictcounts <- predict(lm1,data.frame(Diff=diff.test))
count.day.test <- exp(predictcounts)
weight.class
countvec.test <- foreach(i=1:length(count.day.test),.combine = c)%dopar%{
  c(rep((count.day.test[i]*weight.class[1])/6,6),
    rep((count.day.test[i]*weight.class[2])/6,6),
    rep((count.day.test[i]*weight.class[3])/6,6),
    rep((count.day.test[i]*weight.class[4])/6,6)) 
}

#2nd attempt
weight.time <- numeric(0)
#We try to calc weighted mean among the classes as well
weight.time<-foreach(i=c(0,6,12,18),.combine = rbind)%do%{
  for(j in i:(i+5)){
    vec <- sum(data.train[data.train$Time==j,2])/sum(data.train[data.train$Class==unique(data.train$Class)[(i/6)+1],2])
    weight.time <- c(weight.time,vec)
  }
  weight.time
}
#developing test set
countvec.test.2 <- foreach(i=1:length(count.day.test),.combine = c)%dopar%{
  c(((count.day.test[i]*weight.class[1]))*weight.time[1:6],
    ((count.day.test[i]*weight.class[2]))*weight.time[7:12],
    ((count.day.test[i]*weight.class[3]))*weight.time[13:18],
    ((count.day.test[i]*weight.class[4]))*weight.time[19:24]) 
}


set.seed(404)
index <- sample(nrow(data.train),0.6*nrow(data.train))
data.test.new <- data.train[index,]
data.train.new <- data.train[index,]

#3
#Using Regression with Neural Networks
#Scaling the data is important
#library(caret)
#procValues <- preProcess(lm.df[,2], method = c('center','scale'))
#scaledtrain <- predict(procValues,lm.df)
#scaledtest <- predict(procValues,data.frame(diff.test[,1]))

#We need to scale them at diff levels
max_c <- max(lm.df[,1])
min_c <- min(lm.df[,1])
scaled_c <- as.data.frame(scale(lm.df[,1], center = min_c, scale = max_c - min_c))

max_d <- max(lm.df[,2])
min_d <- min(lm.df[,2])
scaled_d <- as.data.frame(scale(lm.df[,2], center = min_d, scale = max_d - min_d))
train.nnet <- data.frame(scaled_c,scaled_d)
colnames(train.nnet) <- c("Count","Diff")
#diff.test scaled
scaled_t <- as.data.frame(scale(diff.test,center = min_d, scale = max_d - min_d))
colnames(scaled_t) <- "Diff"
library(neuralnet)
n <- names(train.nnet)
f <- as.formula(paste("Count ~ ", paste(n[!n %in% "Count"], collapse = " + ")))
nn <- neuralnet(f,data=train.nnet,hidden=c(5,3),linear.output=T)
pr.nn <- compute(nn,scaled_t[,1])
pr.nn_ <- pr.nn$net.result*(max_c-min_c)+min_c
count.test.nnet<- exp(pr.nn_)
#now diff.test contains the vector which shows day count from starting
countvec.test.3 <- foreach(i=1:length(count.test.nnet),.combine = c)%dopar%{
  c(((count.test.nnet[i]*weight.class[1]))*weight.time[1:6],
    ((count.test.nnet[i]*weight.class[2]))*weight.time[7:12],
    ((count.test.nnet[i]*weight.class[3]))*weight.time[13:18],
    ((count.test.nnet[i]*weight.class[4]))*weight.time[19:24]) 
}



data.test <- read.csv(paste0(data.dir,"Test.csv"),stringsAsFactors = F)
data.test$Count <- countvec.test.3
write.csv(data.test,"submission_darkrider19_4.csv")






