#Condition Monitoring of Wind Turbine 
#We Will be using a randomForest Approach for the classification problem
#With this problem we aim to classify the faults in a wind turbine system
#suign parameter 'Max', 'Min', 'Mean', 'Skewness', 'Kurtosis' , 'Standard
#Deviation'

dir <- "~/Fault_Proj/"
data.train <- read.csv(paste0(dir,"train.csv"))
data.test <- read.csv(paste0(dir,"test.csv"))
data.train <- data.train[,1:7]
data.test <- data.test[,1:6]
library(randomForest)

fit <- randomForest(Class~., data=data.train, ntree=10000)
pred.class <- predict(fit,data.test)


#FBFG:20
#FBHG:20
#HBFG:20
#HBHG:31

pred.class_t <- c(rep("FBHG",20),rep("FBFG",20),
                  rep("HBFG",20), rep("HBHG",31))

sum(pred.class==pred.class_t)

