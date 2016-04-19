install.packages('fNonlinear')
library(fNonlinear); library(foreach); library(TTR)
data.dir <- '~/DRM/Data/'
#Variable for no of years
Year <- 14:0
fx.rate <- read.csv(paste0(data.dir,'FX Rate.csv'),stringsAsFactors = F)
fx.rate$Date <- as.POSIXlt(as.Date(fx.rate$Date, format='%d-%b-%Y'))
fx.rate$Quarter <- quarters(fx.rate$Date)
fx.rate$Year <- (fx.rate$Date)$year + 1900
FX.df <- as.data.frame(matrix(NA,nrow = length(Year),ncol = 16))
rownames(FX.df) <- as.character(2014:2000)
colnames(FX.df) <- c('USD.Q1','USD.Q2','USD.Q3','USD.Q4',
                     'Pound.Q1','Pound.Q2','Pound.Q3','Pound.Q4',
                     'Euro.Q1','Euro.Q2','Euro.Q3','Euro.Q4',
                     'Yen.Q1','Yen.Q2','Yen.Q3','Yen.Q4')
foreach(i = seq_along(Year))%do%{
  FX.df[i,'USD.Q1'] <- mean(fx.rate[which(fx.rate$Quarter=='Q1'& fx.rate$Year==Year[i]),'USD'])   
  FX.df[i,'USD.Q2'] <- mean(fx.rate[which(fx.rate$Quarter=='Q2'& fx.rate$Year==Year[i]),'USD'])
  FX.df[i,'USD.Q3'] <- mean(fx.rate[which(fx.rate$Quarter=='Q3'& fx.rate$Year==Year[i]),'USD'])
  FX.df[i,'USD.Q4'] <- mean(fx.rate[which(fx.rate$Quarter=='Q4'& fx.rate$Year==Year[i]),'USD'])
  
  FX.df[i,'Pound.Q1'] <- mean(fx.rate[which(fx.rate$Quarter=='Q1'& fx.rate$Year==Year[i]),'Pound'])   
  FX.df[i,'Pound.Q2'] <- mean(fx.rate[which(fx.rate$Quarter=='Q2'& fx.rate$Year==Year[i]),'Pound'])
  FX.df[i,'Pound.Q3'] <- mean(fx.rate[which(fx.rate$Quarter=='Q3'& fx.rate$Year==Year[i]),'Pound'])
  FX.df[i,'Pound.Q4'] <- mean(fx.rate[which(fx.rate$Quarter=='Q4'& fx.rate$Year==Year[i]),'Pound'])
  
  FX.df[i,'Euro.Q1'] <- mean(fx.rate[which(fx.rate$Quarter=='Q1'& fx.rate$Year==Year[i]),'Euro'])   
  FX.df[i,'Euro.Q2'] <- mean(fx.rate[which(fx.rate$Quarter=='Q2'& fx.rate$Year==Year[i]),'Euro'])
  FX.df[i,'Euro.Q3'] <- mean(fx.rate[which(fx.rate$Quarter=='Q3'& fx.rate$Year==Year[i]),'Euro'])
  FX.df[i,'Euro.Q4'] <- mean(fx.rate[which(fx.rate$Quarter=='Q4'& fx.rate$Year==Year[i]),'Euro'])
  
  FX.df[i,'Yen.Q1'] <- mean(fx.rate[which(fx.rate$Quarter=='Q1'& fx.rate$Year==Year[i]),'Yen'])   
  FX.df[i,'Yen.Q2'] <- mean(fx.rate[which(fx.rate$Quarter=='Q2'& fx.rate$Year==Year[i]),'Yen'])
  FX.df[i,'Yen.Q3'] <- mean(fx.rate[which(fx.rate$Quarter=='Q3'& fx.rate$Year==Year[i]),'Yen'])
  FX.df[i,'Yen.Q4'] <- mean(fx.rate[which(fx.rate$Quarter=='Q4'& fx.rate$Year==Year[i]),'Yen'])

}
rownames(FX.df) <- NULL
FX.df <- cbind(Year=2014:2000,FX.df)
rm(fx.rate)
#Importing WPI Index Data with base year 1993-1994(due to unavailability of a data
#a factor of 1.866 is used to change the base value)
WPI <- read.csv(paste0(data.dir,'wpi.csv'),stringsAsFactors = F)
WPI$Date <- as.POSIXlt(as.Date(WPI$Date, format='%d-%b-%Y'))
WPI$Quarter <- quarters(WPI$Date)
WPI$Year <- (WPI$Date)$year+1900 
WPI.df <- as.data.frame(matrix(NA,nrow = length(Year),ncol = 4))
rownames(WPI.df) <- 2014:2000
colnames(WPI.df) <- c('WPI.Q1','WPI.Q2','WPI.Q3','WPI.Q4')
foreach(i = seq_along(Year))%do%{
  WPI.df[i,'WPI.Q1'] <- mean(WPI[which(WPI$Quarter=='Q1' & WPI$Year==Year[i]),'WPI.Index'])
  WPI.df[i,'WPI.Q2'] <- mean(WPI[which(WPI$Quarter=='Q2' & WPI$Year==Year[i]),'WPI.Index'])
  WPI.df[i,'WPI.Q3'] <- mean(WPI[which(WPI$Quarter=='Q3' & WPI$Year==Year[i]),'WPI.Index'])
  WPI.df[i,'WPI.Q4'] <- mean(WPI[which(WPI$Quarter=='Q4' & WPI$Year==Year[i]),'WPI.Index'])
}
rownames(WPI.df) <- NULL
WPI.df <- cbind(Year=2014:2000,WPI.df)

#Next we are going to take GDP(@ factor cost) we will base the values 
#on 2004-2005 due to unavailability of data
GDP.df <- read.csv(paste0(data.dir,'GDP.csv'),stringsAsFactors = F)
GDP.df <- GDP.df[1:15,1:5]

#Call Money Market Rate
RATE.df <- read.csv(paste0(data.dir,'Market_Rate.csv'),stringsAsFactors = F)
RATE.df <- RATE.df[,1:5]
final.df.Q1 <- cbind(FX.df$USD.Q1,RATE.df$Rate.Q1,WPI.df$WPI.Q1,GDP.df$GDP.Q1)
final.df.Q2 <- cbind(FX.df$USD.Q2,RATE.df$Rate.Q2,WPI.df$WPI.Q2,GDP.df$GDP.Q2)
final.df.Q3 <- cbind(FX.df$USD.Q3,RATE.df$Rate.Q3,WPI.df$WPI.Q3,GDP.df$GDP.Q3)
final.df.Q4 <- cbind(FX.df$USD.Q4,RATE.df$Rate.Q4,WPI.df$WPI.Q4,GDP.df$GDP.Q4)
final.df <- rbind(final.df.Q1,final.df.Q2,final.df.Q3,final.df.Q4)
colnames(final.df) <- c('FX.Rate', 'Rate', 'WPI', 'GDP')
#Now after Data Preparation and Processing is done

#Volatility and Non-Linearity Tests

#BDS Test for Non-Linearity
#The bdsTest test examines the spatial dependence of the observed 
#series. To do this, the series is embedded in m-space and the 
#dependence of x is examined by counting near points. Points for
#which the distance is less than eps are called near. The BDS test 
#statistic is asymptotically standard Normal

bdsTest(final.df[,1], m = 3, eps = NULL, title = NULL, description = NULL)


volatility(final.df[,1],calc="close")



#Artificial Neural Networks
library(neuralnet)
scaled.df <- function(data){
  maxs <- apply(data, 2, max) 
  mins <- apply(data, 2, min)
  scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
  #Reserving 80% for training my data and 20% for testing
  train <- scaled[13:60,]
  test <- scaled[1:12,]
  colnames(train) <- c('FX.Rate', 'Rate', 'WPI', 'GDP')
  colnames(test) <- c('FX.Rate', 'Rate', 'WPI', 'GDP')
  list(Train = train, Test = test)
}
#Q1 <- scaled.df(final.df.Q1)
#Q2 <- scaled.df(final.df.Q2)
#Q3 <- scaled.df(final.df.Q3)
#Q4 <- scaled.df(final.df.Q4)
final <- scaled.df(final.df)
#rm(final.df.Q4,final.df.Q3,final.df.Q2,final.df.Q1)
# Colnames
n <- c('FX.Rate', 'Rate', 'WPI', 'GDP')
f <- as.formula(paste("FX.Rate ~", 
                      paste(n[!n %in% "FX.Rate"], collapse = " + ")))

nn <- neuralnet(f,data=final$Train,hidden=c(30,20),linear.output=T)
plot(nn)
#The black lines show the connections between each layer and the 
#weights on each connection while the blue lines show the bias term 
#added in each step. 
#The bias can be thought as the intercept of a linear model.

#predicting for the years 2012,2013,2014
pr.nn <- compute(nn,final$Test[,2:4])

MSE.nn <- mean(pr.nn$net.result-final$Test[,1])^2
#Since  data is less we can't conclude anything with such small amount of data
lm.fit <- lm(f,data = final$Train)
lm.predict <- predict(lm.fit, final$Test[,2:4])
MSE.lm <- mean(lm.predict-final$Test[,1])^2
summary(lm.fit)
#It turns out that MSE of linear model is ~10 times that of a neural net

plot(pr.nn$net.result,final$Test[,1],
     main = 'Relationship B/W Nnet Pred and Actual Values',
     xlab = 'Predicted Neural Network Values',
     ylab = 'Actual Values')
lines(final$Test[,1],final$Test[,1], col='green')

plot(lm.predict,final$Test[,1],
     main = 'Relationship B/W Linear Model Pred and Actual Values',
     xlab = 'Predicted Neural Network Values',
     ylab = 'Actual Values')
lines(final$Test[,1],final$Test[,1], col='green')