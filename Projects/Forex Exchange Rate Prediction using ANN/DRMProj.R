#install.packages('stochvol')
library(stochvol); library(foreach)
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

#Next we are going to take GDP(@ factor cost) we will base the values 
#on 2004-2005 due to unavailability of data
GDP.df <- read.csv(paste0(data.dir,'GDP.csv'),stringsAsFactors = F)

