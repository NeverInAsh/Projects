data.dir <-"~/Analytics Vidhya Competition/Loan Prediction/Data/"
data.train <- read.csv(paste0(data.dir,"train.csv"),stringsAsFactors = F)
data.test <- read.csv(paste0(data.dir,"test.csv"),stringsAsFactors = F)
str(data.train)
data.train[data.train==""] <-NA
data.test[data.test==''] <-NA
#let us convert categorical variable as factors 
convert <- c(2:6,11:13)
data.train[,convert] <- data.frame(apply(data.train[convert], 2, as.factor))
data.test[,convert[-8]] <- data.frame(apply(data.test[convert[-8]], 2, as.factor))

#since the no of NA values are less i prefer removing the rows
#remove_rows <- union(which(is.na(data.train[,9])),
 #                     c(which(is.na(data.train[,10])),
  #                    which(is.na(data.train[,11]))))
#data.train<- data.train[-remove_rows,]
#rm(remove_rows)

#We can follow the above procedure or we can use MICE( Multivariate Imputation
#Via Chained Equation) to impute missing values.

#Note missing values are coded as NA
#install.packages('mice')
library('mice')
#let us see form of missing value present in each variable in a dataset.
View(md.pattern(data.train))
#480 rows are competely perfect...
#Or we can see them graphically
#install.packages('VIM')
library(VIM)
mice_plot <- aggr(data.train, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data.train), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
summary(mice_plot)
#imputing the values
#%m :- No of Imputations(default=5)
#%maxit :- Maximum Iteration
#method:- vector containing method name for each col.
#we will rather use defaultMethod argument to specify default method for 
#each kind
#PMM (Predictive Mean Matching)  - For numeric variables
#logreg(Logistic Regression) - For Binary Variables( with 2 levels)
#polyreg(Bayesian polytomous regression) - For Factor Variables (>= 2 levels)
#Proportional odds model (ordered, >= 2 levels)

imputed_Data <- mice(data.train, m=5, maxit = 50,method = 'pmm',seed = 500)
imputed_Data_t <- mice(data.test, m=5, maxit = 50,method = 'pmm',seed = 500)

#Now since there are 5 imputed data sets we will use any for further work
#We can combine the results using pool()
data.train_imp <- complete(imputed_Data,3)
data.test_imp <- complete(imputed_Data_t,3)


hist(yjPower(data.train_imp$CoapplicantIncome,
             coef(coappinc.pow)),
     main="Applicant Income",
     xlab="Applicant No",
     ylab="Relative frequency",
     breaks=50,
     col="lightblue",
     freq=FALSE, # freq=FALSE means plot density, not counts
     xaxt="n") # xaxt="n" means "x axis tick marks == no"

#Now by using the above analysis we can see that the data distribution is 
#non normal we can generalize this by box-cox transformaton
#Three Variables:-ApplicantIncome,Co-applicantIncome,LoanAmount
#isntall Rcpp 
library(car)
appinc.pow <- powerTransform(data.train_imp$ApplicantIncome)
#his is is Box-Cox transformation of U+1 for nonnegative values, and of |U|+1 with parameter 
#2-lambda for U negative.
coappinc.pow <- powerTransform(as.integer(data.train_imp$CoapplicantIncome),family = 'yjPower')
loaamt.pow <- powerTransform(data.train_imp$LoanAmount)

#Going for the transformation
data.train_imp$ApplicantIncome <- bcPower(data.train_imp$ApplicantIncome,
                                      coef(appinc.pow))
data.test_imp$ApplicantIncome <- yjPower(data.test_imp$ApplicantIncome,
                                      coef(appinc.pow))
data.train_imp$CoapplicantIncome <- yjPower(data.train_imp$CoapplicantIncome,
                                        coef(coappinc.pow))
data.test_imp$CoapplicantIncome <- yjPower(data.test_imp$CoapplicantIncome,
                                       coef(coappinc.pow))
data.train_imp$LoanAmount <- bcPower(data.train_imp$LoanAmount,
                                 coef(loaamt.pow))
data.test_imp$LoanAmount <- bcPower(data.test_imp$LoanAmount,
                                coef(loaamt.pow))

#After Usng the Box_Cox Transformation we have the data normalized
#Let use Boruta package in R to do feature selection which uses mean decrease
#accuracy to determine importance of each feature 

#install.packages("Boruta")
library('Boruta')
set.seed('404')
#Using s3 Method for class 'formula'
#%doTrace :- Verbosity level, here 2 implies status(rejected or not) and 
#reporting each importance source run
boruta.train <- Boruta(Loan_Status~., data=data.train_imp, doTrace=2)
print(boruta.train)

#Loan_Amount_Term is a tentative variable.The tentative attributes 
#will be classified as confirmed or rejected by comparing the median 
#Z score of the attributes with the median Z score of the best 
#shadow attribute

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#Although Dependent is shown as a reduntant variable i would like to use it
#I would also remove Married as a variable (Pseudo included in Dependents)
#alongwith Loan_ID

attr <- getSelectedAttributes(final.boruta, withTentative = F)
attr <- c(attr[3:8],'Dependents')

data.train.f <- data.train_imp[,c(attr,'Loan_Status')]
data.test.f <- data.test_imp[,attr]

rm(attr,convert)

#Now we will begin making a predictive model.
prop.table(table(data.train.f$Credit_History,data.train.f$Loan_Status),
           margin = 1)
#With the above data i conclude that if a person doesn't have Credit
#history it is 93% likely he doesn't get a loan approved
#require(lattice)
histogram(~Loan_Status|Property_Area + Credit_History, data= data.train.f)
#It can be seen that person not having a credit history hardly gets his/her
#loan passed
#Let us split applicant Income by Property_Area and Loan_Status
#app.in.agg <- aggregate(ApplicantIncome~Loan_Status + Property_Area,
 #                      data=data.train.f,mean)

#Now I plan to use Random Forest for classification task
library(randomForest)
fit <- randomForest(Loan_Status~., data=data.train.f, ntree=1000)

summary(fit)
predicted <- predict(fit, data.test.f)
data.test.f$predicted <- NULL
data.test.f <- cbind(data.test.f, Loan_Status=predicted)

prop.table(table(data.test.f$Credit_History,data.test.f$Loan_Status),
           margin = 1)

#We can see random forest algo gives same results

sub.df <- data.frame(Loan_ID=data.test$Loan_ID,
                     Loan_Status=data.test.f$Loan_Status)
rownames(sub.df)<-NULL
write.csv(sub.df, file = 'darkrider19_sub1.csv')


