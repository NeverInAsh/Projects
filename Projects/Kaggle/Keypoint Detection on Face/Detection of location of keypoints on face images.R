#Setting Environmetn Variables
data.dir <- "~/Kaggle_Competition/1-Detect the location of keypoints on face images/Data/"
submission.dir <- "~/Kaggle_Competition/1-Detect the location of keypoints on face images/Submission/"

train.file <- paste0(data.dir,"training.csv")
test.file <- paste0(data.dir,"test.csv")

data.train <- read.csv(train.file, stringsAsFactors = F)
data.test <- read.csv(test.file, stringsAsFactors = F)
#Let us take out the image column
image.train <- data.train$Image
data.train$Image <- NULL
image.test <- data.test$Image
data.test$Image <- NULL
#str(image.train) a chr of length 7049
#Since the vector is long we can implement parallelization into 
#the normal sapply loop
#http://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/
#install.packages("foreach")
#install.packages("doSNOW")
library("foreach")
library("doSNOW")
c1 <- makeCluster(2) #No of Cores
registerDoSNOW(c1)
#Enter loop here
image.train <- foreach(im = image.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
image.test <- foreach(im = image.test, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
save(image.train,image.test,data.train,data.test,mean.patches,
     p,data.parameters,angle.par, dis.par,indices,file='data.Rd')
#Each image is a vector of 96*96 pixels 
#Converting image into a matrix
im1 <- matrix(rev(image.train[5565,]),nrow = 96, ncol = 96)
image(1:96, 1:96, im1, col=gray((0:255)/255))
#We need to adjust for origin
points(96-data.train$nose_tip_x[5565], 96-data.train$nose_tip_y[5565], col='red')

points(96-data.train$left_eye_center_x[5565], 96-data.train$left_eye_center_y[5565],  col="blue")
points(96-data.train$left_eye_inner_corner_x[5565], 96-data.train$left_eye_inner_corner_y[5565], col="blue")
points(96-data.train$left_eye_outer_corner_x[5565], 96-data.train$left_eye_outer_corner_y[5565], col="blue")
points(96-data.train$left_eyebrow_inner_end_x[5565], 96-data.train$left_eyebrow_inner_end_y[5565], col="blue")
points(96-data.train$left_eyebrow_outer_end_x[5565], 96-data.train$left_eyebrow_outer_end_y[5565], col="blue")

points(96-data.train$right_eye_center_x[5565], 96-data.train$right_eye_center_y[5565], col="green")
points(96-data.train$right_eyebrow_outer_end_x[5565], 96-data.train$right_eyebrow_outer_end_y[5565], col="green")
points(96-data.train$right_eye_outer_corner_x[5565], 96-data.train$right_eye_outer_corner_y[5565], col="green")
points(96-data.train$right_eye_inner_corner_x[5565], 96-data.train$right_eye_inner_corner_y[5565], col="green")
points(96-data.train$right_eyebrow_inner_end_x[5565], 96-data.train$right_eyebrow_inner_end_y[5565], col="green")

points(96-data.train$mouth_right_corner_x[5565], 96-data.train$mouth_right_corner_y[5565], col="yellow")
points(96-data.train$mouth_left_corner_x[5565], 96-data.train$mouth_left_corner_y[5565], col="yellow")
points(96-data.train$mouth_center_bottom_lip_x[5565], 96-data.train$mouth_center_bottom_lip_y[5565], col="yellow")
points(96-data.train$mouth_center_top_lip_x[5565], 96-data.train$mouth_center_top_lip_y[5565], col="yellow")

#To see how variable is our data
#points(96-data.train$nose_tip_x, 96-data.train$nose_tip_y, col='red')

# list the coordinates we have to predict
coordinate.names <- gsub("_x", "", names(data.train)[grep("_x", names(data.train))])
patch_size  <- 10
search_size <- 2

# for each one, compute the average patch
#Since parallel workers each operate in a clean R session, we have to
#load "foreach" package in each worker
mean.patches <- foreach(coord = coordinate.names, .packages="foreach")%dopar%{
  cat(sprintf("computing mean patch for %s\n", coord))
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  
  # compute average patch
  patches <- foreach(i = 1:nrow(data.train), .combine=rbind, .packages="foreach")%do%{
    im  <- matrix(data = image.train[i,], nrow=96, ncol=96)
    x   <- data.train[i, coord_x]
    y   <- data.train[i, coord_y]
    x1  <- (x-patch_size)
    x2  <- (x+patch_size)
    y1  <- (y-patch_size)
    y2  <- (y+patch_size)
    if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) )
    {
      as.vector(im[x1:x2, y1:y2])
    }
    else
    {
      NULL
    }
  }
  matrix(data = colMeans(patches), nrow=2*patch_size+1, ncol=2*patch_size+1)
}

# for each coordinate and for each test image, find the position that best correlates with the average patch
p <- foreach(coord_i = 1:length(coordinate.names), .combine=cbind, .packages="foreach") %dopar% {
  # the coordinates we want to predict
  coord   <- coordinate.names[coord_i]
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  
  # the average of them in the training set (our starting point)
  mean_x  <- mean(data.train[, coord_x], na.rm=T)
  mean_y  <- mean(data.train[, coord_y], na.rm=T)
  
  # search space: 'search_size' pixels centered on the average coordinates 
  x1 <- as.integer(mean_x)-search_size
  x2 <- as.integer(mean_x)+search_size
  y1 <- as.integer(mean_y)-search_size
  y2 <- as.integer(mean_y)+search_size
  
  # ensure we only consider patches completely inside the image
  x1 <- ifelse(x1-patch_size<1,  patch_size+1,  x1)
  y1 <- ifelse(y1-patch_size<1,  patch_size+1,  y1)
  x2 <- ifelse(x2+patch_size>96, 96-patch_size, x2)
  y2 <- ifelse(y2+patch_size>96, 96-patch_size, y2)
  
  # build a list of all positions to be tested
  params <- expand.grid(x = x1:x2, y = y1:y2)
  
  # for each image...
  r <- foreach(i = 1:nrow(data.test), .combine=rbind, .packages="foreach") %do% {
    if ((coord_i==1)&&((i %% 100)==0)) { cat(sprintf("%d/%d\n", i, nrow(data.test))) }
    im <- matrix(data = image.test[i,], nrow=96, ncol=96)
    
    # ... compute a score for each position ...
    r  <- foreach(j = 1:nrow(params), .combine=rbind, .packages="foreach") %do% {
      x     <- params$x[j]
      y     <- params$y[j]
      p     <- im[(x-patch_size):(x+patch_size), (y-patch_size):(y+patch_size)]
      score <- cor(as.vector(p), as.vector(mean.patches[[coord_i]]))
      score <- ifelse(is.na(score), 0, score)
      data.frame(x, y, score)
    }
    
    # ... and return the best
    best <- r[which.max(r$score), c("x", "y")]
    
  }
  names(r) <- c(coord_x, coord_y)
  #if(sum(is.na(data.train[,coord_x]))){#we can consider only one dim
   # data.train[,coord_x][is.na(data.train[,coord_x])] <- r$coord_x
   # data.train[,coord_y][is.na(data.train[,coord_y])] <- r$coord_y
  #}
  r
}
#Removing any NA Values
na.avg <- colMeans(p,2)
for(i in 1:length(coordinate.names)){
  coord   <- coordinate.names[i]
  coord_x <- paste(coord, "x", sep="_")
  coord_y <- paste(coord, "y", sep="_")
  data.train[,coord_x][is.na(data.train[,coord_x])] <- as.numeric(na.avg[coord_x])
  data.train[,coord_y][is.na(data.train[,coord_y])] <- as.numeric(na.avg[coord_y])
}


#I plan to parametrize angle and length of the line joining noise center 
#and point on right eye side(say)
#defining x_dis and y_dis thesea re distances
#x_dis <- matrix(rep(NA,7049*15),nrow=7049)
#y_dis <- matrix(rep(NA,7049*15),nrow=7049)
coord_x <- character(0)
coord_y <- character(0)
for(j in 1:length(coordinate.names)){
  coord   <- coordinate.names[j]
  coord_x <- append(coord_x,paste(coord, "x", sep="_"))
  coord_y <- append(coord_y,paste(coord, "y", sep="_"))
  
} 

data.parameters<- foreach(i = 1:nrow(data.train), .combine=rbind, .packages="foreach")%dopar%{
  x_dis <- as.matrix(data.train[i,coord_x]-data.train[i,'nose_tip_x'])     
  y_dis <- as.matrix(data.train[i,coord_y]-data.train[i,'nose_tip_y'])
  angle.frame <- rbind(x_dis,y_dis)
  #Note how coordinates have been handled for angle so what would
  #normally be logical for x>0 & y>0 is now logical for x<0 &y<0
  #as pixels are mage w.r.t nose_tip
  angle <- apply(angle.frame,2,function(x){
    if(x[1]>0 & x[2]>0){angle <- 180*(1+atan(x[2]/x[1])/pi)}
    else if(x[1]<0 & x[2]>0){angle <- 180*(atan(x[2]/x[1])/pi)}
    else if(x[1]<0 & x[2]<0){angle <- 180*(atan(x[2]/x[1])/pi)}
    else if(x[1]>0 & x[2]<0){angle <- 180*(1+atan(x[2]/x[1])/pi)}
    
  })
  dis <- sqrt(x_dis*x_dis + y_dis*y_dis)
  angle <- matrix(unlist(angle),nrow=1,ncol=15)
  data.frame(angle,dis,x_dis,y_dis)
}
x_dis <- data.parameters[,31:45]
y_dis <- data.parameters[,46:60]
names(x_dis)<- coordinate.names
names(y_dis)<- coordinate.names

angle.par <- data.parameters[,1:14] #removed nose_tip
dis.par <- data.parameters[,16:30]
names(dis.par) <- coordinate.names
dis.par$nose_tip <- NULL
names(angle.par) <- coordinate.names[-11]



#now let us adjust for some of the outliers:-
#1) Y coord of mouth params will be less than Y coord of all mouth params
#(all pizel data is wrt to right top corner)
#2) Y coord of mouth params will be greater than Y coord of eyes
#(all pizel data is wrt to right top corner)
# So now we will find indices not following the above rules

#This help reduce the "bias" introduced due to assumption that all the 
#points can be taken as mean of the present data points
indices <- foreach(i = 1:nrow(data.train), .combine=c, .packages="foreach")%dopar%{
  s1 <- sum(data.train[i,coord_y[12:15]] > data.train[i,coord_y[11]])
  s2 <- sum(data.train[i,coord_y[11]] > data.train[i,coord_y[1:10]])
  if(s1!=4 | s2!=10){
    i
  }
}

data.train <- data.train[-indices,]
angle.par <- angle.par[-indices,]
dis.par <- dis.par[-indices,]

#We will divide our training dataset into a new training and testing set
set.seed(2016)
index <- sample(nrow(data.train),0.7*nrow(data.train))
data.train.new <- data.train[index,]
data.test.new <- data.train[-index,]
angle.par.new.train <- angle.par[index,]
dis.par.new.train <- dis.par[index,]
angle.par.new.test <- angle.par[-index,]
dis.par.new.test <- dis.par[-index,]
#Now the parameters are selected as follwoing:-
#mean nose_tip is taken as nose_tip point
#Now normal trigo of y=dis.par*sin(angle.par) and x=dis.par*cos(angle.par)
#this x and y will be matched with x and y of data.test.new and
#appropriate dis and angle corresponding to each point will then be taken
coord_x_new <- coord_x[-11]
coord_y_new <- coord_y[-11]
total.err <- foreach(i= 1:nrow(data.train.new),.combine=rbind,.packages="foreach")%dopar%{
  x_dis <-foreach(j=1:length(coord_x_new),.combine=cbind,.packages="foreach")%do%{
    as.matrix(data.train.new[i,coord_x[j]]-data.train.new[-i,coord_x[j]]) 
    
  }
  y_dis <-foreach(j=1:length(coord_y_new),.combine=cbind,.packages="foreach")%do%{
    as.matrix(data.train.new[i,coord_y[j]]-data.train.new[-i,coord_y[j]]) 
    
  }
  dis <- sqrt(x_dis*x_dis + y_dis*y_dis)
  colSums(dis)
}
rownames(total.err) <- NULL #to make min.err.index a non-named vector
min.err.index <- foreach(i=1:ncol(total.err), .packages = "foreach")%do%{
  which(total.err[,i]==min(total.err[,i]))
}

dis.par.min <- foreach(i=1:ncol(total.err),.combine = c,.packages = "foreach")%dopar%{
  mean(dis.par.new.train[min.err.index[[i]],i])
}
angle.par.min <- foreach(i=1:ncol(total.err),.combine = c,.packages = "foreach")%dopar%{
 ret <- foreach(j= min.err.index[[i]],.packages="foreach",.combine = '+')%dopar%{
    if(i>=1 & i<=10){
      if(angle.par.new.train[j,i]>=0 & angle.par.new.train[j,i]<180){
        c<-angle.par.new.train[j,i]}
    }
   if(i>=11 & i<=14){
     if(angle.par.new.train[j,i]<=0 & angle.par.new.train[j,i]>180){
       c<-angle.par.new.train[j,i]}
   }
   c
 }
 
 ret/length(min.err.index[[i]])
 
}
#na.avg :- variable having means of all points
#X<- (na.avg['nose_tip_x']) + dis.par.min[i]*cos((angle.par.min[i]/180)*pi)
#Y<- (na.avg['nose_tip_y']) + dis.par.min[i]*sin((angle.par.min[i]/180)*pi)
#We have to name the coordinates acc to the coordinate system
XY<-foreach(i=1:length(angle.par.min),.combine=rbind,.packages = "foreach")%dopar%{
  if(angle.par.min[i]>0 & angle.par.min[i]<90){
    X<- (na.avg['nose_tip_x']) - dis.par.min[i]*cos((angle.par.min[i]/180)*pi)
    Y<- (na.avg['nose_tip_y']) - dis.par.min[i]*sin((angle.par.min[i]/180)*pi)
  }
  else if(angle.par.min[i]>90 & angle.par.min[i]<180){
    X<- (na.avg['nose_tip_x']) - dis.par.min[i]*cos((angle.par.min[i]/180)*pi)
    Y<- (na.avg['nose_tip_y']) - dis.par.min[i]*sin((angle.par.min[i]/180)*pi)
  }
  else if(angle.par.min[i]>180 & angle.par.min[i]<270){
    X<- (na.avg['nose_tip_x']) - dis.par.min[i]*cos((angle.par.min[i]/180)*pi)
    Y<- (na.avg['nose_tip_y']) - dis.par.min[i]*sin((angle.par.min[i]/180)*pi)
  }
  else if(angle.par.min[i]<0){
    X<- (na.avg['nose_tip_x']) - dis.par.min[i]*cos((angle.par.min[i]/180)*pi)
    Y<- (na.avg['nose_tip_y']) - dis.par.min[i]*sin((angle.par.min[i]/180)*pi)
  }
  data.frame(X,Y)
}
rownames(XY)<-NULL  #Removing random row naming        
#Adjusting for right top corner
x <- XY$X
y <- XY$Y
#Vector which contains final xy values
x.y <- numeric(0)
for(i in 1:14){
  x.y <- c(x.y,x[i],y[i])
}
x.y <- append(x.y,c(na.avg['nose_tip_x'],na.avg['nose_tip_y']),20)
names(x.y) <- colnames(data.train)

rmse <- foreach(i=1:ncol(data.train), .combine = c, .packages = "foreach")%dopar%{
  sqrt(mean(((data.test.new[,i]-x.y[i]))^2))
}
col.name <- colnames(data.train)
nose_tip_diff <- data.train.new[,21:22]-c(na.avg['nose_tip_x'],na.avg['nose_tip_y'])
colnames(nose_tip_diff) <- c("nose_tip_x_diff","nose_tip_y_diff")
x.y.df <- matrix(x.y,ncol=30,dimnames=list(NULL,col.name))
x.y.df<-apply(x.y.df,2,function(x)rep(x,nrow(data.train.new)))
colnames(x.y.df) <- paste(colnames(x.y.df),"x.y",sep ='_' )
lm.data.set <- cbind(data.train.new,nose_tip_diff,x.y.df)

feature.name <- colnames(lm.data.set)

diff_avg <-  foreach(i=1:nrow(data.train.new),.combine = rbind,.packages = 'foreach')%dopar%{
  as.numeric(data.train.new[i,1:30])-as.numeric(na.avg)
}
diff_avg <- data.frame(diff_avg) 
mean.diff <- apply(diff_avg,2,mean)
linear.model_x<- foreach(i = seq(1,30,2),.combine=cbind,.packages = "foreach")%dopar%{
   lm1 <- lm(lm.data.set[,feature.name[i]]~lm.data.set[,feature.name[31]])
   coeff <- lm1$coefficients
   names(coeff) <- NULL
   coeff
}
linear.model_y<- foreach(i = seq(2,30,2),.combine=cbind,.packages = "foreach")%dopar%{
  lm1 <- lm(lm.data.set[,feature.name[i]]~lm.data.set[,feature.name[32]])
  coeff <- lm1$coefficients
  names(coeff) <- NULL
  coeff
}
colnames(linear.model_x) <- coord_x
colnames(linear.model_y) <- coord_y
#Calculating Mean diff which can then be multiplied by corresponding weights
#To get new X and Y coordinates

#mean.diff.x <- mean(nose_tip_diff$nose_tip_x_diff)
#mean.diff.y <- mean(nose_tip_diff$nose_tip_y_diff)

#Since Data is non-normal seen using qqplot() we can check for all 
#nrow(nose_tip_diff) to see which mean reduces the error to the best
plot(nose_tip_diff$nose_tip_x_diff)
#range lies b/w 10,-20
plot(nose_tip_diff$nose_tip_y_diff)
#range lies b/w 20,-5
diff.x<-nose_tip_diff$nose_tip_x_diff
diff.y<-nose_tip_diff$nose_tip_y_diff
mean.diff.x <- mean(diff.x[which(diff.x<10 & diff.x>-20)])
mean.diff.y <- mean(diff.y[which(diff.y<20 & diff.y>-5)])

linear.model_x[2,] <- linear.model_x[2,]*mean.diff.x
linear.model_y[2,] <- linear.model_y[2,]*mean.diff.y

x.lm <- apply(linear.model_x,2,sum)
y.lm <- apply(linear.model_y,2,sum)
x.y.lm <- numeric(0)
for(i in 1:15){
  x.y.lm <- c(x.y.lm,x.lm[i]+mean.diff[2*i-1],y.lm[i]+mean.diff[2*i])
}

total.err.lm <- foreach(i= 1:nrow(lm.data.set),.combine=rbind,.packages="foreach")%dopar%{
  x_dis <-foreach(j=1:length(coord_x),.combine=cbind,.packages="foreach")%do%{
    as.matrix((lm.data.set[i,coord_x[j]]+ mean.diff[2*j-1])-lm.data.set[-i,coord_x[j]]) 
    
  }
  y_dis <-foreach(j=1:length(coord_y),.combine=cbind,.packages="foreach")%do%{
    as.matrix((lm.data.set[i,coord_y[j]]+ mean.diff[2*j])-lm.data.set[-i,coord_y[j]]) 
    
  }
  dis <- sqrt(x_dis*x_dis + y_dis*y_dis)
  colSums(dis)
}
rownames(total.err) <- NULL #to make min.err.index a non-named vector
min.err.index.lm <- foreach(i=1:ncol(total.err.lm),.combine = c,.packages = "foreach")%do%{
  which.min(total.err.lm[,i])
}

x.y.lm <- numeric(0)
for(i in 1:15){
  x.y.lm <- c(x.y.lm,lm.data.set[min.err.index.lm[i],2*i-1],lm.data.set[min.err.index.lm[i],2*i])
}



#There are mnay bugs in this func
rmsefunc <- function(data1,datapred){
  rmselm <- foreach(i=1:ncol(data1), .combine = c, .packages = "foreach")%dopar%{
    sqrt(mean(((data1[,i]-datapred[i]))^2))
  }
  rmselm
}
sum(rmsefunc(data.test.new.lm,x.y.lm))/30
#rmselm/30 < rmse/30


#Checking with a diff seed

score <- foreach(i=10000:10310,.combine=c,.packages = 'foreach')%dopar%{
  set.seed(i)
  index.lm <- sample(nrow(data.train),0.6*nrow(data.train))
  data.train.new.lm <- data.train[index.lm,]
  data.test.new.lm <- data.train[-index.lm,]
  sum(rmsefunc(data.test.new.lm,x.y.lm))/30
}

lm <- read.csv(paste0(submission.dir, 'submission_darkrider19_lm3.csv'), stringsAsFactors = F)
x.y.lm.sub <- lm$Location[1:30]
names(x.y.lm.sub) <- feature.vector[1:30]

#Getting over with linear modela and using XGBoost Algo
#install.packages('xgboost')
#install.packages('require')
#install.packages('data.table')
#install.packages('magrittr')

#Making Submissions
lookup.id <- read.csv(paste0(data.dir, 'IdLookupTable.csv'), stringsAsFactors = F)
submission <- read.csv(paste0(data.dir, 'SampleSubmission.csv'), stringsAsFactors = F)
sub.col.names <- names(submission)
loo.col.names <- names(lookup.id)
submission$Location <- NULL
feature.vector <- lookup.id$FeatureName
submission$Location <- x.y.lm[feature.vector]
write.csv(submission, file="submission_darkrider19.csv", quote=F, row.names=F)
