#-------------------  Exercise 1  -------------------------

#install.packages('cluster')
#install.packages('cclust')
#install.packages('HSAUR')
#install.packages('fpc')
#install.packages('factoextra')
library('cluster')
library('cclust')
library('fpc')
k_means_by_euclideian <- function(number_of_clusters, dataset){
  kmeans <- kmeans(dataset[, 1:2], number_of_clusters, nstart = 20)
  print(kmeans)
  return (kmeans)
}

k_means_by_manhattan <- function(number_of_clusters, dataset){
  matrix <- data.matrix(dataset[,1:2])
  cluster <- cclust(matrix, number_of_clusters, dist="manhattan",method= "kmeans")
  print(cluster)
  return (cluster)
}

# Question 1
x <-runif(250, 0, 10)
y <-runif(250, 0, 10)
data <- data.frame(x,y,1)
head(data)
par(mfrow=c(5,2))
clusters_by_euclidian <- k_means_by_euclideian(10,data)
clusters_by_manchattan <-  k_means_by_manhattan(10,data)

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

plotcluster(data, clusters_by_euclidian$cluster)
plotcluster(data, clusters_by_manchattan$cluster)


# Question 2
number_of_samples <- 250
number_of_cicles <- 5 
percircle <- as.integer((number_of_samples/number_of_cicles))
percircle
final_data_frame <- data.frame(x = numeric(), y = numeric(), circle = numeric())

# Create the data 
counter <- 1
while (counter <= number_of_cicles){
  theta = runif(percircle) * (2 * pi)
  r <- runif(percircle)
  x <- r * cos(theta)
  y <- r * sin(theta)
  x <- (x+5)
  y <- (y+5)
  final_data_frame <- rbind(final_data_frame, data.frame(x = x, y = y, circle = counter))
  counter <- counter + 1
}

plot(final_data_frame[,1],final_data_frame[,2])
clusters_by_euclidian_with_three <- k_means_by_euclideian(3,final_data_frame)
clusters_by_manhattan_with_three <-  k_means_by_manhattan(3,final_data_frame)
plotcluster(final_data_frame, clusters_by_euclidian_with_three$cluster)
plotcluster(final_data_frame, clusters_by_manhattan_with_three$cluster)
clusters_by_euclidian_six <- k_means_by_euclideian(6,final_data_frame)
clusters_by_manchattan_six <-  k_means_by_manhattan(6,final_data_frame)
plotcluster(final_data_frame, clusters_by_euclidian_six$cluster)
plotcluster(final_data_frame, clusters_by_manchattan_six$cluster)
clusters_by_euclidian <- k_means_by_euclideian(12,final_data_frame)
clusters_by_manchattan <-  k_means_by_manhattan(12,final_data_frame)
plotcluster(final_data_frame, clusters_by_euclidian$cluster)
plotcluster(final_data_frame, clusters_by_manchattan$cluster)

clusters_by_euclidian_with_three

clusters_by_euclidian_six

#Question 3
#install.packages("dbscan")
library('dbscan')
library('cluster')
library('cclust')
#library('fpc')

#Implement dbscan
dbsc <- dbscan(final_data_frame, eps=1, minPts=3);dbsc
#Plot results
pairs(final_data_frame, col = dbsc$cluster + 1L)



#-------------------  Exercise 2  -------------------------

##Import data
lines <- readLines('E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/Coronary Heart Disease.txt')
CHD <- read.table(text=lines,skip=1,sep = ',', header = T)
names(CHD)[2] <- "Class"

#See the structure of the data
str(CHD)
#Check for inbalance of the data
library(MASS)

#Histogram
barplot(table(CHD$Class), main = "Distribution by class", xlab = "CHD", ylab="Frequency",col= c("paleturquoise3","honeydew"),ylim=c(0,60))

# Grouped Bar Plotcounts 
color.names = c("navyblue","royalblue4","steelblue3","skyblue3","slategray3","paleturquoise3","powderblue","honeydew")
barplot(table(CHD), main="Age distribution per class",
        xlab="Age", col=color.names,
        legend = rownames(table(CHD)),args.legend = list( x = 'top', ncol = 2),ylim=c(0,15), beside=TRUE)


#__________Question 2.1______________

##Implement linear regression
linModel <- lm(Class ~ Age, data = CHD)
linCoefs <- linModel$coefficients; linCoefs
summary(linModel)
R2 <- summary(linModel)$r.squared ;R2
pred<- predict(linModel)

##Plot
library(ggplot2)
ggplot(CHD, aes(x = Age, y = Class)) + 
  geom_point(aes(colour=as.factor(Class),shape= as.factor(Class)), size = 3) +
  geom_smooth(method=lm , color="red", se=FALSE) +
  xlab("Age") + 
  ylab("Cononary Heart Disease") + 
  ggtitle("Predict Coronary Heart Disease")

##Prediction for Age=41 -> 0.359851
predict_41 = predict(linModel,list(Age=c(41)))
predict_41

#__________Question 2.2_____________

#Implement Logistic Regression
probitMod <- glm(Class ~ Age, data=CHD, family=binomial(link="probit"))
#Predict the result in probabilities
predicted <- predict(probitMod, CHD, type="response")
#Coefficients of the model
coef <- probitMod$coefficients; coef
#Summary of the model
summary(probitMod)
#class prediction
fitted.results <- ifelse(predicted > 0.5,1,0)

#Pseudo R2
library(pscl)
pseudoR2 <- pR2(probitMod); pseudoR2
#Accuracy
accuracy = mean(fitted.results==CHD$Class); accuracy

##Plot
range(CHD$Age)

xweight <- seq(20, 70,0.01)
yweight <- predict(probitMod, list(Age=xweight),type="response")
plot(x=CHD$Age,y=as.numeric(CHD$Class), type="p", lwd=6, xlab="Ηλικία", ylab="Πιθανότητα να πάσχει από CHD")
lines(xweight,yweight,col=2,lwd=7)
title("Λογιστική Παλινδρόμηση")

##Prediction for Age=41 -> 0.3316598, Class 0
probit_41 <- predict(probitMod, list(Age=c(41)),type="response")
class_41<-round(probit_41, 0)
class_41


#__________Question 2.2_____________
perceptron <- function(x, y, eta, niter) {
  
  # initialize weight vector
  weight <- rep(1, dim(x)[2] + 1)
  errors <- rep(1, niter)
  
  
  # loop over number of epochs niter
  for (jj in 1:niter) {
    
    # loop through training data set
    for (ii in 1:length(y)) {
      
      # Predict binary label using Heaviside activation function
      
      z <- sum(weight[2:length(weight)] * as.numeric(x[ii, ])) + weight[1]
      
      
      if(z >=0) {
        ypred <- 1
      } else {
        ypred <- 0
      }
      
      # Change weight - the formula doesn't do anything 
      # if the predicted value is correct
      weightdiff <- eta*(y[ii]-ypred)*c(1, as.numeric(x[ii, ]))
      weight <- weight + weightdiff
      
      # Update error function
      if ((y[ii] - ypred) != 0.0) {
        errors[jj] <- errors[jj] + 1
      }
      
    }
  }
  
  # weight to decide between the two classes 
  print(weight)
  return(errors)
}


#Algorithm result
x <- data.frame(Age=CHD$Age) #Parameters of the model: -3.5  0.1
y <- CHD[ ,c(2)]


#Large learning rate - No convergence
err <- perceptron(x, y, 1, 100)
plot(1:100, err, type="l", lwd=2, col="red", xlab="For 100 Epoches", ylab="Error")
title("Errors vs epoch - learning rate eta = 0.1")

#Small learning rate - Asymptotic error
err1 <- perceptron(x, y, 0.00000001, 100)
plot(1:100, err1, type="l", lwd=2, col="red", xlab="For 100 Epoches", ylab="Error")
title("Errors vs epoch - learning rate eta = 1e-8")

cl = data.frame(y=y)

weight <-c(-35, 1) 
weight <- as.matrix(weight)
library(dplyr)
#Add bias
X <- mutate(x,bias=1)
#Move the bias column to col1
X <- as.matrix(X[, c(ncol(X), 1:(ncol(X)-1))])
sum <- X%*%weight

#Assign class
yhat <- sum
for (j in 1:length(sum)){
  if(sum[j,1]>0){
    yhat[j,1]=1
  }
  else{
    yhat[j,1]=0
  }
}

#Count correct classification examples
count=0
for (j in 1:length(yhat)){
  if(yhat[j,1]==cl$y[j])
    count= count+1
}
#Percentage of correct classification
pct = count/length(yhat)*100; pct


###_________IRIS DATASET________
##Import data
data(iris)
#Import the dataset

iris[,6]<-0
iris[iris[,5]=="setosa",6]<-1
names(iris)[6] <- "Setosa"
iris<-iris[,-5]

#Prepare input data
x<-iris[ ,c(1:4)]
y<-iris[,c(5)]


#Model Results
err <- perceptron(x, y, 0.01, 1000)
plot(1:1000, err, type="l", lwd=2, col="red", xlab="epoch #1000", ylab="errors")
title("Errors vs epoch - learning rate eta = 1")

#Large learning rate - No convergence
err <- perceptron(x, y, 1, 1000)
plot(1:1000, err, type="l", lwd=2, col="red", xlab="epoch #1000", ylab="errors")
title("Errors vs epoch - learning rate eta = 1")

#Small learning rate - Asymptotic error
err <- perceptron(x, y, 0.00001, 1000)
plot(1:1000, err, type="l", lwd=2, col="red", xlab="epoch #1000", ylab="errors")
title("Errors vs epoch - learning rate eta = 0.00001")

#Report Performance
weight <-c(0.800, -0.341, 0.484, -0.299, 0.541) 
weight <- as.matrix(weight)

#Add bias
X <- mutate(iris[,1:4],bias=1)
#Move the bias column to col1
X <- as.matrix(X[, c(ncol(X), 1:(ncol(X)-1))])
sum <- X%*%weight

#Assign class
yhat <- sum
for (j in 1:length(sum)){
  if(sum[j,1]>0){
    yhat[j,1]=1
  }
  else{
    yhat[j,1]=0
  }
}

#Count correct classification examples
count=0
for (j in 1:length(yhat)){
  if(yhat[j,1]==iris$Setosa[j])
    count= count+1
}
#Percentage of correct classification
pct = count/length(yhat)*100;pct


#Model Prediction
lines <- readLines('E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/iris1.txt')
newiris <- read.table(text=lines,skip=1,sep = ',', header = T)
names(newiris)[5] <- "Setosa"

weight <-c(0.800, -0.341, 0.484, -0.299, 0.541) 
weight <- as.matrix(weight)

#Add bias
X <- mutate(newiris[,1:4],bias=1)
#Move the bias column to col1
X <- as.matrix(X[, c(ncol(X), 1:(ncol(X)-1))])
sum <- X%*%weight

#Assign class
yhat <- sum
for (j in 1:length(sum)){
  if(sum[j,1]>0){
    yhat[j,1]=1
  }
  else{
    yhat[j,1]=0
  }
}

#Count correct classification examples
count=0
for (j in 1:length(yhat)){
  if(yhat[j,1]==newiris$Setosa[j])
    count= count+1
}
#Percentage of correct classification
pct = count/length(yhat)*100
pct


#--------------Exercise 3

#Import libraries
library(MASS)
library(rms)
library(pscl)

#Import dataset
data<-Pima.te
data

#We split the dataset into 80% for train and 20% for test. 
training = sample(nrow(data),265,replace = FALSE)
train = data[training, ]
test = data[-training, ]

#Fit a logistic regression model
model1 <- glm(type ~.,data = train,family = "binomial")
summary(model1)

#With all independent variables
model11 <- lrm(type ~.,data = train)
model11
pR2(model1)

#Without bp
model2 <- glm(type ~.-bp,data = train,family = "binomial")
summary(model2)
model22 <- lrm(type ~ npreg+glu+skin+bmi+ped+age,data = train)
model22
pR2(model2)

#Without bp and skin
model3 <- glm(type ~ npreg + glu + bmi + ped + age,data = train,family = "binomial")
summary(model3)
model33 <- lrm(type ~ npreg + glu + bmi + ped + age,data = train)
model33
pR2(model3)

#Without bp, skin and age
model4 <- glm(type ~ npreg + glu + bmi + ped ,data = train,family = "binomial")
summary(model4)
model44 <- lrm(type ~ npreg + glu + bmi + ped ,data = train)
model44
pR2(model4)

#The best model is model3
predictions <- predict(model3,data = train, type = "response")
predictions

#Classification matrix
(table(ActualValue = train$type, PredictedValue = predictions>0.3))
predictionsTest <- predict(model3, newdata = test, type = "response")
predictionsTest

#Classification matrix
(table(ActualValue = test$type, PredictedValue = predictionsTest>0.3))
table(predictionsTest > 0.5,test$type)
(35+15)/(35+10+7+15)

table(predictionsTest>0.3, test[,"type"])

accuracy <- table(predictionsTest>0.3, test[,"type"])
sum(diag(accuracy))/sum(accuracy)


#--------------Exercise 4
library(forecast)
library(ggplot2)

# Let's create a time series object - class ts
# Data starts in 2016 - 4 observations/year (quarterly)
mydata = c(1,3,6,4,2,2,7,5,2,4,8,5,1,3,8,6)
myts = ts(data = mydata, start = 2016, frequency = 4)

# Lets see how the data looks
autoplot(myts, ylab = "Beer Sales(millions)", xlab = "Years",main = "Beer Sales" )
ggseasonplot(myts,main = "Seasonal Plot: Beer Sales")

# Checking the class
class(myts)
# Checking the timestamp
time(myts)

# library(ggplot2 and forecast)
autoplot(decompose(myts, type = "multiplicative"))

# seasonal adjustment
decomposition = decompose(myts, "multiplicative")

class(decomposition)

# we are subtracting the seasonal element
de.seasonal = myts / decomposition$seasonal

# getting a plot
plot(de.seasonal)

#Seasonality indexes
seas_ind = decomposition$figure

plot(decomposition$seasonal)

#Trend line
fit <- tslm(de.seasonal~time(de.seasonal))
summary(fit)
coefs = fit$coefficients


#Cyclical Variation
resid = fit$residuals
yhat = fit$fitted.values

relat_cycl= resid*100/yhat
relat_cycl

plot(relat_cycl)

#---------------Exercise 5
#Useful libraries
library(lubridate)
library(zoo)
library(forecast)
library(astsa)
library(xts)

# Let's create a time series object - class ts
mydata = c(37.44,44.14,46.25,43.99,51.84,49.10,58.56,58.02,70.28)
income_ts = ts(data = mydata, start = 2011, frequency = 1)

# Checking the class
class(income_ts)
# Checking the timestamp
time(income_ts)

# Lets see how the data looks
autoplot(income_ts, ylab = "Annual Income (1000euro)", xlab = "Time",main = "Income" )


fit <- tslm(income_ts~trend)
coefs = fit.coefficients

#Plot
tsplot(income_ts, col = "mediumblue", lwd = 2, ylab="Εισπράξεις", main="Εισπράξεις από ετήσιες πωλήσεις")
lines(fit$fitted.values,col="red",lwd=3)


#Cyclical Variation
resid = fit$residuals
yhat = fit$fitted.values

#Plot cyclical variation
relat_cycl= resid*100/yhat
plot(relat_cycl)

#----------------Exercise 6
#Import Dataset
library(readr)
data <- read_csv("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/bankofcanada.csv", 
                 col_names = FALSE, col_types = cols(X1 = col_character()))
View(data)
str(data)

#Useful libraries
library(lubridate)
library(zoo)
library(forecast)
library(astsa)
library(xts)


data <- data[order(data$X1), ]

#Convert first column from characters to date format
mydate=ymd(data$X1)
range(mydate)
#Verify
class(mydate)

# Getting a zoo object
myzoo = zoo(data$X2,order.by = mydate)
#Convert it to ts
myts.NA <- as.ts(myzoo,frequency=365, start=c(2018,2),end=c(2019,365)); myts.NA
#Imputate missing values with the previous value
myts = na.locf(myts.NA);myts
class(myts)

tsplot(myzoo, ylab="Exchange Rate USD/CAD", col = "mediumblue", lwd = 2,main="Daily Exchange Rate USD/CAD")

#Trend with linear regression
linear_trend <- lm(myts~time(myts))
summary(linear_trend)

#Plot
tsplot(myzoo, col = "mediumblue", lwd = 2, ylab="Exchange Rate USD/CAD", main="Daily Exchange Rate USD/CAD")
abline(linear_trend,col="red",lwd=3)

#Quadratic trend with polynomial regression
Time <- time(myts)
Time2 <- Time*Time
poly <- lm(myts ~ Time + Time2)
summary(poly)           

#Plot
tsplot(myzoo, col = "mediumblue", lwd = 2, ylab="Exchange Rate USD/CAD", main="Daily Exchange Rate USD/CAD")
lines(smooth.spline(Time,predict(poly)),col="red",lwd=3)


#The strong positive relationship lags reflect the strong seasonality in the data 
gglagplot(myts)

#Autocorrelation measures the linear relationship between lagged values of a time series.

#https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-differencing-to-remove-a-trend-or-seasonal-effects.html

#First difference 
myts.D1 <-diff(myts, differences = 1)
## plot the differenced data
plot(myts.D1, ylab = expression(paste(nabla^1, "Exchange Rate")),col="steelblue")

#Second difference
myts.D2 <-diff(myts, differences = 2)
## plot the differenced data
plot(myts.D2, ylab = expression(paste(nabla^2, "Exchange Rate")),col="steelblue")


# Prediction for the quatradic trend
timevalues <- seq(18262,18283,1)
predictedcounts <- predict(poly,list(Time=timevalues, Time2=timevalues^2)); predictedcounts

tsplot(myzoo, col = "mediumblue", lwd = 2, ylab="Exchange Rate USD/CAD", main="Daily Exchange Rate USD/CAD")
lines(smooth.spline(Time,predict(poly)),col="red",lwd=3)
points(y=predictedcounts, x = timevalues)

#January Variable
data$Month <- month(as.POSIXlt(data$X1, format="%Y-%m-%d"))
data[,4]<-0
data[data[,3]==1,4]<-1
data <- data[,-3]
names(data)[3]<-paste("X3") 

data$X3 <- as.logical(data$X3)
data$X1=ymd(data$X1)

library(ggplot2)
ggplot(data, aes(y = X2, x = X1)) + geom_point () + aes(colour = X3)


x = ts(data$X2)
y = data$X3

lm.xreg<-tslm(x ~ trend+y)
summary(lm.xreg)

#Plot
tsplot(x, col = "mediumblue", lwd = 2, ylab="Exchange Rate USD/CAD", main="Daily Exchange Rate USD/CAD")
lines(lm.xreg$fitted.values,col="red",lwd=3)

lm.poly.xreg<-tslm(x ~ trend+I(trend^2)+y)
summary(lm.poly.xreg)

#Plot
tsplot(x, col = "mediumblue", lwd = 2, ylab="Exchange Rate USD/CAD", main="Daily Exchange Rate USD/CAD")
lines(lm.poly.xreg$fitted.values,col="red",lwd=3)

tsplot(x, col = "mediumblue", lwd = 2, ylab="Exchange Rate USD/CAD", main="Daily Exchange Rate USD/CAD")
lines(lm.xreg$fitted.values,col="orange",lwd=3)
lines(lm.poly.xreg$fitted.values,col="red",lwd=3)




#Scatterplot with lags
library(graphics)
par(mfrow = c(1,2))
lag.plot(myts, lags=2, main="Lag Plot")

#Autocorrelation lag1
acf(myts, lag.max=2, plot=FALSE)

#Autocorrelation function
AC <- function(y,k) {
  
  X <- ts.union(yt = y, yt2 = stats::lag(x = y, k = k))
  c0 <- var(y)
  m <- mean(y)
  n <- length(y)
  ct <- sum((X[, 1] - m) * (X[, 2] - m), na.rm = TRUE) / (n - 1)
  rt <- ct / c0
  
  return(rt)
}

AC(myts,1)


tsdisplay(myts) # autoregression?

# AR(1) model
ar.model = arima(myts, order = c(1,0,0)); ar.model
checkresiduals(ar.model)


# MA(1) model
ma.model = arima(myts, order = c(0,0,2)); ma.model
checkresiduals(ma.model)


#ARIMA (2,1,2)

arima.model = arima(myts, order = c(2,1,2)); arima.model

residuals(arima.model)

checkresiduals(arima.model)

accuracy(ar.model)
accuracy(ma.model)
accuracy(arima.model)


#----------------Exercise 7

library(ggplot2)
library(cowplot) # required to arrange multiple plots in a grid
theme_set(theme_bw(base_size=12)) # set default ggplot2 theme
library(dplyr)
library(grid) # required to draw arrows


data(iris)
head(iris)


p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()
p3 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + geom_point()
p4 <- ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, color=Species)) + geom_point()
plot_grid(p1, p2, p3, p4, labels = "AUTO")


par(mfrow = c(2, 2))
hist(iris$Sepal.Length, breaks = 20)
hist(iris$Sepal.Width, breaks = 20)
hist(iris$Petal.Length, breaks = 20)
hist(iris$Petal.Width, breaks = 20)

pca <- prcomp(iris[,-5], center = TRUE, scale = TRUE); ir.pca
summary(pca)

# add species information back into PCA data
pca_data <- data.frame(pca$x, Species=iris$Species)
head(pca_data)

ggplot(pca_data, aes(x=PC1, y=PC2, color=Species)) + geom_point()


pca$rotation

# capture the rotation matrix in a data frame
rotation_data <- data.frame(pca$rotation, variable=row.names(pca$rotation))
# define a pleasing arrow style
arrow_style <- arrow(length = unit(0.05, "inches"),
                     type = "closed")
# now plot, using geom_segment() for arrows and geom_text for labels
ggplot(rotation_data) + 
  geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow=arrow_style) + 
  geom_text(aes(x=PC1, y=PC2, label=variable), hjust=0, size=3, color='red') + 
  xlim(-1.,1.25) + 
  ylim(-1.,1.) +
  coord_fixed() # fix aspect ratio to 1:1

percent <- 100*pca$sdev^2/sum(pca$sdev^2)
percent

perc_data <- data.frame(percent=percent, PC=1:length(percent))
ggplot(perc_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80)

