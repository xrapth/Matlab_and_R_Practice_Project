#---------------Exersise 1 -----------------
## Import libraries
library(rpart)

##import dataset
data(kyphosis)
class(kyphosis)

#Descriptive statistics
summary(kyphosis)

freq<-table(painters$School)
freq

#Histograms
barplot(table(kyphosis$Number),main="Frequency",xlab="Number of vertebrae involved")
barplot(table(kyphosis$Age),main="Frequency",xlab="Age")
barplot(table(kyphosis$Start),main="Frequency",xlab="First vertebra operated on")

#Plot with outstanding outliers
plot(kyphosis$Number, kyphosis$Age, col=4, ylab="Patient's Age", xlab="Number of vertebrae involved")


#Boxplot
boxplot(kyphosis$Number, horizontal=T, xlab= "Number of vertebrae involved", ylab='Frequency' )
#Detect oytliers values
outlier_values <- boxplot.stats(kyphosis$Number)$out  # outlier values

#Returns the lines of the outliers
which(kyphosis$Number %in% outlier_values)

#Same thing with identify
ID <- c(1:dim(kyphosis)[1])
plot(ID,kyphosis$Number)

identify(ID,kyphosis$Number)

#[1] 43 53


#---------------Exersise 2 -----------------
## Import Dataset
library(readr)
capital <- read_delim("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/capital.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
View(capital)

#Import libraries
library(psych)

#Question 1
#Variable gender must be converted to categorical
capital$gender <- as.factor(capital$gender)

#Relative Frequency Diagrams
relat_freq<-prop.table(table(capital$gender))
barplot(relat_freq,main="Relative Frequency",xlab="Gender")

# 3D Exploded Pie Chart
library(plotrix)
lbls <- c("Male", "Female")
pie3D(relat_freq,labels=lbls,explode=0.1,radius = 1.5, main="Pie Chart of Gender ")

hist(capital$balance,main="Histogram of Balance",xlab="Balance Variable", col="paleturquoise3",breaks=10, freq=TRUE)


# Question 2
boxplot(capital$balance, xlab= "Balance", ylab='Values of Balance' )

library(reshape2)
df.m <- melt(capital, id.var = "gender")
library(ggplot2)
ggplot(data = df.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=gender))


#Question 3
#Descriptive statistics
summary(capital)

desc.mat <- describeBy(capital,capital$gender)
desc.mat

#Question 4
##First way
qqnorm(capital$balance, pch = 1, frame = FALSE)
qqline(capital$balance, col = "steelblue", lwd = 2)

##Second way
library("car")
qqPlot(capital$balance)

#Dataset in normally distributed. Can assume normality. 

#________________-----Excersise 3-----_______________

#import dataset
data(mtcars)
View(mtcars)

names(mtcars)
class(mtcars$mpg)
class(mtcars$am)


mtcars$am <- as.factor(mtcars$am)
class(mtcars$am)

#Explore the relationship between fuel consumption and transmission
boxplot(mtcars$mpg ~ mtcars$am,xlab= "Transmission", ylab='Fuel consumption')

#H0: mean mpg of automatic cars = mean mpg of non-automatic cars
#two-sided test
#assume non-equal variances

t.test(mtcars$mpg ~ mtcars$am, mu=0, alt="two.sided", conf.level=0.95 ,var.eq=F, paired=F)
#Assume that me cannot reject the null hypothesis


#________________-----Excersise 4-----_______________
library(readr)
OctopusF <- read_csv("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/OctopusF.txt")
View(OctopusF)

#Import important libraries
library(dplyr)
library(psych)

#Descriptive statistics
summary(OctopusF)

describe(OctopusF$Weight)

# Histogram
hist(OctopusF$Weight,
     main="Histogram of Octopus weight",
     xlab = "Weight",
     xlim=c(40,2500),
     breaks = 30,
     col="steelblue",
     freq = TRUE)

#QQ-Plot
qqnorm(OctopusF$Weight, pch = 1, frame = FALSE)
qqline(OctopusF$Weight, col = "steelblue", lwd = 2)     #The data seem skewed. Not following the normal distribution

#Normality test
shapiro.test(OctopusF$Weight)
#Confidence level
t.test(OctopusF$Weight, mu=545, alt="two.sided", conf.level=0.95 ,var.eq=F, paired=F)

#Assume that me cannot reject the null hypothesis

#________________-----Excersise 5-----_______________
#http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

library(MASS)

data(survey)

View(survey)

#contingency table 
tab <- table(survey$Smoke,survey$Exer);tab

#Test the hypothesis whether the students smoking habit is independent of their exercise level at .05 significance level.

#We apply the chisq.test function to the contingency table tbl, and found the p-value to be 0.4828.

chisq.test(tab)

#As the p-value 0.4828 is greater than the .05 significance level, 
#we do not reject the null hypothesis that the smoking habit is 
#independent of the exercise level of the students.


#The warning message found in the solution above is due to the small cell values in the contingency table. 
#To avoid such warning, we combine the second and third columns of tbl, and save it in a new table named ctbl. 
#Then we apply the chisq.test function against ctbl instead.

ctbl = cbind(tab[,"Freq"], tab[,"None"] + tab[,"Some"]) 
ctbl

chisq.test(ctbl) 


#________________-----Excersise 6-----_______________

library(ggvis) #Data visulization
library(psych) #Scatterplot matrix
library(knitr) #html table
library(dplyr)
library(neuralnet) #artifical neural network 

library(readxl)
ConcreteData <- read_excel("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/Concrete_Data.xls", 
                            sheet = "Sheet1")
View(ConcreteData)
str(ConcreteData)


#Exploratory Analysis
summary(ConcreteData)
describe(ConcreteData)

ConcreteData %>% 
   ggvis(x = ~Concrete, fill:= "#27bc9c") %>% 
   layer_histograms() %>% 
   layer_paths(y = ~Concrete, 35.82, stroke := "red")

#From the histogram, it is clear that the distribution is slightly positively skewed. Despite that, there are still majority lot of 
#concretes with strength close to the mean of 35.82. Not too many concretes have strength too strong or weak.


#Correlation Matrix
cor(ConcreteData[c("Cement","Slag","Ash", "Water","Superplasticizer","CoarseAggregate", "FineAggregate", "Age", "Concrete" )])

#Scatter plots
pairs(ConcreteData[c("Cement","Slag","Ash", "Water","Superplasticizer","CoarseAggregate", "FineAggregate", "Age", "Concrete" )])
#Cpmple
pairs.panels(ConcreteData[c("Cement","Slag","Ash", "Water","Superplasticizer","CoarseAggregate", "FineAggregate", "Age", "Concrete" )])


#Normalization to [0,1]
normalize <- function(x){return ((x - min(x))/(max(x) - min(x) ))}
concrete_norm <- as.data.frame(lapply(ConcreteData, normalize))


#We will now use 25 percent of the data for testing and 75 percent for the training dataset. This will be divided into two separate datasets; 
#training and testing.
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

#Build a neural network with one hidden layer 
concrete_model <- neuralnet(Concrete ~ Cement+Slag+Ash+Water+Superplasticizer+CoarseAggregate+FineAggregate+Age , data = concrete_train, hidden = 1)

plot(concrete_model) #The error shown on the plot is the Sum of Squared Errors (SSE). The lower the SSE the better.

#MODEL EVALUATION
#building the predictor, exclude the target variable column
model_results <- compute(concrete_model, concrete_test[1:8])

#store the net.results column 
predicted_strength <- model_results$net.result

#As this a numeric prediction problem,correlation insead of a confusion matrix is used to provide insights of 
#the linear association between them both.

#Evaluation of the model
cor(predicted_strength, concrete_test$Concrete)


#MODEL IMPROVEMENT
#building the new model
concrete_model2 <- neuralnet(Concrete ~ Cement+Slag+Ash+Water+Superplasticizer+CoarseAggregate+FineAggregate+Age, data = concrete_train, hidden = 5 )
plot(concrete_model2)  #The SSE has reduced significantly from 5.67 to only 1.78 with increased in few thousands of steps.

#Implementing the Improved Neural Network
#Building the new predictor
model_results2 <- compute(concrete_model2, concrete_test[1:8])
#storing the results
predicted_strength2 <- model_results2$net.result

#Evaluation of the  new model
cor(predicted_strength2, concrete_test$strength)

#The compute() function works a little differently. It returns a list with two components $neurons, which stores the neurons for each layer 
#in the network and $net.result, that stores the predicted values

#http://rstudio-pubs-static.s3.amazonaws.com/472898_c8cc06c48f114976988b767b6318ade7.html

#________________-----Excersise 7-----_______________

library(readr)
faithfull <- read_table2("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/faithfull.txt")
View(faithfull)

library(dplyr)
describe(faithfull)


#Exploratory analysis
qqnorm(faithfull$eruptions, pch = 1, frame = FALSE)
qqline(faithfull$eruptions, col = "steelblue", lwd = 2)     #The data seem skewed. Not following the normal distribution

qqnorm(faithfull$waiting, pch = 1, frame = FALSE)
qqline(faithfull$waiting, col = "steelblue", lwd = 2)     #The data seem skewed. Not following the normal distribution

shapiro.test(faithfull$eruptions)
shapiro.test(faithfull$waiting)


#Correlation Test
library("ggpubr")
ggscatter(faithfull, x = "waiting", y = "eruptions", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Waiting time to the next erruption in mins", ylab = "Eruption time in mins")

#Our null hypothesis will be that the correlation coefficient IS NOT significantly different from 0. 95%
cor.test(faithfull$eruptions, faithfull$waiting, method = c("spearman"))


shapiro.test(faithfull$eruptions)

shapiro.test(faithfull$waiting)


#-----Linear model-----

model <- lm(eruptions ~ waiting, faithfull )
summary(model)

#As the p-value is much less than 0.05, we reject the null hypothesis that β = 0. 
#Hence there is a significant relationship between the variables in the linear regression model of the data set faithful.

f <- summary(model)$fstatistic
f

model_p <-pf(f[1],f[2],f[3],lower=FALSE)
model_p

#Residuals
res <- resid(model)

#Residuals Plot
plot(faithful$waiting, res, 
      ylab="Residuals", xlab="Waiting Time", 
      main="Faithful Eruptions") 
abline(0, 0) 



#Standarised Resisduals
stdres = rstandard(model)

plot(faithful$waiting, stdres, 
     ylab=" Standard Residuals", xlab="Waiting Time", 
     main="Faithful Eruptions") 
abline(0, 0) 


qqnorm(stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Faithful Eruptions") 
qqline(stdres)


shapiro.test(stdres)



#Prediction
coeffs = coefficients(model); coeffs 

waiting = 80           # the waiting time 

duration = coeffs[1] + coeffs[2]*waiting 
duration 

#Based on the simple linear regression model, 
#if the waiting time since the last eruption has been 80 minutes, we expect the next one to last 4.1762 minutes


#Confidence Interval for Linear Regression
#In the data set faithful, develop a 95% confidence interval of the mean eruption duration for the waiting time of 80 minutes.
newdata = data.frame(waiting=80)
predict(model, newdata, interval="confidence")
#The 95% confidence interval of the mean eruption duration for the waiting time of 80 minutes is between 4.1048 and 4.2476 minutes.

#Prediction Interval for Linear Regression
#In the data set faithful, develop a 95% prediction interval of the eruption duration for the waiting time of 80 minutes.
predict(model, newdata, interval="predict")
#The 95% prediction interval of the eruption duration for the waiting time of 80 minutes is between 3.1961 and 5.1564 minutes.


#________________-----Excersise 8-----_______________
library(dplyr)
library(psych)

data("stackloss")
head(stackloss)

#Exploratory Analysis
describe(stackloss)

#Exploratory analysis
qqnorm(stackloss$Air.Flow, pch = 1, frame = FALSE, main='Normal Q-Q Plot for Air.Flow')
qqline(stackloss$Air.Flow, col = "steelblue", lwd = 2) 

qqnorm(stackloss$Water.Temp, pch = 1, frame = FALSE,main='Normal Q-Q Plot for Water.Temp')
qqline(stackloss$Water.Temp, col = "steelblue", lwd = 2)

qqnorm(stackloss$Acid.Conc., pch = 1, frame = FALSE,main='Normal Q-Q Plot for Acid.Conc')
qqline(stackloss$Acid.Conc., col = "steelblue", lwd = 2)

qqnorm(stackloss$stack.loss, pch = 1, frame = FALSE, main='Normal Q-Q Plot for Stack.loss')
qqline(stackloss$stack.loss, col = "steelblue", lwd = 2)

shapiro.test(stackloss$Air.Flow)
shapiro.test(stackloss$Water.Temp)
shapiro.test(stackloss$Acid.Conc.)
shapiro.test(stackloss$stack.loss)


# Linear Model
stackloss.lm  = lm(stack.loss ~.,data=stackloss )
summary(stackloss.lm)$r.squared 
summary(stackloss.lm)

#As the p-value is much less than 0.05, we reject the null hypothesis that βi = 0. 
#Hence there is a significant relationship between the variables in the linear regression model of the data set stackloss.


#Prediction
coeffs = coefficients(stackloss.lm); coeffs 

newdata <- data.frame(Air.Flow=72,Water.Temp=20,Acid.Conc.=85)
predict(stackloss.lm, newdata)


#Confidence Interval for Linear Regression
#In the data set faithful, develop a 95% confidence interval of the mean eruption duration for the waiting time of 80 minutes.
predict(stackloss.lm, newdata, interval="confidence")
#The 95% confidence interval of the mean Stack loss is between 20.21846 and 28.945.

#Prediction Interval for Linear Regression
#In the data set faithful, develop a 95% prediction interval of the eruption duration for the waiting time of 80 minutes.
predict(stackloss.lm, newdata, interval="predict")
#The 95% prediction interval of the stack loss is between 16.4661 and 32.69736


#Residuals
res <- resid(stackloss.lm)
#Standarised Resisduals
stdres = rstandard(stackloss.lm)

#Standarized residuals distribution
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores",main="Stack loss",pch = 1, frame = FALSE) 
qqline(stdres,col = "steelblue", lwd = 2)

shapiro.test(stdres)

#________________-----Exersise 9-----_______________
library(readr)
Spectrum_Breizh <- read.table("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/Spectrum_Breizh.txt",  sep=";", header=TRUE, row.names=1)
View(Spectrum_Breizh)

#### Hist the organic Carbon
hist(Spectrum_Breizh[,"OC"], freq=F, xlab="Organic Carbon",breaks=20,col="paleturquoise3", main = "Organic Carbon Histogram",xlim=c(0,10))
lines(density(Spectrum_Breizh[,"OC"]),lwd = 1.8)


##Remove outlier
which(Spectrum_Breizh[,1]>8)
Spectrum_Breizh <- Spectrum_Breizh[-79,]
max(Spectrum_Breizh)

#### Hist the organic Carbon
hist(Spectrum_Breizh[,"OC"], freq=F, xlab="Organic Carbon",breaks=20,col="paleturquoise3", main = "Organic Carbon Histogram",xlim=c(0,6))
lines(density(Spectrum_Breizh[,"OC"]),lwd = 1.8)


library(lattice)
library(caret)

#Normalization
preproc1 <- preProcess(Spectrum_Breizh[,-1], method=c("range"))
norm1 <- predict(preproc1, Spectrum_Breizh[,-1])

#Final Dataset
fin.df <- cbind(norm1,Spectrum_Breizh$OC)
fin.df

#Traditional regression is not adequate because of the high dimensional component to the data set along with the multi-colinearity of the variables.  
#PLS is a powerful and effective method to handle these sorts of problematic data sets

###Install package
#install.packages("pls")
library(pls)

### Partial Least Squares (PLS) Regression
plsFit <- plsr(Spectrum_Breizh$OC ~., ncomp = 100, data = fin.df, validation = "LOO",scale=F)
summary(plsFit)

#Plot RMSEP for different number of components
validationplot(plsFit, val.type="RMSEP")

#Plot R2 for different number of components
validationplot(plsFit, val.type="R2")

pls.RMSEP = RMSEP(plsFit, estimate="CV")
plot(pls.RMSEP, main="RMSEP PLS", xlab="components")
min_comp = which.min(pls.RMSEP$val)
points(min_comp, min(pls.RMSEP$val), pch=1, col="red", cex=1.5)
min_comp #[1] 81
plot(plsFit, ncomp=81, asp=1, line=TRUE)

#Train the model with the correct number of parameters
plsFit <- plsr(Spectrum_Breizh$OC ~., ncomp = 81, data = fin.df, validation = "LOO",scale=F)
summary(plsFit)

#Plot the residuals
res.pls <- resid(plsFit)
plot(res.pls[,,min_comp],pch=15,cex=.5,ylab="Residuals",main="Scatterplot of residuals")
abline(h=c(-2,0,2), lty=c(2,1,2))

#Residuals distribution
qqnorm(res.pls[,,min_comp], ylab="Residuals", xlab="Normal Scores",main="Sepctrum_Breizh distribution of residuals",pch = 1, frame = FALSE) 
qqline(res.pls[,,min_comp],col = "steelblue", lwd = 2)


#________________-----Exercise 10-----_______________

library(readxl)
market <- read_excel("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/MarketData.xlsx")
View(market)

#load the libraries needed in the following codes
library(s20x)
library(car)


#list the name of each variable (data column) and the first six rows of the dataset
head(market)

# basic statistics of the variables
summary(market)

#set the 1 by 2 layout plot window
par(mfrow = c(1,2))
# boxplot to check if there are outliers
boxplot(market$Sales,horizontal = TRUE, xlab='Sales')

# histogram to explore the data distribution shape
hist(market$Sales,main='Distribution of variable Sales',xlab='Sales',prob=T)
lines(density(market$Sales),lty='dashed',lwd=2.5,col='red')

#Detect oytliers values
outlier_values <- boxplot.stats(market$Sales)$out; outlier_values  # outlier values

#Returns the lines of the outliers
which(market$Sales %in% outlier_values)
#Remove outliers
market <- market[-5,]

#Verify
par(mfrow = c(1,2))
# boxplot to check if there are outliers
boxplot(market$Sales,horizontal = TRUE, xlab='Sales')
# histogram to explore the data distribution shape
hist(market$Sales,main='Sales Distribution',xlab='Sales',prob=T)
lines(density(market$Sales),lty='dashed',lwd=2.5,col='red')

par(mfrow = c(1,2))
# add a normal distribution line in histogram
hist(market$Preis, freq=FALSE, col="gray", xlab="Price", main="Price Distribution",xlim=c(5,15))
curve(dnorm(x, mean=mean(market$Preis), sd=sd(market$Preis)), add=TRUE, col="red") #line

hist(market$Costs, freq=FALSE, col="gray", xlab="Cost", main="Cost Distribution")
curve(dnorm(x, mean=mean(market$Costs), sd=sd(market$Costs)), add=TRUE, col="red") #line

hist(market$Arrivals, freq=FALSE, col="gray", xlab="Arrivals", main="Arrivals Distribution")
curve(dnorm(x, mean=mean(market$Arrivals), sd=sd(market$Arrivals)), add=TRUE, col="red") #line

shapiro.test(market$Sales)
shapiro.test(market$Preis)
shapiro.test(market$Costs)
shapiro.test(market$Arrivals)

#Normalization to [0,1]
normalize <- function(x){return ((x - min(x))/(max(x) - min(x) ))}
concrete_norm <- as.data.frame(lapply(market[,-1], normalize))

#Correlation
par(mfrow = c(1,1))
#Correlation Matrix
cor(market[c("Sales","Preis","Costs", "Arrivals")])

#Scatter plots
pairs(market[c("Sales","Preis","Costs", "Arrivals")])

#Complex visualization
pairs.panels(market[c("Sales","Preis","Costs", "Arrivals")])

linear.lm = lm(Sales ~ ., data = market)
summary(linear.lm)


#Residuals
res = resid(linear.lm)
describe(res)
#Residuals Plot
plot(res, ylab="Residuals", xlab="Variables", main="Sales") 
abline(0, 0) 

#Standarised Resisduals
stdres = rstandard(linear.lm)
describe(stdres)
#Standard Residuals plot
plot(stdres, ylab="Residuals", xlab="Variables", main="Sales") 
abline(0, 0) 

#Standarized residuals distribution
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores",main="Sales",pch = 1, frame = FALSE) 
qqline(stdres,col = "steelblue", lwd = 2)

shapiro.test(stdres)

#Prediction
coeffs = coefficients(linear.lm); coeffs 

lmtest::bptest(linear.lm) 

#Both these test have a p-value less that a significance level of 0.05, therefore we can reject the null hypothesis that the variance of the residuals 
#is constant and infer that heteroscedasticity is indeed present, thereby confirming our graphical inference.

#https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/
#https://campusguides.lib.utah.edu/c.php?g=160853&p=1054158

#________________-----Exercise 11-----_______________
insurance = read.csv("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/insurance.csv", stringsAsFactors = T)
str(insurance)

#Descriptive Statistics
summary(insurance)
describe(insurance)

#Histogram of Charges
hist(insurance$charges,breaks = 30, freq=T,xlab="Insurance Charges",col="paleturquoise3", main = "Insurance Charges",xlim=c(0,70000),ylim=c(0,200))

#Correlation Matrix
cor(insurance[c("age", "bmi", "children", "charges")])

#he values are seperated into two sets off diagonal although they are identical. For example, the highest correlation pair is 
#the age & expenses with about 0.3 (positive correlation), which indicates that an older individual spend more on the medical.
#The relationship can be seen on the top right scatter plot

#Exploratory analysis
pairs(insurance[c("age", "bmi", "children", "charges")])

library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

ins_model <- lm(charges ~ ., data = insurance)
summary(ins_model)

#R-square = 0.75 indicates that about 75% of the variation in expenses is explained by the model.

#Standarised Resisduals
stdres = rstandard(ins_model)

#Standarized residuals distribution
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores",main="Insurance charges",pch = 1, frame = FALSE) 
qqline(stdres,col = "steelblue", lwd = 2)


#Improving model performance
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)


#Standarised Resisduals
stdres = rstandard(ins_model2)

#Standarized residuals distribution
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores",main="Insurance charges",pch = 1, frame = FALSE) 
qqline(stdres,col = "steelblue", lwd = 2)

shapiro.test(stdres)

#https://rpubs.com/cheverne/linearregressionanalysis_insurance


#________________-----Exercise 12-----_______________

library(readr)
mf1 <- read_delim("E:/BigData_Analytics/PredictiveAnalytics/MF_Ergasia/data/mf1.csv",";", escape_double = FALSE, trim_ws = TRUE)
View(mf1)

mf<- mf1[,1:4]
str(mf)

mf$Shift <- as.factor(mf$Shift)
mf$`Complaint Code` <- as.factor(mf$`Complaint Code`)
mf$`Manufacturing Plant` <- as.factor(mf$`Manufacturing Plant`)

#____-Question 1-_____
summary(mf)

#contingency table 
tab1 <- table(mf$`Shift`,mf$'Complaint Code'); tab1

barplot(tab1,beside=T,ylab="Count",xlab="Complain Code",col=c("black","red","yellow"), main ='Contigency of Complain Code and Shift')
legend("topright",inset=0.15,c("Day", "Swing", "Graveyard"),pch=15,col=c("black","red","yellow"))

chisq.test(tab1)
#As the p-value 0.3574 is greater than the .05 significance level, we do not reject the null hypothesis
#that the Shift is independent of the Complaint Code.

tab2 <- table(mf$`Shift`,mf$`Manufacturing Plant`); tab2

barplot(tab2,beside=T,ylab="Count",xlab="Manifacturing Plant",col=c("black","red","yellow"),main ='Contigency of Manifacturing Plant and Shift')
legend("topright",inset=0.15,c("Day", "Swing", "Graveyard"),pch=15,col=c("black","red","yellow"))

chisq.test(tab2)
#As the p-value 0.2912 is greater than the .05 significance level, we do not reject the null hypothesis
#that the Shift is independent of the Manufacturing Plant.

tab3 <- table(mf$`Complaint Code`,mf$`Manufacturing Plant`); tab3

barplot(tab3,beside=T,ylab="Count",xlab="Manufacturing Plant",col=c("black","red","yellow",'blue'),main ='Contigency of Manifacturing Plant and Complaint Code')
legend("topright",inset=0.15,c("Corrosion", "Cracked Lens", "Wiring",'Sound'),pch=15,col=c("black","red","yellow"))

chisq.test(tab3)
#As the p-value 0.2755 is greater than the .05 significance level, we do not reject the null hypothesis
#that the Complain Code is independent of the Manufacturing Plant.


#for the continouσ variable dollars
dollars =mf$`Dollar Claim Amount`
range(dollars)
breaks= seq(14500,39500,by=2000);breaks
dollars.cut = cut(dollars,breaks, right=F)

dollars.cut.freq = table(dollars.cut); cbind(dollars.cut.freq)

tab4 <- table(dollars.cut,mf$`Manufacturing Plant`); tab4
chisq.test(tab4)

tab5 <- table(dollars.cut,mf$`Shift`); tab5
chisq.test(tab5)

tab6 <- table(dollars.cut,mf$`Complaint Code`); tab6
chisq.test(tab6)                                     #Complaint code and claim amount are not independent. The null hypothesis is rejected.

#____-Question 2-____

tab3 <- table(mf$`Complaint Code`,mf$`Manufacturing Plant`); tab3

ctbl = cbind(tab3[,1], tab3[,2] + tab3[,3]) 
ctbl

chisq.test(ctbl)


#____-Question 3-____

ind1 <-which(mf$`Manufacturing Plant` %in% c(1)); ind1
ind2 <-which(mf$`Manufacturing Plant` %in% c(2)); ind2

maninf1<-mf[ind1,]
maninf2<-mf[ind2,]

t.test(maninf1$`Dollar Claim Amount`,maninf2$`Dollar Claim Amount`, conf.level = 0.98)


#________________-----Exercise 13-----_______________
x <- c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 8, 9, 9)
y <- c(5, 6, 2, 3, 5, 6, 7, 8, 3, 5, 7, 8, 4, 6, 8, 4, 5, 6, 7, 4, 3, 2, 5)

data <- data.frame(x=x, y=y)
data

#install.packages("dbscan")
library('dbscan')
library('cluster')
library('cclust')
#library('fpc')

#Implement dbscan
dbsc <- dbscan(data, eps=1, minPts=3);dbsc
#Plot results
pairs(data, col = dbsc$cluster + 1L)

#cluster new points
xnew <- c(3,4,2)
ynew <- c(7,5,9)
newdata <- data.frame(x=xnew, y=ynew)
predict(dbsc, newdata, data)          #[1] 2 2 0

#k-means - Elbow Method
#The elbow method looks at the percentage of variance explained as a function of the number of clusters: 
#One should choose a number of clusters so that adding another cluster doesn’t give much better modeling of the data. 
#More precisely, if one plots the percentage of variance explained by the clusters against the number of clusters, the first clusters will add much information (explain a lot of variance), but at some point the marginal gain will drop, giving an angle in the graph. The number of clusters is chosen at this point, hence the “elbow criterion”. This “elbow” cannot always be unambiguously identified.

#Elbow Method for finding the optimal number of clusters

## Compute within-cluster sum error for k = 2 to k = 15.
k.max = 20
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=1,iter.max = 50 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 15, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Change of slope is observed for k=4
set.seed(123)
km <- kmeans(data, 4)
plot(data, col=km$cluster,main='K-means for k=4')

#________________-----Exercise 14-----_______________

#Create dataset
x = c(0.4005, 0.2148 ,0.3457 , 0.2652 , 0.0789 , 0.4548) 
y = c(0.5306, 0.3854, 0.3156, 0.1875, 0.4139 , 0.3022)
data = data.frame(X=x,Y=y)
head(data)
plot(data,pch=15,main='Data Visualization')

#Focus on the agglomerative or bottom-up approach, where you start with each data point as its own cluster and 
#then combine clusters based on some similarity measure.The similarity between the clusters is often calculated 
#from the dissimilarity measures like the euclidean distance between two clusters. So the larger the distance between two clusters, the better it is.

dist_mat <- dist(data, method = 'euclidean'); dist_mat

#Single link - MIN (single linkage) Dij - η ελάχιστη από τις αποστάσεις μεταξύ κάθε στοιχείου της ομάδας i και j
hclust_single <- hclust(dist_mat, method = 'single')
#The distance between clusters is shown on the y axis
plot(hclust_single,ylab='Distance between clusters',main="Single linkage Agglomerative Algorithm")

#Complete link - MAX (complete linkage) Dij - η μέγιστη από τις αποστάσεις μεταξύ κάθε στοιχείου της ομάδας i και j
hclust_complete <- hclust(dist_mat, method = 'complete')
#The distance between clusters is shown on the y axis
plot(hclust_complete,ylab='Distance between clusters',main="Complete linkage Agglomerative Algorithm")
