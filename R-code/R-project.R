# Project
rm(list=ls())
#clear the termnal in R
cat("\014") 
#clear all the graphics plot in R studio
graphics.off()
### read data from csv file
x <-read.csv("C:\\data\\Folds5x2_pp.csv",header=TRUE)
str(x)
summary(x)
colnames(x)
# Features consist of hourly average ambient variables 
# - Temperature (T) in the range 1.81?C and 37.11?C,
# - Ambient Pressure (AP) in the range 992.89-1033.30 milibar,
# - Relative Humidity (RH) in the range 25.56% to 100.16%
#   - Exhaust Vacuum (V) in teh range 25.36-81.56 cm Hg
# - Net hourly electrical energy output (EP) 420.26-495.76 MW

#########
# bloxplot measures the outlier in the data
#performing boxplot before resampling the data
########



#########
# Total of 9568 data is in the dataset.
# To make easy for calculation 
#2000 samples were selected randomly from the dataset

library(dplyr)
data = sample_n(x,800)


#### saving data in CSV to work on int
setwd("C:\\data\\")
write.csv(data, file="project_trunc1.csv")

data<-read.csv("C:\\data\\project_trunc1.csv",header=TRUE)
### removing the number column from the data
data <- data[c("AT",'V','AP','RH','PE')]
str(data)
summary(data)
colnames(data)

### again taking the boxplot of the newly sampled data for the outliers
par(mfrow=c(1,5))
boxplot(data$AT)
boxplot(data$V)
boxplot(data$AP)
boxplot(data$RH)
boxplot(data$PE)
par(mfrow=c(1,1))
boxplot(data,main = "Boxplot of Sampled Data")
###figure shows presence of the outlier in two variabels
### PRESSURE and Relative Humidity shows there is presence of the outlier
### The circular points in the box plot is the evidence of the outlier.




# scatterplot 
library(car)
pairs(~PE+AT+V+AP+RH,
      main = "Scatterplots:  Prediction of full load electrical power output of a base load operated combined cycle power plant", 
      data=data, 
      # diagonal = "histogram"
      labels=c("Power Output","X1:Temperature (AT)","X2:Exhasut Vaccum(V)",
               "X3:Ambient Pressure(AP)","X4:Relative Humidity(RH)","X4:"))
#scatterplotMatrix(data,spread=FALSE, smoother.args = list(lty=2),diagonal = 'histogram')

### correlation between parameters
library(psych)
cor(data[c("PE", "RH", "AP","V", "AT")])
pairs.panels(data[c("PE", "RH", "AP","V", "AT")], main="Correlation among Variables")
###########################
####ANALYSIS:
## 1. Linear Negative relation between Temperature and Power Output
## 2. Linear Negative Relation between Exhasut Vaccum and Power Output
## 3. Positive Linear Relation between Ambient Pressure and Power Output
## 4. It seems a positive relation exits between Power Output and Relative Humidity
## 5. A positive linear relationship exist between Temperature and Humidity.
## 6. It seems negative linear relationship exists between Temperature and Ambient Pressure
## 7. It seems negative linear relationship exists between Temperature and Relative Humidity
## 8. No relationship between Exhasut Vaccum and Ambient Pressure
## 9. It seems no relationship exists between ambient pressure and Relative Humidity 
############################

#####checking Histogram of all the variable and comparing against its log transformation
par(mfrow=c(2,5))
hist(data$PE)
hist(data$AT)
hist(data$AP)
hist(data$RH)
hist(data$V)
hist(log(data$PE))
hist(log(data$AT))
hist(log(data$AP))
hist(log(data$RH))
hist(log(data$V))
par(mfrow =c(1,1))

### creating  model with all possibilities

data$AT_sq <- (data$AT)^2
data$V_sq <- (data$V)^2
data$AP_sq <- (data$AP)^2
data$RH_sq <- (data$RH)^2


##### Purpose modal 
### +AT*V + AT*AP + AT*RH + V*AP+ V*RH + AP*RH 

model <- lm(PE~AT+AT_sq+V+V_sq+AP+AP_sq+RH+RH_sq  , data=data)
summary(model)
## Shows that every parameters are significant.

##
library(olsrr)
best <- ols_best_subset(model)
best
step <- ols_stepwise(model)
step

### new model
model <- lm(PE~AT+V+AP+RH  , data=data)
summary(model)
### multicollinearity Check
vif(model)

anova(model)
## Sequential Anova Test.
## Approach Top to down
## seems every parameters are significant

###### Residual normality 
qqnorm(model$residuals)
qqline(model$residuals)
## Most of the data follows the normal linear trend.
## It seems residuals are normal. But, further inverstigation is required

##### Histogram of the residuals
hist(model$residuals)
## It seems the histogram is normal


##### Performing the shapiro test of normality
shapiro.test(model$residuals)
## It shows that the residual distribution isnot normal

#### Data investigation for outliers
par(mfrow = c(2,2))
plot(model)
par(mfrow =c(1,1))

#### removing the outliers in the data



## boxplot of the all the data 
## check the normality of the data
boxplot(data)

##############################################
# found that the residuals are not normal 
# taking log of the PE 
##############################################

#log transformed modal
data$logPE <- log(data$PE)
model <- lm(logPE~AT+V+AP+RH,data=data)
summary(model)
anova(model)

par(mfrow=c(2,2))
plot(model)

#residual normality 
qqnorm(model$residuals)
qqline(model$residuals)

# shapiro test of normality
shapiro.test(model$residuals)
hist(model$residuals)

####################################################################################
# Taking log transformation of response didn't provide normal residual distribution
####################################################################################

#log of all variable transformed modal

model <- lm(logPE~logAT+logV+logAP+logRH,data=data)
summary(model)
anova(model)

par(mfrow=c(2,2))
plot(model)

#residual normality 
qqnorm(model$residuals)
qqline(model$residuals)

# shapiro test of normality
shapiro.test(model$residuals)
hist(model$residuals)

####################################################################################
# Taking log transformation of all data  didn't provide normal residual distribution
####################################################################################
