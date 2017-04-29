library(scatterplot3d)
library(ggplot2)
library(tidyverse)


PortugeseStudents <- MathStudents <- read.csv("C:/Users/srush/Desktop/Student Alcohol Consumption/student/Portugese.csv",sep=";",header=T,stringsAsFactors = T)
class(PortugeseStudents)
summary(PortugeseStudents)
names(PortugeseStudents)

MathStudents <- read.csv("C:/Users/srush/Desktop/Student Alcohol Consumption/student/Math.csv",sep=";",header=T,stringsAsFactors = F)
class(MathStudents)
summary(MathStudents)
names(MathStudents)


##Adding a field called total alcohol consumption based on the weekend and daily alcohol consumption rates

PortugeseStudents$Talc <- round(((PortugeseStudents$Walc * 2 + PortugeseStudents$Dalc * 5) / 7),digits=0)

print("Total alcohol consumption of the portugese students based on their weekend and weekdays alcohol consumption rate")
print(PortugeseStudents$Talc)

MathStudents$Talc <- round(((MathStudents$Walc * 2 + MathStudents$Dalc * 7) / 2),digits=0)

print("Total alcohol consumption of the mathematics students based on their weekend and weekdays alcohol consumption rate")
print(MathStudents$Talc)




## Find the sutdents with more number of failures and very high alcohol consumption

high_alcohol_high_failures <-  filter(PortugeseStudents,failures>3,Talc==5)

print("Students with very high alcohol consumption and more number of failures")
print(high_alcohol_high_failures)


study_time_failures <- filter(PortugeseStudents,studytime<=2,failures>3)
print("Students with less study time and more failures")
print(study_time_failures)

studytime_alcohol_consumption <- filter(PortugeseStudents,studytime<=2,freetime>=3,Talc>=4)
print("Students with less study time, more free time and high alcohol consumption")
print(studytime_alcohol_consumption)


filter(PortugeseStudents,famsize == "GT3", Pstatus == "A",Medu <= 1,Fedu <=2,guardian=="other",studytime <=2,failures >3,higher == "no",absences >=50,Talc>=4)

lessgrade <- filter(PortugeseStudents,Talc>=4,G3<=5)
print("Students with less grade and high alcohol consumption")
print(lessgrade)

## Arrange the rows by the order of Alcohol consumption(High to low),failures,grades
Ordered_by_Talc <- arrange(PortugeseStudents,desc(Talc),failures,G3,G1,G2)

## Select the Total alcohol consumption,Famuly relationships,Family size,Student's study time, failures and grades
select(Ordered_by_Talc,Talc,famsize,famrel,studytime,failures,absences)

## The average alcohol consumption among all the students would be
print("The average alcohol consumption among all the students")
print(mean(PortugeseStudents$Talc))

print("Average number of failures")
print(mean(PortugeseStudents$failures))

print("Selecting the students based on the alcohol consumption, family relationships,grades and failures")
select(PortugeseStudents,desc(Talc),famrel,G3,failures)

print("Summary of students")
summary(PortugeseStudents)

print("Covariance between the alcohol consumption and the failure of students")
cov(PortugeseStudents$Talc,PortugeseStudents$failures)

print("Correlation between the alcohol consumption and the failure of students")
cor(PortugeseStudents$Talc,PortugeseStudents$failures)

print("Exploring the relation between the failure and alcohol consumption")
aggregate(failures ~ Talc,summary,data=PortugeseStudents)
boxplot(Talc ~ failures,summary,data=PortugeseStudents)

print("Box plot between the final grade and alcohol consumption")
boxplot(Talc ~ G3,summary,data=PortugeseStudents)

##Impact of the relations in family on the final grade

plot(jitter(PortugeseStudents$famrel),jitter(PortugeseStudents$G3))

## Scatterplot between the total alcohol consumption, failures, final grade.
scatterplot3d(PortugeseStudents$Talc,PortugeseStudents$failures,PortugeseStudents$G3,color="blue")

##Dividing the data set into training and test data sets

## Setting the seed to reproduce the same result
set.seed(1234)

## 70% of the data will be training data set and the 30% data will be test data set

divide <- sample(2,nrow(PortugeseStudents),replace=TRUE,prob=c(0.7,0.3))

traindata <- PortugeseStudents[divide==1,]
testdata <- PortugeseStudents[divide==2,]

##Linear regression using R

fit <- lm(Talc ~ failures + G3 + famrel + famsize + studytime + absences,data=traindata)

##Creating the formula to predict the total alcohol consumption rate using student failures, absences, final grade, study time, family size and family relations

fit

print("Coefficients of our linear model")
fit$coefficients

print("Linear model summary")
summary(fit)

p <- predict(fit,data=testdata)

## P is the predicted total alcohol consumption

p <- round(p,digits=0)
plot(p)

## Generalized linear regression

myformula <- Talc ~ failures + G3 + famsize + studytime + absences + higher + goout


stu_alc <- glm(myformula,family=gaussian("log"),data=traindata)
pred <- predict(stu_alc,type="response")

plot(traindata$Talc,pred,xlab="Observed values",ylab="Predicted values")



## Creating a decision tree to predict the number of failures

library(rpart)
myform <- failures ~ Talc + studytime + goout + higher + famrel + famsize + absences + health
h_rpart <- rpart(myform,data=traindata,control=rpart.control(minsplit=10))
print(h_rpart$cptable)
print(h_rpart)
plot(h_rpart)
text(h_rpart,use.n=T)

## Select the tree with minimum prediction error

min_error_tree <- which.min(h_rpart$cptable[,"xerror"])
cp <- h_rpart$cptable[min_error_tree,"CP"]
failure_pred <- prune(h_rpart,cp=cp)
print(failure_pred)
plot(failure_pred)
text(failure_pred,use.n=T)

## Predict the values based on the formula created

failure_form1_pred <- predict(failure_pred,newdata = testdata)
xlim <- range(PortugeseStudents$failures)
plot(failure_form1_pred ~ failures , data=testdata, xlab="Observed" , ylab= "Predicted", ylim=xlim,xlim=xlim)
abline(a=0,b=1)

form2 <- failures ~ Talc + studytime + goout + famrel + famsize + absences + health
tree_pred_failure <- rpart(form2,data=traindata,control = rpart.control(minsplit=10))
print(tree_pred_failure$cptable)
print(tree_pred_failure)
plot(tree_pred_failure)
text(tree_pred_failure,use.n=T)

min_error_form2 <- which.min(tree_pred_failure$cptable[,"xerror"])
cp <- tree_pred_failure$cptable[min_error_form2,"CP"]
form2_pred <- prune(tree_pred_failure,cp=cp)
print(form2_pred)
plot(form2_pred)
text(form2_pred,use.n=T)

failure_form2_pred <- predict(form2_pred,newdata = testdata)
xlim <- range(PortugeseStudents$failures)

plot(failure_form2_pred ~ failures , data = testdata , xlab="Observed",ylab="Predicted",ylim=xlim,xlim=xlim)
abline(a=0,b=1)


## Random Forest to build a prediction model to predict Alcohol Consumption
library(randomForest)
rf <- randomForest(Talc ~ .,data=traindata,ntree=100,proximity=TRUE)
table(predict(rf),traindata$Talc)
print(rf)
plot(rf)

importance(rf)

print(importance(rf))
varImpPlot(rf,type=2)

## Random Forest to build a prediction model to predict failures

rf_fail <- randomForest(failures ~ .,data=traindata,ntree=100,proximity=TRUE)
table(predict(rf_fail),traindata$failures)
print(rf_fail)
plot(rf_fail)

importance(rf_fail)

print(importance(rf_fail))

varImpPlot(rf_fail,type=2)



