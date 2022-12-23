 url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
 data<- read.csv(url, header=FALSE)
 head(data)
 colnames(data) <- c("age", "sex", "cp", "trestbps", "chol", "fbs","restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "hd")
#Columns have been labeled with the original names that were found on the website 
str(data)

#Now we need to clean the data
data[data=="?"]<- NA
data[data$sex == 0,]$sex<-"F"
data[data$sex == 1,]$sex <-"M"
data$sex <- as.factor(data$sex)
 
data$cp<-as.factor(data$cp)
data$fbs<-as.factor(data$fbs)
data$restecg<-as.factor(data$restecg)
data$exang<-as.factor(data$exang)
data$slope<-as.factor(data$slope)
 
data$ca<-as.integer(data$ca)
data$ca<- as.factor(data$ca)
 
data$thal<- as.integer(data$thal)
data$thal<- as.factor(data$thal)
 
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no= "Unhealthy")
data$hd<- as.factor(data$hd)
str(data)
nrow(data[is.na(data$ca) | is.na(data$thal),])
#6 rows of data have NAs in them
data[is.na(data$ca) | is.na(data$thal),]
#Above are the NA values 
#We can remove these rows
nrow(data)
data<- data[!(is.na(data$ca) | is.na(data$thal)),]
nrow(data)
#These samples have been removed and 297 samples remain 
xtabs(~hd +sex, data=data)
xtabs(~hd + cp,data=data)
xtabs(~hd + fbs,data=data)
xtabs(~hd + restecg, data=data)
xtabs(~hd + exang, data=data)
xtabs(~hd + slope, data=data)
xtabs(~hd + ca, data=data)
xtabs(~hd + thal, data=data)
#Now lets use logistic regression to predict heart disease using only the gender of each patient 
logistic<- glm( hd~sex, data=data,family="binomial")
summary(logistic)
#This is an exapmle of logistic regression with one independent variable
#Now lets use multiple variables for prediction by using the formula hd ~ . which models heart disease using all of the remaining variables in our dataframe 'data'
logistic<- glm( hd~., data=data,family="binomial")
summary(logistic)
predicted.data<- data.frame(probability.of.hd=logistic$fitted.values,hd=data$hd)
predicted.data<- predicted.data[order(predicted.data$probability.of.hd,decreasing = FALSE),]
predicted.data$rank<-1:nrow(predicted.data)
library(ggplot2)
library(cowplot)

ggplot(data=predicted.data,aes(x=rank, y=probability.of.hd))+ geom_point(aes(color=hd), alpha=1, shape=4, stroke=2)+ xlab("Index") + ylab("Predicted probability of getting heart disease")
# Call ggplot2 and use geom_point() to draw the data
ggsave("heart_disease_probabilities1.pdf")