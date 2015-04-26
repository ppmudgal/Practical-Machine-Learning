
library(caret)
library(ggplot2)
library(randomForest)

#read the training data
data <- read.csv("./pml-training.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
# read the test data
test <- read.csv("./pml-testing.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))

#data cleansing process
data<-data[ , ! apply( data , 2 , function(x) any(is.na(x)) ) ]
data<-data[ , ! apply( data , 2 , function(x) any(is.null(x)) ) ]
data<-data[ , ! apply( data , 2 , function(x) any(is.nan(x)) ) ]

#remove irrelevant columns

data=data[!colnames(data)=="X"]
data=data[!colnames(data)=="cvtd_timestamp"]


#Convert the character to factor
data$new_window<-as.factor(data$new_window)
data$user_name<-as.factor(data$user_name)
data$classe<-as.factor(data$classe)

# Data preparation for testing
# Data Cleansing 
test<-test[ , ! apply( test , 2 , function(x) any(is.na(x)) ) ]
test<-test[ , ! apply( test , 2 , function(x) any(is.null(x)) ) ]
test<-test[ , ! apply( test , 2 , function(x) any(is.nan(x)) ) ]
str(test)
colnames(test)
dim(test)

test$new_window<-as.factor(test$new_window)
test$user_name<-as.factor(test$user_name)
levels(test$new_window)<-levels(data$new_window)
levels(test$user_name)<-levels(data$user_name)

# Divide the data in training and testing

inTrain<-createDataPartition(y=data$classe,p=0.75,list=FALSE)
training<-data[inTrain,]
testing<-data[-inTrain,]

# fit random Forest
rfFit<-randomForest(classe~.,data=training,importance =TRUE)
#get the summary of model
rfFit

# predict using testing tabke
rf.testing = predict (rfFit ,newdata =testing)
#get the confusion matrix
confusionMatrix(data =  rf.testing, factor(testing$classe))

# get the prediction
rf.test = data.frame(predict (rfFit ,newdata =test))
answers<-as.vector(t(rf.test))

#Generating Files
pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
            filename = paste0("problem_id_",i,".txt")
            write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
}

pml_write_files(answers)

