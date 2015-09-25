#Clear the workspace
rm(list = ls())

#Load required libraries
library(rpart) 
library(rpart.plot)
library(RColorBrewer) 
library(rattle)
library(caret)
library(randomForest)
setwd("/Users/AndyC/Dropbox/rdata/cousera/prac_mach_learning")
#Set seed
set.seed(998)
#Read in Datasets convert  #DIV/0! to NA values
train1<- read.delim(file="pml_training.csv", header=TRUE,sep=",", na.strings=c("NA","#DIV/0!",""))
test1 <- read.delim(file="pml_testing.csv",header=TRUE,sep=",", na.strings=c("NA","#DIV/0!",""))

#Initial data inspection
summary(train1)
str(train1)

#Remove first ID columns as not needed
train1<- train1[,-1]
test1<- test1[,-1]
#From inspection a number of the variables have a large amount of NA values
#remove variables wiht >60% NA values


sel<-numeric()#Create a vector to store the high proportion NA column indexes
for(i in 1:ncol(train2)) { #for every column in the training dataset  
  if( sum( is.na( train2[, i] ) ) /nrow(train2) >= 0.6 ) sel <- c(sel,i )#If for col i  NAs > 60% of total observations note the column index  
} 
train2<-train1[,-sel]#Remove the columns wiht high proportion of NA results
test2<-test1[,-sel]
#Look for variaubles wiht Near Zero Variance
dataNZV <- nearZeroVar(train2, saveMetrics=FALSE)
#Do we really want to remove the flagged NZV columns
table(train2[,dataNZV])
table(train2[,dataNZV])[2]/length(train2[,dataNZV])
#2% of variables are yes  rest are no
#we will leave it in as we are using Random Forest

#Can we redudce the data using pronciple components analysis?


#Partition in to training and test dataset
trN <- createDataPartition(y=train2$classe, p=0.6, list=FALSE) 
train3 <- train2[trN, ]; 
test3 <- train2[-trN, ] 
dim(myTraining)
dim(myTesting)

#Model tuning, optimizing the tuning parameters
train(x=train3[,-59],y=train3[,59],method="randomForest")

#Use RF to model predictions
m1 <- randomForest(classe ~. , data=train3)
preds <- predict(m1, test3, type = "class")
confusionMatrix(preds, test3$classe)

# Get data into same type for RF
for (i in 1:ncol(test2) ) { 
  
  if( names(train2[i])==names(test2[i]) ) { 
    class(test2[,i]) <- class(train2[,i]) 
    levels(test2[,i]) <- levels(train2[,i])
  }      
} #And to make sure Coertion really worked, simple smart ass technique: 
test2 <- rbind(train2[2, -58] , test2) #note row 2 does not mean anything, this will be removed right.. now:
test2<- test2[-1,]

#Create scored file to submit for evaluation
preds2 <- predict(m1, test2, type = "class")

for(i in 1:length(preds2) ){     
  filename = paste0("problem_id_",i,".txt")
  write.table(preds2[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)   
} 

