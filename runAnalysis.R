########################################################################################################## 
## Coursera Getting and Cleaning Data Course Project 
## runAnalysis.r File Description: 
## Project Data downloaded from the following website 
##https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement. 
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names. 
##5.From the data set in step 4, creates a second, independent tidy data set with the 
##average of each variable for each activity and each subject. 
#########################################################################################################

##Clean up workspace 
rm(list=ls())
 setwd("C:/Users/Clement/Josephine/DataScience/Cleaning Data")
#Read data files
# Read in the data from files 
features     = read.table('UCI HAR Dataset/features.txt',header=FALSE); #imports features.txt 
activityType = read.table('UCI HAR Dataset/activity_labels.txt',header=FALSE); #imports activity_labels.txt 
subjectTrain = read.table('UCI HAR Dataset/train/subject_train.txt',header=FALSE); #imports subject_train.txt 
xTrain       = read.table('UCI HAR Dataset/train/x_train.txt',header=FALSE); #imports x_train.txt 
yTrain       = read.table('UCI HAR Dataset/train/y_train.txt',header=FALSE); #imports y_train.txt 

names(features) 
head(features, 3)
dim(features) 

names(activityType)
head(activityType,3)
dim(activityType)

names(subjectTrain)
head(subjectTrain,3)
dim(subjectTrain)

names(xTrain)
head(xTrain,3)
dim(xTrain))

names(yTrain)
head(yTrain,3)
dim(yTrain)

colnames(activityType)  = c('activityId','activityType'); 
names(activityType) 

colnames(subjectTrain)  = "subjectId"; 
names(subjectTrain) 

colnames(xTrain)        = features[,2]; 
names(xTrain)
 
colnames(yTrain)        = "activityId";
names(yTrain)
###################################################################
## 1.Merges the training and the test sets to create one data set.
##################################################################
# Create the final training set by merging yTrain, subjectTrain, and xTrain 
trainingData = cbind(yTrain,subjectTrain,xTrain);
head(trainingData, n=1)
names(trainingData)


# Read in the test data 
subjectTest = read.table('UCI HAR Dataset/test/subject_test.txt',header=FALSE); #imports subject_test.txt 
xTest       = read.table('UCI HAR Dataset/test/x_test.txt',header=FALSE); #imports x_test.txt 
yTest       = read.table('UCI HAR Dataset/test/y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to the test data imported above 
colnames(subjectTest) = "subjectId"; 
names(subjectTest) 
colnames(xTest)= features[,2];  
colnames(yTest)= "activityId";
names(yTest) 

# Create the final test set by merging the xTest, yTest and subjectTest data 
testData = cbind(yTest,subjectTest,xTest);
head(testData,1)

# Combine training and test data to create a final data set 
finalData = rbind(trainingData,testData);
head(finalData,1)

##############################################################################################
## 2.##Extracts only the measurements on the mean and standard deviation for each measurement. 
##############################################################################################

finColNames  = colnames(finalData)
IsMeasures =
(grepl("activity..",colNames)| grepl("subject..",colNames)| grepl("-mean..",colNames)
& !grepl("-meanFreq..",colNames)& !grepl("mean..-",colNames) | grepl("-std..",colNames)
		    & !grepl("-std()..-",colNames))

##Get the final data 
finalData = finalData[IsMeasures==TRUE];
head(finalData,2)
names(finalData)


########################################################################################
## 3. ##Uses descriptive activity names to name the activities in the data set
########################################################################################
# Merge the finalData set with the acitivityType table to include descriptive activity names 
names(activityType)
head(activityType)
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
head(finalData)
names(finalData)

##################################################################################
##4. Appropriately labels the data set with descriptive variable names. 
###################################################################################
##update the finColNames to update the column names of final Data
finColNames  = colnames(finalData)
# Cleaning up the variable names 
for (i in 1:length(finColNames))  
 {  
   finColNames[i] = gsub("\\()","",finColNames[i]) 
   finColNames[i] = gsub("-std$","StdDev",finColNames[i]) 
   finColNames[i] = gsub("-mean","Mean",finColNames[i]) 
   finColNames[i] = gsub("^(t)","time",finColNames[i]) 
   finColNames[i] = gsub("^(f)","freq",finColNames[i]) 
   finColNames[i] = gsub("([Gg]ravity)","Gravity",finColNames[i]) 
   finColNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",finColNames[i]) 
   finColNames[i] = gsub("[Gg]yro","Gyro",finColNames[i]) 
   finColNames[i] = gsub("AccMag","AccMagnitude",finColNames[i]) 
   finColNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",finColNames[i]) 
   finColNames[i] = gsub("JerkMag","JerkMagnitude",finColNames[i]) 
   finColNames[i] = gsub("GyroMag","GyroMagnitude",finColNames[i]) 
 };  

## Assign the new column names to the finalData
colnames(finalData) = finColNames

##########################################################################################
## 5.From the data set in step 4, creates a second, independent tidy data set with the 
##average of each variable for each activity and each subject. 
###########################################################################################
##Find Mean of the data frame  finalData
finalMeanData <- finalData %>% 
group_by(activityId,subjectId,activityType ) %>% 
summarise_each(funs(mean))
View(finalMeanData)

# Export the tidyData set  
write.table(finalMeanData, 'UCI HAR Dataset/finalMeanData.txt',row.names=FALSE,sep='\t');

