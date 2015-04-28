

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="WearableComputing.zip", method="curl")
unzip("WearableComputing.zip")
## download and unzip the file

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt") ## read in X_test.txt 
head(X_test)
dim(X_test) ## 561
names(X_test) ## 2947

y_test <- read.table("./UCI HAR Dataset/test/y_test.txt") ## read in y_test.txt
View(y_test)
nrow(y_test) ## 2947; 1 column

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")## read in subject_test.txt
## 2974 rows

feature.lables <- read.table("./UCI HAR Dataset/features.txt")##read in features.text
View(feature.lables)
features<- as.vector(feature.lables$V2)


X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")## read in X_train.txt
dim(X_train)
names(X_train)

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")## read in y_train.txt
dim(y_train)
names(y_train)

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")## read in subject_train.txt

## Part of part four
colnames(X_test) <- features ## add variable names from the features.txt file
colnames(X_train) <- features## add variable names from the features.txt file

mergeDat_test <- cbind(subject_test, y_test)## merge the subject and activity test columns
colnames(mergeDat_test) <- c("Subject", "Activity")## rename the columns (Part four)
mergeDat_test <- cbind(mergeDat_test, X_test)## add the X_test columns

## Do the same for the train data as above ^
mergeDat_train <- cbind(subject_train, y_train)
colnames(mergeDat_train) <- c("Subject", "Activity") ##(part four)
mergeDat_train <- cbind(mergeDat_train, X_train)


## Part One: merge training and test data sets
mergeDat_total <- rbind(mergeDat_test, mergeDat_train) 
## add the two sub-datasets for test and train together

mean <- grepl("mean()", colnames(mergeDat_total))
std <- grepl("std()", colnames(mergeDat_total))
##create two logical vectors that return true is the column name of mergeDat_total containts 
      ## the string "main()" and "std()" (one vector for each string).

mergeDat_sub <- mergeDat_total[, (mean|std)] 
## subset by colmns containing the phrases "means()" and "std()" 

## Part Two: Extract measurments on mean and std for each measurment
mergeDat_sub <- cbind(mergeDat_total[, 1:2], mergeDat_sub) ## readd subject and activity columns



activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
## read the activity labels from the text file
activity_labels

##IntCode         Associated value 
## 1          WALKING
## 2   WALKING_UPSTAIRS
## 3 WALKING_DOWNSTAIRS
## 4            SITTING
## 5           STANDING
## 6             LAYING

##Part three
## replace the activity int code with its associated value found in the activity_labels text file
mergeDat_sub$Activity<- replace(mergeDat_sub$Activity, mergeDat_sub$Activity==1, "Walking")
mergeDat_sub$Activity<- replace(mergeDat_sub$Activity, mergeDat_sub$Activity==2, "Walking_Upstairs")
mergeDat_sub$Activity<- replace(mergeDat_sub$Activity, mergeDat_sub$Activity==3, "Walking_Downstairs")
mergeDat_sub$Activity<- replace(mergeDat_sub$Activity, mergeDat_sub$Activity==4, "Sitting")
mergeDat_sub$Activity<- replace(mergeDat_sub$Activity, mergeDat_sub$Activity==5, "Standing")
mergeDat_sub$Activity<- replace(mergeDat_sub$Activity, mergeDat_sub$Activity==6, "Laying")
View(mergeDat_sub$Activity)
head(mergeDat_sub)

library(dplyr)
as.numeric(as.character(mergeDat_sub[, 3:81]))
mean_sub<- mergeDat_sub %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

write.table(mean_sub, file="TidyMean.txt", row.name=FALSE)
