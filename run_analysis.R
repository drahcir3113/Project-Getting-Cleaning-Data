# download and unzip file
getwd()

if(!file.exists("./dat")){dir.create("./dat")}
datUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(datUrl,destfile="./dat/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./dat/Dataset.zip",exdir="./dat")

#1. Merges the training and the test sets to create one data set.
# Reading trainings tables:
xtrain <- read.table("./dat/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./dat/UCI HAR Dataset/train/y_train.txt")
subjecttrain <- read.table("./dat/UCI HAR Dataset/train/subject_train.txt")

# Reading testing tables:
xtest <- read.table("./dat/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./dat/UCI HAR Dataset/test/y_test.txt")
subjecttest <- read.table("./dat/UCI HAR Dataset/test/subject_test.txt")

# Reading feature vector:
features <- read.table('./dat/UCI HAR Dataset/features.txt')

# Reading activity labels:
activityLabels = read.table('./dat/UCI HAR Dataset/activity_labels.txt')

#Assigning column names:
colnames(xtrain) <- features[,2] 
colnames(ytrain) <-"activityId"
colnames(subjecttrain) <- "subjectId"

colnames(xtest) <- features[,2] 
colnames(ytest) <- "activityId"
colnames(subjecttest) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

#Merging all data in one set:
mrg_train <- cbind(ytrain, subjecttrain, xtrain)
mrg_test <- cbind(ytest, subjecttest, xtest)
combinedata <- rbind(mrg_train, mrg_test)

#2. Extracting only the measurements on the mean and standard deviation for each measurement
colNames <- colnames(combinedata)   #read colum name
#Create vector for defining ID, mean and standard deviation
mean_and_std <- (grepl("activityId" , colNames) |   
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

setForMeanAndStd <- combinedata[ , mean_and_std == TRUE] #subset from combinedata

#3. Uses descriptive activity names to name the activities in the data set
datactivityNames <- merge(setForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)

#4 Appropriately labels the data set with descriptive variable names
names(datactivityNames)
names(datactivityNames)<-gsub("Acc", "Accelerometer", names(datactivityNames))
names(datactivityNames)<-gsub("Gyro", "Gyroscope", names(datactivityNames))
names(datactivityNames)<-gsub("BodyBody", "Body", names(datactivityNames))
names(datactivityNames)<-gsub("Mag", "Magnitude", names(datactivityNames))
names(datactivityNames)<-gsub("^t", "Time", names(datactivityNames))
names(datactivityNames)<-gsub("^f", "Frequency", names(datactivityNames))
names(datactivityNames)<-gsub("tBody", "TimeBody", names(datactivityNames))
names(datactivityNames)<-gsub("-mean()", "Mean", names(datactivityNames), ignore.case = TRUE)
names(datactivityNames)<-gsub("-std()", "STD", names(datactivityNames), ignore.case = TRUE)
names(datactivityNames)<-gsub("-freq()", "Frequency", names(datactivityNames), ignore.case = TRUE)
names(datactivityNames)<-gsub("angle", "Angle", names(datactivityNames))
names(datactivityNames)<-gsub("gravity", "Gravity", names(datactivityNames))


#5 From the data set in step 4, creates a second, 
    #independent tidy data set with the average of each variable for each activity and each subject.
#Making second tidy data set
TidySet <- aggregate(. ~subjectId + activityId, datactivityNames, mean)
TidySet <- TidySet[order(TidySet$subjectId, TidySet$activityId),]

#Writing second tidy data set in txt file
write.table(TidySet, "TidySet.txt", row.name=FALSE)






