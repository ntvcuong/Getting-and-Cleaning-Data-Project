##1. Merge the training and the test sets to create one data set
#Get the direction of data sets
path <- file.path('./UCI HAR Dataset')

#Get activity data of training set and test set
activitytest <- read.table(file.path(path,'test','Y_test.txt'), header = FALSE)
activitytrain <- read.table(file.path(path,'train','Y_train.txt'), header = FALSE)

#Get subject data of training set and test set
subjecttrain <- read.table(file.path(path,'train','subject_train.txt'), header = FALSE)
subjecttest <- read.table(file.path(path,'test','subject_test.txt'), header = FALSE)

#Get features data of training set and test set
featuretest <- read.table(file.path(path,'test','X_test.txt'),header = FALSE)
featuretrain <- read.table(file.path(path,'train','X_train.txt'),header = FALSE)

#Merge test sets and training sets
subject <- rbind(subjecttrain,subjecttest)
activity <- rbind(activitytrain,activitytest)
feature <- rbind(featuretrain, featuretest)

#Set names for the merged tables
names(subject) <- 'subject'
names(activity) <- 'activity'
featurenames <- read.table(file.path(path,'features.txt'), header = FALSE)
names(feature) <- featurenames$V2

#Merge all data to get the final table
data <- cbind(subject,activity)
data <- cbind(feature,data)

##2. Extracts only the measurements on the mean and standard deviation for each measurement
grep('mean()|std()',names(data))
selMea <- grep('mean\\(\\)|std\\(\\)',names(data), value = TRUE)
selMea <- c(selMea,'subject','activity')
data <- subset(data, select =  selMea)

##3. Uses descriptive activity names to name the activities in the data set
actnames <- read.table(file.path(path,'activity_labels.txt'),header = FALSE)
data$activity <- factor(data$activity, labels = actnames$V2)

##4. Appropriately labels the data set with descriptive variable names.
names(data) <- gsub('^t','time',names(data))
names(data) <- gsub('^f','frequency',names(data))
names(data) <- gsub('Acc','Accelerometer',names(data))
names(data) <- gsub('mean\\(\\)','Mean',names(data))
names(data) <- gsub('std\\(\\)','StandardDeviation',names(data))
names(data) <- gsub('Gyro', 'Gyroscope', names(data))
names(data) <- gsub('Mag', 'Magnitude', names(data))
names(data) <- gsub('BodyBody', 'Body', names(data))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
gr_data <- group_by(data,subject,activity)
tidydata <- summarise_each(gr_data,funs(mean))
tidydata<-arrange(tidydata,subject,activity)
write.table(tidydata,file='tidydata.txt',row.names = FALSE)



