install.packages("data.table")
library(data.table)
install.packages("plyr")
library(plyr)
install.packages("reshape2")
library(reshape2)
install.packages("zoo")
library(zoo)

#1. Merges the training and the test sets to create one data set.
#1.0. load features.txt to X_train.txt
  #set the directory to train to load X_train.txt
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset/train")
x_train <- read.table("X_train.txt", header = F, sep="")
  #set the directory to load features.txt
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset")
features <- read.table("features.txt", sep="")

#1.1.put the columns names of features.txt to X_train.txt
names_cols_features <- as.character(features[,2])
colnames(x_train) <- names_cols_features  
names(x_train)

#############################################################################
  #set the directory to test to load X_train.txt
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset/test")
x_test <- read.table("X_test.txt", header = F, sep="")

#1.1.put the columns names of features.txt to X_train.txt
colnames(x_test) <- names_cols_features
names(x_test)

#############################################################################

#1.3.0. merge y_train.txt with activity_labels.txt
  #set the directory to load activity_labels.txt
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset")
activity_labels <- read.table("activity_labels.txt", sep="")
str(activity_labels) 

  #set the directory to load y_train.txt
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset/train")
y_train <- read.table("y_train.txt")

  #merge together and write names
activityM = merge(y_train, activity_labels, sort=F)
colnames(activityM) <- c("number","activity")

#1.3.1. merge y_test.txt with activity_labels.txt
  #set the directory to load y_test.txt
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset/test")
y_test <- read.table("y_test.txt")

#merge together and write names
activityM_test = merge(y_test, activity_labels, sort=F)
colnames(activityM_test) <- c("number","activity")

#############################################################################
#add activityM and activityM_test to x_train, x_test
#train
str(x_train)   #7352 obs. of  561 variables
str(activityM) #7352 obs. of  2 variables
x_train["activity"] <- as.character(activityM[,2])
str(x_train$activity)
#test
str(x_test)         #2947 obs. of  561 variables
str(activityM_test) #2947 obs. of  2 variables
x_test["activity"] <- as.character(activityM_test[,2])
str(x_test$activity)

#############################################################################
#add subject to x_train, x_test
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset/train")
subject_train <- read.table("subject_train.txt")
setwd("C:/Users/enrigle/Documents/Clases/Stat Learning/Getting and Cleaning Data/w3/UCI HAR Dataset/test")
subject_test <- read.table("subject_test.txt")
colnames(subject_train) <- "subject"
colnames(subject_test) <- "subject"
x_train["subject"] <- subject_train
x_test["subject"] <- subject_test

#############################################################################
#combine the train and test data by row
SamsungData <- rbind(x_train, x_test) #7352+2947
table(SamsungData$activity)

#2. Extracts only the measurements on the mean and standard deviation for 
#each measurement.
names(SamsungData)
names <- names(SamsungData) 
names <- gsub("-mean()", "-Mean", names)
names <- gsub("-std()", "-Std", names)
setnames(SamsungData, names)
names(SamsungData)
names

logicalMeanStand = (grepl("activity",names) | grepl("subject",names) | 
                   grepl("-Mean..",names) & !grepl("-MeanFreq..",names) |
                   grepl("-Std..",names))

SamsungData_MS = SamsungData[mean_standard==TRUE]

# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names.
names <- names(SamsungData_MS)
names

for (i in 1:length(names))
{
  names[i] = gsub("\\()","",names[i])
  names[i] = gsub("-Std$","StdDev",names[i])
  names[i] = gsub("-Std","StdDev",names[i])
  names[i] = gsub("AccMag","AccMagnitude",names[i])
  names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",names[i])
  names[i] = gsub("([Bb]odyacc)","BodyAccJerkMagnitude",names[i])
  names[i] = gsub("JerkMag","JerkMagnitude",names[i])
  names[i] = gsub("GyroMag","GyroMagnitude",names[i])
  names[i] = gsub("(tBody)","TimeBody",names[i])
  names[i] = gsub("(fBody)","FreqBody",names[i])
  names[i] = gsub("(tGravity)","TimeGravity",names[i])
  names[i] = gsub("(fGravity)","FreqGravity",names[i])
}
setnames(SamsungData_MS, names)
names(SamsungData_MS)


# 5. Creates a second, independent tidy data set with the average of each
  #variable for each activity and each subject. 
setwd('..')
is.list(SamsungData_MS)
attach(SamsungData_MS)
average <- aggregate(SamsungData_MS,by=list(subject, activity), FUN=mean, na.rm=T)

warnings() #all the warning are due to the activity column, because is character

  #change the name of Group.1 and Group.2 to activity and subject and exclude
  #the last 2 columns activity and subject
colnames(average)[1] <- "activity"
colnames(average)[2] <- "subject"
colnames(average)[69] <- "borrar1"
colnames(average)[70] <- "borrar2"

  #lower case to all column names
names <- names(average)
names <- tolower(names)
names <- names(average)
names <- gsub("(bodybody)","body",names)

colnames(average) <- names
names(average)

  #delete the last two columns
average$borrar1 <- NULL
average$borrar2 <- NULL


  #export the data.frame
write.csv(average, file = 'tidydata.csv',
          row.names = F, quote = F)
