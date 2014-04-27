#######################################################
# Peer Assessments /Getting and Cleaning Data Project #
#######################################################

install.packages("reshape")
install.packages("reshape2")
library("reshape")
library("reshape2")

# Load Datasets
X_train <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/X_train.txt", quote="\"")
Y_train <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/Y_train.txt", quote="\"")
subject_train <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/train/subject_train.txt", quote="\"")

X_test <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/X_test.txt", quote="\"")
Y_test <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/Y_test.txt", quote="\"")
subject_test <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/test/subject_test.txt", quote="\"")

# Merges the training and the test sets to create one data set
X_train_test <- rbind(X_train,X_test)
Y_train_test <- rbind(Y_train,Y_test)
subject_train_test <- rbind(subject_train,subject_test)

# Add Activity and Subject columns to the X-Activity Columns

# Load Activity Labels
features <- read.table("~/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset/features.txt", quote="\"")

# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Identify and subset Mean and StdDev columns
mean <- which(grepl("-mean",features[,2]))
std <- which(grepl("-std",features[,2]))
mean_std <- sort(c(mean,std))

X_mean_std <- subset(X_train_test, select=mean_std)

# Subset the Column Names and rename the Mean and Std Columns
feature_names <- features[mean_std,]
names(X_mean_std) <- feature_name[,2]

# Add Activity and Subject columns to the X-Activity Columns
XYsubject <-cbind(X_mean_std,Y_train_test,subject_train_test)
names(XYsubject)[80:81] <- c("Activity","Subject")

# Uses descriptive activity names to name the activities in the data set
XYsubject[,80][which(XYsubject[,80]==1)] <- "WALKING"
XYsubject[,80][which(XYsubject[,80]==2)] <- "WALKING_UPSTAIRS"
XYsubject[,80][which(XYsubject[,80]==3)] <- "WALKING_DOWNSTAIRS"
XYsubject[,80][which(XYsubject[,80]==4)] <- "SITTING"
XYsubject[,80][which(XYsubject[,80]==5)] <- "STANDING"
XYsubject[,80][which(XYsubject[,80]==6)] <- "LAYING"

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
melt <- melt(XYsubject, id.vars=80:81)
mean_value <- ddply(melt, .(Subject, Activity, variable), summarize, mean=mean(value))
tidy <- dcast(mean_value, Activity+Subject ~ variable, value.var="mean")

# Write Tidy Dataset as .TXT file
write.table(tidy, file = "tidy.txt", sep = ",", col.names = colnames(tidy))
