# train <- mergeDataset("~/git/R/Getting_and_Cleaning_Data/class_project/UCI HAR Dataset/train", 
# "~/git/R/Getting_and_Cleaning_Data/class_project/UCI HAR Dataset/test", 
# "~/git/R/Getting_and_Cleaning_Data/class_project/UCI HAR Dataset/features.txt", 
# "~/git/R/Getting_and_Cleaning_Data/class_project/UCI HAR Dataset/activity_labels.txt")
#
# This script requires the following R libraries:
#   * dplyr
#   * plyr
# These libraries need to be installed prior to use
#
library(dplyr) 
library(plyr)

# Data from the UCI Human Activity Recognition project
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# mergeDataset - function
#   Arguments:  trainDir - path to the directory where the training data set for the 
#                           UCI Human Activity Recognition project
#               testDir - path to the directory where the test data set is
#               featuresFile - path to the features file detailing all the features
#                               in the data file set
#               activityLabelFile - path to the file that lists all the activities 
#                                   performed by the subjects in the study
#
#   Returns:    A data frame that has been properly formated with combined data from
#               the train and test data sets, with cleaned feature names and properly
#               assigned activity labels
#   Purpose:    This function will combine the data sets for the training and test
#               data sets into one combined data frame, with clean feature column names.
#               The data frame will also be subsetted to include the mean and 
#               standard deviation values in the original data set, and have all activities
#               labeled descriptively
mergeDataset <- function(trainDir, testDir, featuresFile = NULL, activityLabelFile = NULL) {
    if (is.null(featuresFile)) {
        stop("featuresFile argument is required.  Should be the path to the file 
             detailing the features of the data set")
    }
    if (is.null(activityLabelFile)) {
        stop("activityLabelFile argument is required.  Should be the path to the file 
             detailing the activities performed by the subjects in the data set")
    }
    # Load the activity labels from files
    activityLabels <- read.table(activityLabelFile, header = FALSE, col.names = c("activity", "activity_name"))
    
    # Pass the features file to be cleaned and returned as a feature of all 
    # the features in the data set
    features <- cleanFeatureLabels(featuresFile)
    
    # Load the training data set's activity identifiers
    trainActivityFile <- paste(trainDir,"/y_train.txt", sep = "")
    trainActivityX <- read.table(trainActivityFile, header = FALSE, col.names = c("activity"))
    trainActivityX <- trainActivityX[,1]
    
    # Load the subject file for the training data set
    trainSubjectFile <- paste(trainDir,"/subject_train.txt", sep = "")
    trainSubjectX <- read.table(trainSubjectFile, header = FALSE)
    trainSubjectX <- trainSubjectX[,1]
    
    # Load the training data 
    trainDataX <- paste(trainDir,"/X_train.txt", sep = "")
    trainX <- read.table(trainDataX, header = FALSE, sep = "", skip = 0, col.names = features[,2])
    
    # Define all the features that we want to preserve
    trainX <- trainX[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)]
    
    # Combine subjects to the data, and merge the data with the activity labels 
    fullTrainX <- data.frame(subject = trainSubjectX, activity = trainActivityX, trainX)
    fullTrainX <- merge(fullTrainX, activityLabels, by = "activity")
    
    # Fix labeling and remove activity keys
    fullTrainX[,1] <- fullTrainX[,69]
    fullTrainX <- fullTrainX[,1:68]
    
    # Load the test data set's activity identifiers
    testActivityFile <- paste(testDir,"/y_test.txt", sep = "")
    testActivityX <- read.table(testActivityFile, header = FALSE, col.names = c("activity"))
    testActivityX <- testActivityX[,1]
    
    # Load the subject file for the test data set
    testSubjectFile <- paste(testDir,"/subject_test.txt", sep = "")
    testSubjectX <- read.table(testSubjectFile, header = FALSE)
    testSubjectX <- testSubjectX[,1]
    
    # Load the test data 
    testDataX <- paste(testDir,"/X_test.txt", sep = "")
    #testX <- read.fwf(testDataX, c(rep.int(16,561)), header = FALSE, sep = "\t", skip = 0, col.names = features[,2])
    testX <- read.table(testDataX, header = FALSE, sep = "", skip = 0, col.names = features[,2])
    
    # Define all the features that we want to preserve
    testX <- testX[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)]
    
    # Combine subjects to the data, and merge the data with the activity labels 
    fullTestX <- data.frame(subject = testSubjectX, activity = testActivityX, testX)
    fullTestX <- merge(fullTestX, activityLabels, by = "activity")    
    
    # Fix labeling and remove activity keys
    fullTestX[,1] <- fullTestX[,69]
    fullTestX <- fullTestX[,1:68]
    
    # Combine the processed train and test data sets    
    complete <- bind_rows(fullTrainX, fullTestX)

    complete
}

# cleanFeatureLabels - function
#   Arguments:  featuresFile - path to the file listing all the features in the
#                               data set
#   Returns:    vector of feature names with no punctuation and more meaniful names
#   Purpose:    This function will clean some of the stray punctuation that could cause
#               issues with other code. It also replaces some abbreviations to make
#               the feature names more meaningful
cleanFeatureLabels <- function(featuresFile = NULL) {
    if (is.null(featuresFile)) {
        stop("Features file is required")
    }
    
    # Load the features file
    features <- read.table(featuresFile, header = FALSE, col.names = c("feature_number", "feature"))
    
    features$feature <- as.character(features$feature)
    
    # Remove the parenthesis
    features$feature <- gsub("\\)","",features$feature);
    features$feature <- gsub("\\(","",features$feature);
    
    # Change f to Freq
    features$feature <- gsub("^f","Freq",features$feature);
    
    # Change t to Time
    features$feature <- gsub("^t","Time",features$feature);
    
    features
}

# createAverageOfMeanSTD - function
#   Arguments:  data (required) - must be a data.frame with all the raw data records as processed 
#                       by mergeDataset
#               outputFile - optional path to write the summarized data table to a file
#   Returns: data.frame of summarized mean values
#   Purpose: Process the mean of all the values after grouping by activity and subject
#
createAverageOfMeanSTD <- function(data = NULL, outputFile = "averageOfMeanSTD.txt") {
    if (is.null(data)) {
        stop("data argument is required.  Should be a data frame of means and standard 
             deviation")
    }
    #R1 <- do.call("rbind", as.list(
    #    by(data, data["subject"], transform, avg=mean(TimeBodyAcc.std.Y))
    #))
    cols <- colnames(data)
    #columns
    #c(mean(df$TimeBodyAcc.mean.X),mean(df$TimeBodyAcc.mean.Y))
    average <- ddply(data, c("activity","subject"), function(df) { 
        c(mean(df$TimeBodyAcc.mean.X),
          mean(df$TimeBodyAcc.mean.Y), 
          mean(df$TimeBodyAcc.mean.Z),
          mean(df$TimeBodyAcc.std.X),
          mean(df$TimeBodyAcc.std.Y),
          mean(df$TimeBodyAcc.std.Z),
          mean(df$TimeGravityAcc.mean.X), 
          mean(df$TimeGravityAcc.mean.Y),
          mean(df$TimeGravityAcc.mean.Z),
          mean(df$TimeGravityAcc.std.X),
          mean(df$TimeGravityAcc.std.Y),
          mean(df$TimeGravityAcc.std.Z), 
          mean(df$TimeBodyAccJerk.mean.X),
          mean(df$TimeBodyAccJerk.mean.Y),
          mean(df$TimeBodyAccJerk.mean.Z),
          mean(df$TimeBodyAccJerk.std.X),
          mean(df$TimeBodyAccJerk.std.Y), 
          mean(df$TimeBodyAccJerk.std.Z),
          mean(df$TimeBodyGyro.mean.X),
          mean(df$TimeBodyGyro.mean.Y),
          mean(df$TimeBodyGyro.mean.Z),
          mean(df$TimeBodyGyro.std.X), 
          mean(df$TimeBodyGyro.std.Y),
          mean(df$TimeBodyGyro.std.Z),
          mean(df$TimeBodyGyroJerk.mean.X),
          mean(df$TimeBodyGyroJerk.mean.Y),
          mean(df$TimeBodyGyroJerk.mean.Z), 
          mean(df$TimeBodyGyroJerk.std.X),
          mean(df$TimeBodyGyroJerk.std.Y),
          mean(df$TimeBodyGyroJerk.std.Z),
          mean(df$TimeBodyAccMag.mean),
          mean(df$TimeBodyAccMag.std), 
          mean(df$TimeGravityAccMag.mean),
          mean(df$TimeGravityAccMag.std),
          mean(df$TimeBodyAccJerkMag.mean),
          mean(df$TimeBodyAccJerkMag.std),
          mean(df$TimeBodyGyroMag.mean), 
          mean(df$TimeBodyGyroMag.std),
          mean(df$TimeBodyGyroJerkMag.mean),
          mean(df$TimeBodyGyroJerkMag.std),
          mean(df$FreqBodyAcc.mean.X),
          mean(df$FreqBodyAcc.mean.Y), 
          mean(df$FreqBodyAcc.mean.Z),
          mean(df$FreqBodyAcc.std.X),
          mean(df$FreqBodyAcc.std.Y),
          mean(df$FreqBodyAcc.std.Z),
          mean(df$FreqBodyAccJerk.mean.X), 
          mean(df$FreqBodyAccJerk.mean.Y),
          mean(df$FreqBodyAccJerk.mean.Z),
          mean(df$FreqBodyAccJerk.std.X),
          mean(df$FreqBodyAccJerk.std.Y),
          mean(df$FreqBodyAccJerk.std.Z), 
          mean(df$FreqBodyGyro.mean.X),
          mean(df$FreqBodyGyro.mean.Y),
          mean(df$FreqBodyGyro.mean.Z),
          mean(df$FreqBodyGyro.std.X),
          mean(df$FreqBodyGyro.std.Y),
          mean(df$FreqBodyGyro.std.Z),
          mean(df$FreqBodyAccMag.mean),
          mean(df$FreqBodyAccMag.std),
          mean(df$FreqBodyBodyAccJerkMag.mean),
          mean(df$FreqBodyBodyAccJerkMag.std),
          mean(df$FreqBodyBodyGyroMag.mean),
          mean(df$FreqBodyBodyGyroMag.std),
          mean(df$FreqBodyBodyGyroJerkMag.mean),
          mean(df$FreqBodyBodyGyroJerkMag.std)
          )
        } )
    colnames(average) <- cols
    average
    #write.table(average, file = outputFile)
}


