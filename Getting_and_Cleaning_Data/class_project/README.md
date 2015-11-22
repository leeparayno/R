# Getting and Cleaning Data - Course Project

## Overview

The UCI Human Activity Recognition project performed some tests involving Samsung
smartphones, gathering data provided by subject individuals performing a variety 
of activities--with the smartphones providing various sensor data that was available

The data set is available here:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Project Description

This project provides a set of functions for processing the raw data set provided by 
UCI.  It will subset the raw data into the features that only contain the mean and standard deviation values for the sampled data.  It will also process the subject and activities to bind them to the raw data.  In addition, feature names will be assigned to the data set.  This will process both the training and test data sets and combine them into one set of data.

## Functions
### mergeDataset - function

**Purpose:**
   
   This function will combine the data sets for the training and test data sets into one combined data frame, with clean feature column names. The data frame will also be subsetted to include the mean and standard deviation values in the original data set, and have all activities labeled descriptively

**Arguments:**  
   
The mergeDataset will attempt to find the necessary data files in the current working
directory.  If the data is somewhere else, you can specify the locations as necessary   
   
- trainDir (optional) - path to the directory where the training data set for the UCI Human Activity Recognition project
- testDir (optional) - path to the directory where the test data set is
- featuresFile (optional) - path to the features file detailing all the features in the data file set
- activityLabelFile (optional) - path to the file that lists all the activities 
                                   performed by the subjects in the study

**Returns:**
   
   A data frame that has been properly formated with combined data from the train and test data sets, with cleaned feature names and properly assigned activity labels
   



### cleanFeatureLabels - function

**Purpose:**
   
This function will clean some of the stray punctuation that could cause issues with other code. It also replaces some abbreviations to make the feature names more meaningful
   
**Arguments:**
   
The cleanFeatureLabels will attempt to find the features file in the current working directory.  If the features file is somewhere else, you can specify the location as necessary      
   
- featuresFile (optional) - path to the file listing all the features in the
                               data set
**Returns:** 
   
vector of feature names with no punctuation and more meaniful names
   

               
### createAverageOfMeanSTD - function
   
**Purpose:** 
   
Process the mean of all the values after grouping by activity and subject and create an output file of the mean all the sampled mean and standard deviation sample data.

**Arguments:**
   
- data (required) - must be a data.frame with all the raw data records as processed by mergeDataset
- outputFile - (default - averageOfMeanSTD.txt) optional path to write the summarized data table to a file

**Returns:** 
   
Output file will be created from data.frame of summarized mean values

See the Codebook for detailed information regarding the returned:

[https://github.com/leeparayno/R/blob/master/Getting_and_Cleaning_Data/class_project/Codebook.Rmd]()

### Developer: Lee Parayno
