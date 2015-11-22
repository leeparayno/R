# Getting and Cleaning Data - Course Project

## Overview

The UCI Human Activity Recognition project performed some tests involving Samsung
smartphones, gathering data provided by subject individuals performing a variety 
of activities--with the smartphones providing various sensor data that was available

The data set is available here:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Project Description

This project provides a set of functions for processing the raw data set provided by 
UCI.

## Functions
### mergeDataset - function
   Purpose:    
   
   This function will combine the data sets for the training and test data sets into one combined data frame, with clean feature column names. The data frame will also be subsetted to include the mean and standard deviation values in the original data set, and have all activities labeled descriptively

   Arguments:  
   
- trainDir - path to the directory where the training data set for the 
                           UCI Human Activity Recognition project
- testDir - path to the directory where the test data set is
- featuresFile - path to the features file detailing all the features
                               in the data file set
- activityLabelFile - path to the file that lists all the activities 
                                   performed by the subjects in the study

   Returns:    
   
   A data frame that has been properly formated with combined data from the train and test data sets, with cleaned feature names and properly assigned activity labels
   



### cleanFeatureLabels - function
   Purpose:    
   
   This function will clean some of the stray punctuation that could cause issues with other code. It also replaces some abbreviations to make the feature names more meaningful
   
   Arguments:  
   
- featuresFile - path to the file listing all the features in the
                               data set
   Returns:    
   
   vector of feature names with no punctuation and more meaniful names
   

               
### createAverageOfMeanSTD - function
   Purpose: 
   
   Process the mean of all the values after grouping by activity and subject

   Arguments:  
   
- data (required) - must be a data.frame with all the raw data records as processed by mergeDataset
- outputFile - optional path to write the summarized data table to a file

   Returns: 
   
   data.frame of summarized mean values
   

### Developer: Lee Parayno
