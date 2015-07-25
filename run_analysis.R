## This script is linked with Coursera course names : Getting and cleaning data
## It concerns the course project about werable computing 


run_analysis <- function(){

  ## Data for test purpose (to clean at the end)
  ## setwd("C:/Users/miche_000/Documents/R/Coursera-GCD")
  
  ## load useful library

  
  ## Reference
  dataDir <- "UCI HAR Dataset"
  
  ## Check working directory
  message("checking directory")
    WD <- getwd()
    if(!any(list.dirs(WD,full.names=FALSE)==dataDir)) return("No data in current directory. Please check your working directory")
    
    
  ## Load data
  message("reading data")
    
    ## global
    setwd(dataDir)
    features <- read.table("features.txt")  
    activityLabels <- read.table("activity_labels.txt")
  
    ## train
    setwd("train")
    trainingSubject <- read.table("subject_train.txt")  
    trainingSet <- read.table("X_train.txt")  
    trainingLabels <- read.table("y_train.txt")  
    
    ## test
    setwd(WD)
    setwd(dataDir)
    setwd("test")
    testSubject <- read.table("subject_test.txt")  
    testSet <- read.table("X_test.txt")  
    testLabels <- read.table("y_test.txt")  
    
    setwd(WD)
    
    
    ## Merging Data
    message("merging data")
      ## Prepare training Set
      colnames(trainingSet) <- features[,2]
      
      ## Prepare training Subject  
      colnames(trainingSubject) <- "subject"
    
      ## Prepare training Activty
      trainingLabels <- merge(trainingLabels,activityLabels)
      colnames(trainingLabels)[2] <- "activityCode"
      

      ## create training Data
      group <- factor(c(rep("training",nrow(trainingSet))))     
      trainingSet <- cbind(group,trainingLabels[2],trainingSubject,trainingSet)    
    

      ## Prepare test Set
      colnames(testSet) <- features[,2]

      ## Prepare test Subject      
      colnames(testSubject) <- "subject"

      ## Prepare test Activty
      testLabels <- merge(testLabels,activityLabels)
      colnames(testLabels)[2] <- "activityCode"    

      ## create test Data
      group <- factor(c(rep("test",nrow(testSet))))     
      testSet <- cbind(group,testLabels[2],testSubject,testSet)    
      
      ## Merge files

      dataset <- rbind(testSet, trainingSet)

    ## Creating files
    
      ## mean and standard deviation for each measurement
      message("Building tidy data set")
      plage <- c(1:3)
      plage <- c(plage,4:9)     ## tBodyAcc-XYZ
      plage <- c(plage,44:49)   ## tGravityAcc-XYZ
      plage <- c(plage,84:89)   ## tBodyAccJerk-XYZ
      plage <- c(plage,124:129) ## tBodyGyro-XYZ
      plage <- c(plage,164:169) ## tBodyGyroJerk-XYZ
      plage <- c(plage,204:205) ## tBodyAccMag
      plage <- c(plage,217:218) ## tGravityAccMag
      plage <- c(plage,230:231) ## tBodyAccJerkMag
      plage <- c(plage,243:244) ## tBodyGyroMag
      plage <- c(plage,256:257) ## tBodyGyroJerkMag
      plage <- c(plage,269:274) ## fBodyAcc-XYZ
      plage <- c(plage,348:353) ## fBodyAccJerk-XYZ
      plage <- c(plage,427:432) ## fBodyGyro-XYZ
      plage <- c(plage,506:507) ## fBodyAccMag
      plage <- c(plage,519:520) ## fBodyAccJerkMag
      plage <- c(plage,532:533) ## fBodyGyroMag
      plage <- c(plage,545:546) ## fBodyGyroJerkMag

      subset <- dataset[,plage]

      for (i in 1:length(activityLabels[,2])){
          for (j in 1:30) {
            if (i==1 & j==1){
              tidyset <- sapply(subset[(subset$activityCode==activityLabels[1,2] & subset$subject==1),4:69],mean)
            }else{
              tidyset <- rbind(tidyset, sapply(subset[(subset$activityCode==activityLabels[i,2] & subset$subject==j),4:69],mean))
            }      
          }
      }  
      
            
      tidyset <- cbind(as.character(rep(activityLabels[,2],rep(30,length(activityLabels[,1])))),rep(seq(1:30),length(activityLabels[,1])),tidyset)  
      colnames(tidyset)[1:2] <- c("activity","subject")
      rownames(tidyset)[1] <- ""
      tidyset<- tidyset[!is.nan(as.numeric(tidyset[,3])),]
      
      message("Saving tidy data set to 'measurements_mean.txt' and sending View() commande to R")
      write.table(tidyset, file="measurements_mean.txt", quote=FALSE)
      View(tidyset)
}