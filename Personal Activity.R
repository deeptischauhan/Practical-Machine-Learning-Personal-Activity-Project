personal_activity <- function(){
        library(knitr)
        library(caret)
        library(randomForest)
        
        # load the datasets
        completeTraining <- read.csv("pml-training.csv")
        TestingData  <- read.csv("pml-testing.csv")
        
        
        # remove variables with Nearly Zero Variance
        TrainingData <- completeTraining[,-c(nearZeroVar( completeTraining, saveMetrics=F, freqCut = 95/45))]
        
        
        # remove variables that are mostly NA
        AllNA    <- sapply(TrainingData, function(x) mean(is.na(x))) > 0.95
        TrainingData <- TrainingData[, AllNA==FALSE]
        
        # remove columns with metadata
        TrainingData <- TrainingData[, -(1:5)]
       
        
        # model: Random Forest
        set.seed(12345)
        modFitRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
        
        modFitRandForest <- train(classe ~ ., data=TrainingData, method="rf", trControl=modFitRF)
	print(modFitRandForest$finalModel)
	
	predictRandForest <- predict(modFitRandForest, TestingData)
	print(predictRandForest)
}