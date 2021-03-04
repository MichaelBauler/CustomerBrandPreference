###ALL THESE EXAMPLES ARE REGRESSION RELATED#####
##the ONLY diff. of Classification & Regression in R is status of dependent Variable

#Installing Caret 
install.packages("caret", dependencies = c("Depends", "Suggests"))

#====================================================================
#1st EXAMPLE - Automatic Tuning Grid);Randoom Forest
#http://topepo.github.io/caret/bytag.html
#model training: 
http://topepo.github.io/caret/training.html
#model measurement: 
http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad (Dependent Variable) very numeric

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data ####NOT A PART OF THE PIPELINE -Do not always need a sample
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

###IS A PART OF THE PIPELINE
# define an 75%/25% train/test split of the dataset 
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation "10" means 10 folds cv=cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLength = 1 (trains with 1 mtry value for RandomForest)
rfFit1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
# ~ tells that it is the dependent, "." tells to use everything else as our predictors "rf" method code 
#rf = random forest
#TuneLength, when TuneLength = 1 it means that only 1 attempt will be made at tuning this algorythm
#He would never have a TuneLength of 1 maybe start off with TuneLength 5
#if it's still increasing at 5 maybe go to 10 because haven't topped out yet

#training results
rfFit1

#====================================================================
#Second Example(The only thing that changes in this example is that it's TuneLength 2 
#and changed the rfFit2 cuz we don't like to overwrite models

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
rfFit2 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)

#training results
rfFit2
save.image()

######################################################################
######################################################################
######################################################################
######################################################################
#Pipeline Caret 

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
library(caret)
library(corrplot)
library(readr)
library(mlbench)

