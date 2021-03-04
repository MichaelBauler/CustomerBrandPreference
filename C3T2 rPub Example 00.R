#rpub example
#https://rpubs.com/elsanju/predict_brand

################
# Load packages
################
install.packages("caret")
install.packages("caret", dependencies = c("Depends", "Suggests"))

install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
install.packages("doMC") 
install.packages("binr") 
install.packages("C50")

library(ggplot2)
library(readr)
library(ggplot2)
library(binr)
library(C50)
library(caret)

##############
# Import data
###############
CompleteResponses_original <- read.csv("/Users/mikebauler/Desktop/Data.Analytics/C3/surveydata/CompleteResponses.csv")
CompleteResponses_beforePP <- read.csv("/Users/mikebauler/Desktop/Data.Analytics/C3/surveydata/CompleteResponses.csv")
summary(CompleteResponses_original)
names(CompleteResponses_original)
head(CompleteResponses_original)
tail(CompleteResponses_original)
# check for missing values 
anyNA(CompleteResponses_beforePP)
#No NAs

summary(CompleteResponses_original)

#######################
# Feature selection
#######################
#Converting features in categories
CompleteResponses_beforePP$brand <-as.factor(CompleteResponses_beforePP$brand)
CompleteResponses_beforePP$car <- as.factor(CompleteResponses_beforePP$car)
CompleteResponses_beforePP$elevel <- as.factor(CompleteResponses_beforePP$elevel)
CompleteResponses_beforePP$zipcode <- as.factor(CompleteResponses_beforePP$zipcode)

#Dividing the age (3) and the salary (5)
CompleteResponses_beforePP$age_bin <- cut(CompleteResponses_beforePP$age, 3)
CompleteResponses_beforePP$salary_bin <- cut(CompleteResponses_beforePP$salary, 5)

#The only relation that we found was between AGE and SALARY
ggplot(CompleteResponses_beforePP, aes(x=salary, y=age, col=brand )) + geom_point()

#We can see a correlation between salary and age. 
#Looking at the age, we can see a patten if we divide it in three groups (20 to 40, 40 to 60, and 60 to 80). 
#And regarding to the salary, if we divide it in five groups($20.000 to $46.000, $46.000 to $72.000, …)     

#Dividing the age (3) and the salary (5)
CompleteResponses_beforePP$age_bin <- cut(CompleteResponses_beforePP$age, 3)
CompleteResponses_beforePP$salary_bin <- cut(CompleteResponses_beforePP$salary, 5)

#Converting them as a categorical
CompleteResponses_beforePP$age_bin <- as.factor(CompleteResponses_beforePP$age_bin)
CompleteResponses_beforePP$salary_bin <- as.factor(CompleteResponses_beforePP$salary_bin)

#Now, its time to transform them as a factor
#Converting them as a categorical
CompleteResponses_beforePP$age_bin <- as.factor(CompleteResponses_beforePP$age_bin)
CompleteResponses_beforePP$salary_bin <- as.factor(CompleteResponses_beforePP$salary_bin)     


#Finally, I created a new data set to train our model     
#Creating a new data base with only the Brand, and the new categories (age_bin, and salary_bin)
features <- c("brand", "age_bin", "salary_bin")
CompleteResponses_afprepo <- CompleteResponses_beforePP [, features]

#This is what the new data set looks like:
head(CompleteResponses_afprepo)

####################
####################
#Train and Assess the models
####################
####################
set.seed(998)
#Define an 75%/25% train/test.
inTraining <- createDataPartition(CompleteResponses_afprepo$brand, p = .75, list = FALSE)
training <- CompleteResponses_afprepo[inTraining,]
testing <- CompleteResponses_afprepo[-inTraining,]

#Cross Validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

####################
####################
#Model1 = C5.0, 10 folds, automatic tun, tuneLength 2.
####################
####################
Model1 <- train(brand~., data = training, method = "C5.0", trControl=fitControl,
                tuneLength = 2)
Model1 #C5.0
#Accuracy 0.9112278 Kappa 0.8131118

####################
####################
#Model2 = Random Forest, 10 folds, automatic tun, tuneLength 2.
####################
####################
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

Model2 <- train(brand~., data = training, method = "rf", trControl=fitControl, 
                tuneGrid=rfGrid)
Model2 #rf
#Accuracy 0.9112340 Kappa 0.8131112

####################
####################
#Model3 = Extreme Gradient Boosting, 10 folds, automatic tun, tuneLength 2.
###################
####################
Model3 <- train(brand~., data = training, method = "xgbTree", trControl=fitControl,
                tuneLength = 2)
Model3
####################
####################
#Comparing the Models
####################
####################
Classifier <- c('C5.0','Random Forest','eXtreme Boosting Trees')
Accuracy <- c(0.9112291, 0.9112340, 0.9112373)
Kappa <- c(0.8130879, 0.8131112, 0.8130903)
metrics <- data.frame(Classifier, Accuracy, Kappa)
metrics
#extreme Boosting was the most accurate

####################
####################
#Using the model to make predictions
####################
####################
#Pre-Process

#Read the newdata----
surveyIncomplete <- read.csv("/Users/mikebauler/Desktop/Data.Analytics/C3/surveydata/SurveyIncomplete.csv")

#preprocess_newdata ----
surveyIncomplete$age_bin <- cut(surveyIncomplete$age, 3)
surveyIncomplete$salary_bin <- cut(surveyIncomplete$salary, 5)
surveyIncomplete$age_bin <- as.factor(surveyIncomplete$age_bin)
surveyIncomplete$salary_bin <- as.factor(surveyIncomplete$salary_bin)

#feature selection----
features <- c("brand", "age_bin", "salary_bin")
surveyIncomplete_afprepo <- surveyIncomplete [, features]
####################
####################
#APPLY MODEL 3
####################
####################
predictions <- predict(Model3, surveyIncomplete_afprepo)
summary(predictions)


#0    1 
#1962 3038 
####################
####################
#We had 5,000 surveys without the answer about the brand. Using our model, 
#we can predict that 1,967 persons would choose Acer,and 3,033 would choose Sony

####################
####################
#Checking the results
####################
####################

#create new table with surveyIncomplete and predictions----
predictions_table <- surveyIncomplete[1:6]
predictions_table$brand <- predictions
head(predictions_table)

#Checking the results

####################
####################
#Plot: Relation between Age, Salary, and Brand (Predictions)
####################
####################
ggplot(predictions_table, aes(x=age, y=salary, col = brand)) + geom_point() +
  labs(x="Age", y="Salary", title="Relation between Age, Salary, and Brand (Predictions)")

####################
####################
#Plot: Relation between Age, Salary, and Brand (CompleteResponses)
####################
####################
ggplot(CompleteResponses_beforePP, aes(x=age, y=salary, col = brand)) + geom_point() + 
  labs(x="Age", y="Salary", title="Relation between Age, Salary, and Brand (CompleteResponses)")

####################
####################
#As we can see, the graphics, using the real responses, and the predictions- 
#looks the practically the same. Thats mean our model works.
####################
####################

####################
####################
#Final Results
#Which brand do our customers prefer?
####################
####################

#According to the Complete Surveys: 
summary(CompleteResponses_beforePP$brand)
# 0    1 
#3744 6154   (out of 10,000) 
#37%  63%

#According to the Incomplete Surveys (predictions): 
summary(predictions)
#  0    1 
#1962 3038   (out of 5000)
#39%    61%

#Total: 
answers <- summary(CompleteResponses_beforePP$brand) + summary(predictions)
answers
#0    1 
#5706 9192 (out of 15,000)
#38%  #62%  

####################
####################
#To make a graphic that shows the difference and the final result, 
#we can create a new data set with all the surveys:
####################
####################

#Create a new data set with all the survey's answers: real responses and predictions 
table.total <- rbind(CompleteResponses_beforePP[1:7], predictions_table[, c(1:7)])

#Let´s make a graphic:
#change 0 to Acer and 1 to Sony, and add a column brand_name 
table.total$brand_name <- ifelse(table.total$brand == 1, "Sony", "Acer")

#using ggplot to draw barchart for incomplete data
ggplot(data = table.total, mapping = aes(x = brand_name)) +
  geom_bar(position = "dodge") +
  labs(x="Brand", y="Count", title="What brand do our customers prefer?") + 
  geom_text(stat='count', aes(label=..count..), vjust=5, col="white", size=4)

####################
####################
#Recommendations
#Our customers prefer Sony than Acer. 
#So, if the purchasing department is thinking about acquiring electronic products and has to choose between one brand, 
#it would prioritize those of Sony.

#On the other hand, I would suggest to use the Salary and Age plot to make clusters and focus marketing campaigns on those different clusters.For example, The customers that prefer Acer are
#Between 20 and 40 years, with a salary between 50,000 and 100,000.
#Between 40 and 60 years, with a salary between 80,000 and 120,000.
#Between 60 and 80 years, with a salary less than 80,000.
#The rest of the users prefer Sony.



####################
####################
####################
####################
####################
####################
C50imp <-varImp(C50) 
C50imp

RFimp <-varImp(rfFit1) 
RFimp

Classifier <- c('C5.0','Random Forest')
Accuracy <- c(0.9112278, 0.9112340)
Kappa <- c(0.8131118, 0.8131112)
metrics <- data.frame(Classifier, Accuracy, Kappa)
metrics