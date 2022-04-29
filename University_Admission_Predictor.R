setwd(dirname(parent.frame(2)$ofile))   #Setting the working directory to current R script file path
library("ggplot2")
library("reshape2")
library("randomForest")
library("rpart")
library("Metrics")


train <- read.table("Admission_Predict_Ver1.1.csv", sep = ",", header = TRUE, nrows = 400)   #Read top 400

test <- read.table("Admission_Predict_Ver1.1.csv", sep = ",", header = TRUE, nrows=100, skip=400)   #Read last 100

#Adding column headers separately to test
colnames(test) <- c("Serial.No.", "GRE.Score", "TOEFL.Score", "University.Rating", "SOP", "LOR", "CGPA", "Research", "Chance.of.Admit")


train$Serial.No. <- NULL

test$Serial.No. <- NULL


##########################      HEATMAP    #################################

correl <- round(cor(train), 3)    #Create the correlation matrix

melted_correl <- melt(correl)   #this organizes the data

colnames(melted_correl)[which(names(melted_correl) == "value")] <- "Correlation"   #Changing column name from 'value' to 'Correlation'

set.seed(1)

#Printing heatmap
print(ggplot(data = melted_correl, aes(x = Var1, y = Var2, fill = Correlation)) + geom_tile() + 
        geom_text(aes(Var2, Var1, label = Correlation), color = "black", size = 4))

########################################################################################################################

train$Research<-as.factor(train$Research)
train$University.Rating<-as.factor(train$University.Rating)

test$Research<-as.factor(test$Research)
test$University.Rating<-as.factor(test$University.Rating)


copy <- test


#Linear regression model
modelL <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP + Research + CGPA + University.Rating, train)


prediction <- predict(modelL, copy)


#rmse and mae value calculation
mse <- sqrt(mean((prediction-test$Chance.of.Admit)^2))
meanab <- mae(test$Chance.of.Admit, prediction)
print("For Multiple Linear Regression:")
cat("\n")
print(paste("RMSE value is", mse))
print(paste("Mean Absolute Error is", meanab))

#Percentage Accuracy
accuracy <- 100 - (mean(abs((prediction-test$Chance.of.Admit)))*100)

print(paste("Accuracy is", accuracy, "%"))



#########  Used for printing the Actual and Predicted results Plot ####################
m <- test$Chance.of.Admit


l <- c(m, prediction)

c <- rep("Actual", 100)
d <- rep("Predicted", 100)
e <- c(c, d) 


f1 <- c(1:100)
f2 <- c(f1, f1)

AvP <- data.frame(f2, l, e)


print(ggplot(AvP, aes(x = f2, y = l, color = e, shape = e)) + geom_point(size = 2) + 
        xlab("Last 100 samples") + ylab("Chance of Admission") + 
        geom_smooth(method = "lm", se = FALSE))

#######################################################################################



#Random Forest Regression

cat("\n\n")
print("For Random Forest Regression:")
cat("\n")
forest <- randomForest(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP + Research + CGPA + University.Rating, train)

prediction <- predict(forest, copy)

mse <- sqrt(mean((prediction-test$Chance.of.Admit)^2))
meanab <- mae(test$Chance.of.Admit, prediction)
print(paste("RMSE value is", mse))
print(paste("Mean Absolute Error is", meanab))

#Percentage Accuracy
accuracy <- 100 - (mean(abs((prediction-test$Chance.of.Admit)))*100)

print(paste("Accuracy is", accuracy, "%"))




#Decision Tree Regression

cat("\n\n")
print("For Decision Tree Regression:")
cat("\n")
decision <- rpart(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP + Research + CGPA + University.Rating, method = "anova", train)

prediction <- predict(decision, copy)

mse <- sqrt(mean((prediction-test$Chance.of.Admit)^2))
meanab <- mae(test$Chance.of.Admit, prediction)
print(paste("RMSE value is", mse))
print(paste("Mean Absolute Error is", meanab))

#Percentage Accuracy
accuracy <- 100 - (mean(abs((prediction-test$Chance.of.Admit)))*100)

print(paste("Accuracy is", accuracy, "%"))



############### Visualizing the relationship between different factors ######################

create <- read.table("Admission_Predict_Ver1.1.csv", sep = ",", header = TRUE)   #Creating new data
create$Serial.No. <- NULL


Uni_Rating <- as.factor(create$University.Rating)
Research <- as.factor(create$Research)

#Creating a dataframe with the different factors
temp <- data.frame(create$LOR, create$Chance.of.Admit, create$SOP, create$CGPA, create$GRE.Score, create$TOEFL.Score, Research, Uni_Rating)

print(ggplot(temp, aes(x = create$LOR, y = create$Chance.of.Admit, color = Uni_Rating)) 
      + geom_point(size = 2)
      + xlab("LOR") + ylab("Chance of Admit") 
      + geom_smooth(method = "lm", se = FALSE)
      + ggtitle("Relation between COA and LOR wrt Uni Rating"))

print(ggplot(temp, aes(x = create$SOP, y = create$Chance.of.Admit, color = Uni_Rating)) 
      + geom_point(size = 2)
      + xlab("SOP") + ylab("Chance of Admit") 
      + geom_smooth(method = "lm", se = FALSE)
      + ggtitle("Relation between COA and SOP wrt Uni Rating"))

print(ggplot(temp, aes(x = create$CGPA, y = create$Chance.of.Admit, color = Uni_Rating)) 
      + geom_point(size = 2)
      + xlab("CGPA") + ylab("Chance of Admit") 
      + geom_smooth(method = "lm", se = FALSE)
      + ggtitle("Relation between COA and CGPA wrt Uni Rating"))


print(ggplot(temp, aes(x = create$GRE.Score, y = create$Chance.of.Admit, color = Uni_Rating)) 
      + geom_point(size = 2)
      + xlab("GRE") + ylab("Chance of Admit") 
      + geom_smooth(method = "lm", se = FALSE)
      + ggtitle("Relation between COA and GRE wrt Uni Rating"))


print(ggplot(temp, aes(x = create$TOEFL.Score, y = create$Chance.of.Admit, color = Uni_Rating)) 
      + geom_point(size = 2)
      + xlab("TOEFL") + ylab("Chance of Admit") 
      + geom_smooth(method = "lm", se = FALSE)
      + ggtitle("Relation between COA and TOEFL wrt Uni Rating"))


print(ggplot(temp, aes(x = create$University.Rating, y = create$Chance.of.Admit, color = Research)) 
      + geom_point(size = 2)
      + xlab("University Rating") + ylab("Chance of Admit") 
      + geom_smooth(method = "lm", se = FALSE)
      + ggtitle("Relation between Chance of Admission and University Rating wrt Research"))

#######################################################################################################

#Taking an user's Input
GRE.Score <- readline(prompt="Enter your GRE score: ")
GRE.Score <- as.integer(GRE.Score)
while(GRE.Score > 340 || GRE.Score < 0){
  GRE.Score <- readline(prompt="Please re-enter in a valid score (0-340)")
  GRE.Score <- as.integer(GRE.Score)
}
TOEFL.Score <- readline(prompt="Enter your TOEFL score: ")
TOEFL.Score <- as.integer(TOEFL.Score)

while(TOEFL.Score > 120 || TOEFL.Score < 0){
  TOEFL.Score <- readline(prompt="Please re-enter in a valid score (0-120)")
  TOEFL.Score <- as.integer(TOEFL.Score)
}

SOP <- readline(prompt="Enter your SOP score: ")
SOP <- round(as.numeric(SOP), digits = 1)

while(SOP < 0 || SOP > 5){
  SOP <- readline(prompt="RE-nter your SOP score: ")
  SOP <- round(as.numeric(SOP), digits = 1)
}

LOR <- readline(prompt="Enter your LOR score: ")
LOR <- round(as.numeric(LOR), digits = 1)
while(LOR < 0 || LOR > 5){
  LOR <- readline(prompt="RE-nter your LOR score: ")
  LOR <- round(as.numeric(LOR), digits = 1)
}


CGPA <- readline(prompt="Enter your CGPA: ")
CGPA <- round(as.numeric(CGPA), digits = 2)
while(CGPA < 0 || CGPA > 10){
  CGPA <- readline(prompt="RE-nter your CGPA score: ")
  CGPA <- round(as.numeric(CGPA), digits = 2)
}


Research <- readline(prompt="Do you have any Research Experience? (enter 1 for yes and 0 for no): ")
Research <- as.factor(Research)

while(Research != 0 && Research != 1){
  Research <- readline(prompt="RE-nter your research score: ")
  Research <- as.factor(Research)
}

University.Rating <- readline(prompt="What rated university do you want to apply to: ")
University.Rating <- as.factor(University.Rating)

while(University.Rating != 0 && University.Rating != 5 && University.Rating != 4 && University.Rating != 3 && University.Rating != 2 && University.Rating !=1){
  University.Rating <- readline(prompt="RE-nter your University rating score: ")
  University.Rating <- as.factor(University.Rating)
}

cat("\n\n")

Chance.of.Admit <- 0   

#Creating a table with the user's input
tab <- matrix(c(GRE.Score, TOEFL.Score, University.Rating, SOP, LOR, CGPA, Research, Chance.of.Admit), ncol = 8, byrow=TRUE)
colnames(tab) <- c("GRE.Score", "TOEFL.Score", "University.Rating", "SOP", "LOR", "CGPA", "Research", "Chance.of.Admit")
tab <- as.table(tab)

#Making the prediction for the user's admission chances
predictionInput <- predict(modelL, tab)

print(paste("Your chance of admission for a rating", University.Rating, "University is", predictionInput))
  














