                               # S KALVIN NATHAN
                   #TASK1-PREDICTION USING SUPERVISED ML


#READING DATA FROM THE FILE
input<-read.csv("http://bit.ly/w-data")
head(input) #(displays first six rows)

#USING SCATTER PLOT TO VISUALISE RELATIONSHIP
scatter.smooth(x=input$Hours, y=input$Scores, main="Scores ~ Hours") 

#USING BOXPLOT FOR CHECKING OUTLIERS
# dividing graph area in 2 columns
par(mfrow=c(1, 2)) 
# box plot for 'Hours'
boxplot(input$Hours, main="Hours", sub=paste("Outlier rows: ", boxplot.stats(input$Hours)$out)) 
# box plot for 'Scores'
boxplot(input$Scores, main="Scores", sub=paste("Outlier rows: ", boxplot.stats(input$Scores)$out))  

#CORRELATION BETWEEN HOURS AND SCORES
cor(input$Hours, input$Scores)
#0.97 is close to 1 means 
#there exists a strong correlation between hours and scores.

# TESTING AND TRAINING DATA

# setting seed to reproduce results of random sampling
set.seed(100)  
# row indices for training data
trainingRowIndex <- sample(1:nrow(input), 0.8*nrow(input))  
# model training data
trainingData <- input[trainingRowIndex, ]
# test data
testData  <- input[-trainingRowIndex, ]   

# BUILDING MODEL ON TRAINING DATA

# build the model
lmMod <- lm(Scores ~ Hours, data=trainingData)
lmMod
summary(lmMod)

#PLOT OF THE MODEL
plot(input$Hours,input$Scores,xlab="No.of Hours studied",ylab="% scored",main="Hours vs. Scores",col="blue")
abline(lmMod,col="red")

# TO PREDICT SCORES
ScorePred <- predict(lmMod, testData)  
ScorePred
#predicting the score of student given hours(which works as a predictor)
hours<-data.frame(Hours=c(9.25))
result<-predict(lmMod,hours)
result

#COMPARING ACTUAL AND PREDICTED VALUES
df1 <- data.frame(cbind(actual=testData$Scores, predicted=ScorePred))  
# make actual and predicted dataframe.
head(df1)

#Calculation of correlation accuracy
correlation_accuracy <- cor(df1)# 82.7%
correlation_accuracy

