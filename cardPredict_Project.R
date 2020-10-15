
####Load required libraries
#install.packages("AER")
library(AER)
library(fBasics)

####Get the data
data("CreditCard")
head(CreditCard)
attach(CreditCard)
dim(CreditCard)

#--------------------------------A----------------------------------------------------------
####Provide summary stat of the predictors

#Create dummy variable for Owner
#We need a dummy variable for owner, because it is a descriptive variable, it is required in
#order to calculate the summary statistics

Dummy_Owner<-matrix(nrow=nrow(CreditCard),ncol=1)
colnames(Dummy_Owner)="Dummy_Owner"
for (i in 1:nrow(CreditCard))
{
  if (owner[i]=='yes') {
    Dummy_Owner[i,]=1
  } else {
    Dummy_Owner[i,]=0
  }
}

#Create new matrix using only the required variables
#CreditCard_New=as.data.frame(cbind(card,reports,income,age,Dummy_Owner,dependents,months,share))
CreditCard_New=cbind(CreditCard[,c(1,2,4,3,9,10,5)],Dummy_Owner)
colnames(CreditCard_New)=c("Card","Reports","Income","Age","Dependents","Months","Share","Owner")
detach(CreditCard)
attach(CreditCard_New)


pairs(CreditCard_New[,-1])
cor(CreditCard_New[,-1])

##################################################################################################
#The summary of the different columns splits the information into quartiles and tell us what is
#the median, mean, min and max value for each column.

#With this information we can check for extreme values and then decide if we want to include
#them in the analysis, in this specific case, it is easy to see that there are extreme
#values for the Age, where the minimun value is 0.1667, which indicates that we should probably
#exclude this values and clean the data before proceeding.

#We can see that the median income is 29K, and the mean income is 33.65K. The average age of 
#the people in the sample is 33 years and have 1 dependent, in average they have lived for 55
#months in their current address,

#It is important to note, the Owner is a dummy variable taking the value of 1 if person is a
#home owner and 0 if it is not a homeowner.
##################################################################################################

#--------------------------------B----------------------------------------------------------
####There are some values of variable age under one year. Consider data with age>18 for your 
####analysis for the rest of the questions.

#Create a new dataframe excluding all the observations where age is less than 18 years old
Adults=(Age>18)
CreditCard_New_Adults=CreditCard_New[Adults,]
detach(CreditCard_New)
dim(CreditCard_New_Adults)
attach(CreditCard_New_Adults)

#--------------------------------C----------------------------------------------------------
####Plot of income vs. reports (Number of major derogatory reports): mark individuals with 
####card application accepted as blue, and not accepted as red. What do you observe? 

####Create Dummy acceptance required for the plot

Dummy_Acceptance<-matrix(nrow=nrow(CreditCard_New_Adults),ncol=1)
colnames(Dummy_Acceptance)="Dummy_Acceptance"
for (i in 1:nrow(CreditCard_New_Adults))
{
  if (Card[i]=='yes') {
    Dummy_Acceptance[i,]=1
  } else {
    Dummy_Acceptance[i,]=0
  }
}

plot(Reports, Income, xlab='Reports', ylab='Income',  main='Income vs Reports', pch=1, 
     cex.main=1.5, frame.plot=FALSE, col=ifelse(Dummy_Acceptance==1, 'blue', 'red'))

##################################################################################################
#We can see that the number of derogatory reports is more important to get accepted for a 
#credit card than the level of income.

#The more the derogatory reports, the less probable that a person will get accepted for a 
#credit card, it can be seen that almost all the observations with 2 derogatory reports
#or more are rejected without regards to level of income.

#The income is not that relevant to get accepted for a credit card, credit cards seems to
#be accepted no matter the income level.
##################################################################################################

#--------------------------------D----------------------------------------------------------
####Boxplots of income as a function of card acceptance status.  Boxplots of reports as a 
####function of card acceptance status (mark card application accepted as blue, and not accepted as red). 
####What conclusion do you draw?

#Parameters to show 2 plots in the same page
par(mfcol=c(1,2))

#Boxplots of income as a function of credit acceptance
boxplot(Income~Dummy_Acceptance,data=CreditCard_New_Adults, xlab='Acceptance', ylab='Income', col=(c("red","blue"))) 

#Boxplots of Reports as a function of Acceptance status  
boxplot(Reports~Dummy_Acceptance,data=CreditCard_New_Adults, xlab='Acceptance', ylab='Reports', col=(c("red","blue")))

##################################################################################################
#We can corrobate our hypotesis of the previous question where we saw that the income level is not
#that relevant to get a credit card approved, but the number of derogatory reports is very important.

#The first plot (Income) shows that the median of income for credit card acceptance is just 
#a little bit higher than the median income for credit card rejection.

#The opposite happens in the second plot (Derogatory reports) where almost all the accepted
#credit cards are for people with no derogatory reports and it is almost certain that if a
#person has more derogatory reports is more likely to be rejected.
##################################################################################################

#--------------------------------E----------------------------------------------------------
####Construct the histogram for the predictors.

par(mfcol=c(1,1)) 

for (i in 2:ncol(CreditCard_New_Adults))
{
  hist(CreditCard_New_Adults[,i],nclass=30,main=paste("Histogram of",colnames(CreditCard_New_Adults[i])),
       xlab=colnames(CreditCard_New_Adults[i]),col="aquamarine")
}

##################################################################################################
#After running the histogram for all the explanatory variables, it can be seen that reports
#and share are higly right-skewed.

#Following the recommendation, going forward, the analysis will be done using the log(share) and 
#the log(reports+1) in order to reduce the skewness, given that highly skewed predictors have 
#high leverage points and are less likely to be linearly related to the response.
##################################################################################################

#--------------------------------F----------------------------------------------------------
####Use variables 2 to 8 to determine which of the predictors influence the probability that an 
####application is accepted. Use the summary function to print the results. Do any of the predictors 
####appear to be statistically significant? If so, which ones? Explain how each of the significant 
####predictors influences the response variable.

#Calculate the log of the highly skewed variables

LogReports=log(Reports+1)
LogShare=log(Share)

#Logistic Regression

glm.fit=glm(Card~LogReports+Income+Age+Owner+Dependents+Months+LogShare,
            data=CreditCard_New_Adults,family=binomial)
summary(glm.fit)

##################################################################################################
#Based on the z value, the income and the log of share are signinficant at 0 siginificance level,
#the log of reports is significant at a 0.001 signinficance level and the number of dependents
#is significant at a 0.01 level. This means that we can reject the null hypotesis that their
#coeficients equal to zero.

#Significant predictors:
#LogReports: The log odds of an application being accepted decreases 2.91 units for each unit of
#increase in the Log+1 of the derogatory reports.
#Income: For each 1 unit of increase in income (in this case 10K), the log odds of an application
#being accepted is increased by 0.9 units.
#Dependets:For each 1 additional dependent, the log odds of an application being accepted 
#decreases by 0.66 units.
#LogShare: When the Log of Share (ratio of monthly credit card expenditure to yearly income)
#increases in 1 unit, the log odds of a credit card being accepted increases in 3.42
#units.
##################################################################################################

#--------------------------------G----------------------------------------------------------
####Compute the confusion matrix and overall fraction of correct predictions. 


glm.probs=predict(glm.fit,type="response")

head(glm.probs)
length(glm.probs) 

#Contrast is not required because Acceptance is already a dummy variable
#indicates that R has created a dummy variable with a 1 for yes 
contrasts(Card) 

#To predict whether the application will be accepted or not, convert the predicted 
#probabilities into class labels yes or no.
glm.pred=rep("no",length(Card))
glm.pred[glm.probs>.5]="yes" 
table(glm.pred,Card)

#Computes fraction of the applications for which the prediction was correct
mean(glm.pred==Card)
#Test error rate
mean(glm.pred!=Card) 

##################################################################################################
#Based on the confussion matrix, the model correctly predicted  995  cards accepted 
#and incorrectly 2 applications as approved that were turned down (false positive: 2/295 = 0.007)

#The model correctly predicted 293 rejected applications, 
# and predicted as rejected but turned out to be accepted 22 applications (false negative)


# out of total 1017 credit card approvals, model incorrectly rejects 22 application (false negative)
## FN = 22/1017 = 0.021

#The fraction of correct predictions for the model (TN+TP)/Total is 98.17%, which means
#that the logistic model correctly predicted the result of an application more than 98% 
#of the times, with this we can say that the model is very accurate

#Out of 295 actual credit card rejections, the model correctly predicted 293 or 99.3% (Very high)
#Out of 1,017 actual credit card acceptances, the model correctly predicted 995 or 97.8%
#(Also very high)
##################################################################################################

#--------------------------------H----------------------------------------------------------
####Now fit the logistic regression model using a training data for observations 1 to 1000. 
####Compute the confusion matrix and the overall fraction of correct predictions for the 
####test data (that is, the data for observations 1001 to end of data.)

#Define the trian and test data
CreditCard_New_AdultsFit=cbind(CreditCard_New_Adults,LogReports,LogShare)
train=CreditCard_New_AdultsFit[1:1000,]
test=CreditCard_New_AdultsFit[1001:nrow(CreditCard_New_AdultsFit),]

#Fit the logistic regression using only the train data
fitTrain=glm(Card~LogReports+Income+Age+Owner+Dependents+Months+LogShare,
             data=train,family=binomial)
summary(fitTrain)

#Test the model by fitting the test data set into the fitted model
glm.probsTest=predict(fitTrain,test,type="response")
length(glm.probsTest)

#Compute the predictions and compare them to the actual values, create matrix with 312 rows
#initially set as no
glm.predTest=rep("no",nrow(test))

#Prediction, all probabilities above 0.5 change to yes
glm.predTest[glm.probsTest>.5]="yes"

#True value of the test subset
test.card=test[,1]

#Confussion matrix
table(glm.predTest,test.card)

# Test accuracy rate
mean(glm.predTest==test.card)

# Test error rate
mean(glm.predTest!=test.card)

##################################################################################################
#When dividing the data into a train subset and a test subset, we get very similar results
#as the ones obtained when using all the data available to fit the model.

#The model was fitted using only the train data (first 1,000 observations) and then tested in the
#test data (last 312 observations)

#The model correctly predicted 97.11% of the results, only had 6 false negative out of 238 accepted
#predictions(2.52%) and 3 false positive out of 74 (4.05%).

#Still dividing the data between trainning and testing the logistic models seems to be
#higly accurate.
##################################################################################################

#--------------------------------I----------------------------------------------------------
#Compare results in G and H and provide a discussion on the differences in prediction accuracy

##################################################################################################
#The model fraction of corrected predicitions using all the data is 98.17%, and the model
#fraction of corrected prediction spliting the data into train and test is 97.11%.

#The difference is a little bit over 1%, but we can say that for both models, the prediction
#level is highly accurate.

#The false negative error rate is 2.16% for the first model and 2.52% for the second model, both are 
#very close and the model prediction accuracy is confirmed again for both models

#The false positive error rate for the first model is 0.67% and 4.05% for the second one, even tought
#there is an increase in the false positive rate, it is still very low, and both models are
#very accurate. Also, from the perspective of the creditor it is more important to have a low
#false negative error rate and for both models it is very low.
##################################################################################################

library(e1071)  # For NB and SVM
library(caret)  #select tuning parameters
install.packages("DMwR")
library(DMwR)    # For KNN
library(MASS)   # contains the data
# install.packages("kknn")
library(kknn)   #weighted k-nearest neighbors

#SVM
svm.linear <- tune.svm(Card ~ ., data = train,
                       kernel = "linear",
                       cost = c(0.001, 0.01, 0.1, 1, 5, 10))
summary(svm.linear)
best.svm.linear <- svm.linear$best.model
best.svm.linear
svm.linear.pred <- predict(best.svm.linear, newdata = test)
confusionMatrix(svm.linear.pred, test$Card)

#NAIVE BAYES
pima.nb <- naiveBayes(Card ~.,data = train)
pima.nb
nb.pred.prob <- predict(pima.nb, newdata = test, type = "raw")
nb.pred.class <- predict(pima.nb, newdata = test)
confusionMatrix(as.factor(nb.pred.class), as.factor(test$Card))

# classification tree
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
card.ct <- rpart(Card ~ ., data = train, method = "class")


# plot using prp()
prp(card.ct, type = 5, extra = 101,  clip.right.lab = FALSE, 
    box.palette = "GnYlRd", leaf.round = 5, 
    branch = .3, varlen = -10, space=0)  

# Look at decision rules
rpart.rules(card.ct, extra = 4, cover = TRUE)

card.ct.pred.valid <- predict(card.ct,test,type = "class")
library(caret)
confusionMatrix(card.ct.pred.valid, as.factor(test$Card))

#Gradient Boost
library(gbm)
card.gbm <- gbm(Card ~ ., data = train, distribution = "gaussian", n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
summary(card.gbm)

#Neural

#Random Forest
library(randomForest)
rf <- randomForest(as.factor(Card) ~ ., data = train, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  
summary(rf)
## variable importance plot
varImpPlot(rf, type = 1)


#NEURAL NETWORK

library(neuralnet)
library(nnet)
library(caret)

predictors<-c("Card","Reports","Income","Age","Dependents","Months","Share","Owner")
outcomeName<-c("Card")

fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

model_nn<-train(train[,predictors],train[,outcomeName],method='nnet',
                trControl=fitControl,tuneLength=3)
test$pred_nn<-predict(object = model_nn,test[,predictors])
test$pred_nn.prob<-predict(object = model_nn,test[,predictors],
                           type="prob")

library(NeuralNetTools)
# Plot neural net
par(mfcol=c(1,1))
plotnet(model_nn)
# get the neural weights
neuralweights(model_nn)
# Plot the importance
olden(model_nn)


predictors<-c("Card","Reports","Income","Age","Dependents","Months","Share","Owner")
outcomeName<-c("Card")



# Build an Ensemble Model with Multiple Types of Models
# Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
predictors<-c("Reports","Income","Age","Dependents","Months","Share","Owner")
outcomeName<-c("Card")

#Training a random forest model
model_rf<-train(train[,predictors],train[,outcomeName],method='rf',
                trControl=fitControl,tuneLength=3)
#Predicting using random forest model
test$pred_rf<-predict(object = model_rf,test[,predictors])
test$pred_rf.prob<-predict(object = model_rf,test[,predictors],type="prob")
#Checking the accuracy of the random forest model
confusionMatrix(test$Card,test$pred_rf)

#Training a Logistic regression model
model_lr<-train(train[,predictors],train[,outcomeName],method='glm',
                trControl=fitControl,tuneLength=3)
#Predicting using logistic model
test$pred_lr<-predict(object = model_lr,test[,predictors])
test$pred_lr.prob<-predict(object = model_lr,test[,predictors],type="prob")
#Checking the accuracy of the logistic model
confusionMatrix(test$Card,test$pred_lr)


#Training a Naive Bayes model
model_nb<-train(train[,predictors],train[,outcomeName],method='nb',
                trControl=fitControl,tuneLength=3)
#Predicting using Naive Bayes model
test$pred_nb<-predict(object = model_nb,test[,predictors])
test$pred_nb.prob<-predict(object = model_nb,test[,predictors],type="prob")
#Checking the accuracy of the Naive Bayes model
confusionMatrix(test$Card,test$pred_nb)


# install.packages("gains")
library(gains)
test$Card.n = ifelse(test$Card == "X1", 1, 0)
test$pred_rf.n = ifelse(test$pred_rf == "X1", 1, 0)
test$pred_lr.n = ifelse(test$pred_lr == "X1", 1, 0)
test$pred_nb.n = ifelse(test$pred_nb == "X1", 1, 0)

