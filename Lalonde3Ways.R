##############################################################################################################################
"Loaded the Matching library and the lalonde dataset"
library(Matching)
library(arm)
#?lalonde
data(lalonde)

#################################################################################################################################
################################################################################################################################
#QUESTION 1
#################################################################################################################################
################################################################################################################################
#Stratify the data wrt degree
Degree1 = lalonde[lalonde$nodegr == 1,]
Degree0 = lalonde[lalonde$nodegr == 0,]

###############################################################################################################################
#Regression for re78  vs treat, educ, age, re74, u74, u75 and re75: DEGREE == 0
modelD1 = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ + black, data = Degree1)

#Details of statistical significance: For Degree == 1
summary(modelD1)
display(modelD1)
#SIMULATIONS DEG == 0

simD1 <- sim(modelD1)
simD1.coef <- coef(simD1)

#Simulated confint for model Deg == 1.
simD1.coef.df <- as.data.frame(simD1.coef)
simulationsConfidencesD1<-data.frame(quantile(simD1.coef.df$`(Intercept)` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`treat` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`re74` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`re75` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`u74` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`u75` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`age` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`educ` , c(0.025, 0.975)),
                                     quantile(simD1.coef.df$`black` , c(0.025, 0.975))
)
colnames(simulationsConfidencesD1) <- c("(Intercept)","treat", "re74", "re75", "u74", "u75", "age", "educ","black")
simulationsConfidencesD1
#################################################################################################################################

# RELEVANT PLOT For #1, Degree 1
library(sjPlot)
plot_model(modelD1, show.loess.ci = T, show.values = T, show.summary = T, title ="re78 for DEGREE ==1")
plot_model(modelD1, show.values = T, show.ci = T, title ="re78 for DEGREE ==1")

#################################################################################################################################

#Regression for re78  vs treat, educ, age, re74, u74, u75 and re75: DEGREE == 0
modelD0 = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ+black, data = Degree0)

#Details of statistical significance: For Degree == 1
summary(modelD0)
display(modelD0)
#SIMULATIONS DEG == 0

simD0 <-sim(modelD0)
simD0.coef <- coef(simD0)

#Simulated confint for model Deg == 1.
simD0.coef.df=as.data.frame(simD0.coef)
simulationsConfidencesD0<-data.frame(quantile(simD0.coef.df$`(Intercept)` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`treat` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`re74` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`re75` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`u74` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`u75` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`age` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`educ` , c(0.025, 0.975)),
                                     quantile(simD0.coef.df$`black` , c(0.025, 0.975))
                                     )
colnames(simulationsConfidencesD0)<- c("(Intercept)","treat", "re74", "re75", "u74", "u75", "age", "educ","black")
simulationsConfidencesD0
#################################################################################################################################

# RELEVANT PLOT For #1, Degree 0
library(sjPlot)
plot_model(modelD0, show.loess.ci = T, show.values = T, show.summary = T, title ="re78 for DEGREE == 0")
plot_model(modelD0, show.values = T, show.ci = T, title ="re78 for DEGREE == 0")

#################################################################################################################################

#################################################################################################################################
#Run the same simulations for the whole data set just to affirm the inferences.

modelFull = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ + black, data = lalonde)

#Details of statistical significance: For Full dataset.
summary(modelFull)
display(modelFull)

#SIMULATIONS Full dataset
simFull <-sim(modelFull)
simFull.coef <- coef(simFull)
simFull.coef.df <- as.data.frame(simFull.coef)
#simulated confint for model Full dataset.
simulationsConfidencesFull<-data.frame(quantile(simFull.coef.df$`(Intercept)` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`treat` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`re74` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`re75` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`u74` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`u75` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`age` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`educ` , c(0.025, 0.975)),
                                   quantile(simFull.coef.df$`black` , c(0.025, 0.975))
                                   )

colnames(simulationsConfidencesFull)<- c("(Intercept)","treat", "re74", "re75", "u74", "u75", "age", "educ","black")
simulationsConfidencesFull
#################################################################################################################################

# RELEVANT PLOT For #1, FULL
library(sjPlot)
plot_model(modelFull, show.loess.ci = T, show.values = T, show.summary = T, title ="re78 FOR FULL DATASET")
plot_model(modelFull, show.values = T, show.ci = T, title ="re78 FOR FULL DATASET")

#################################################################################################################################

#################################################################################################################################
#################################################################################################################################
#QUESTION 2
#################################################################################################################################
#################################################################################################################################
#Regression for re78  vs I(nodegr*treat): FULL
modelFull.interaction = glm(formula = re78 ~ I(nodegr*treat) + treat +nodegr + black, data = lalonde)

#Details of statistical significance: For Degree == 1
summary(modelFull.interaction)
display(modelFull.interaction)

#SIMULATIONS FULL
simFull.interaction <-sim(modelFull.interaction)
simFull.interaction.coef <- coef(simFull.interaction)

#Simulated confint for FULL.
simFull.interaction.coef.df=as.data.frame(simFull.interaction.coef)
simulationsConfidencesFull.interaction<-data.frame(quantile(simFull.interaction.coef.df$`I(nodegr * treat)` , c(0.025,0.975)),
                                                   quantile(simFull.interaction.coef.df$`nodegr` , c(0.025,0.975)),
                                                   quantile(simFull.interaction.coef.df$`treat` , c(0.025,0.975)),
                                                   quantile(simFull.interaction.coef.df$`black` , c(0.025,0.975))
                                                   )
colnames(simulationsConfidencesFull.interaction)<- c("I(nodegr * treat)", "nodegr", "treat", "black")
simulationsConfidencesFull.interaction
#################################################################################################################################

# RELEVANT PLOT For #2, FULL
library(sjPlot)
plot_model(modelFull.interaction, show.loess.ci = T, show.values = T, show.summary = T, title ="re78 FOR FULL DATASET with Interacttion")
plot_model(modelFull.interaction, show.values = T, show.ci = T, title ="re78 FOR FULL DATASET with Interaction")

#################################################################################################################################


#################################################################################################################################
#################################################################################################################################
#QUESTION 3
#################################################################################################################################
#################################################################################################################################
#Generate u78 based on the re74/u74 and re75/u75 criteria
lalonde$u78[lalonde$re78<=0]<-1 
lalonde$u78[lalonde$re78>0]<-0

#Stratify the data wrt degree
logitDegree1 = lalonde[lalonde$nodegr == 1,]
logitDegree0 = lalonde[lalonde$nodegr == 0,]

logitModD1 <- glm(formula = u78 ~ treat + re74 + re75 + u74 + u75 + re78 + age + educ + black, 
                  data=logitDegree1, family=binomial(link="logit"))
logitModD0 <- glm(formula = u78 ~ treat + re74 + re75 + u74 + u75 + re78 + age + educ + black, 
                  data=logitDegree0, family=binomial(link="logit"))



#SIMULATIONS DEG 1
logitSimD1 <-sim(logitModD1)
logitSimD1.coeff <- coef(logitSimD1)
logitSimD1.coef.df <- as.data.frame(logitSimD1.coeff)
#simulated confint for model Full dataset.
simulationsConfidencesLogD1<-data.frame(quantile(logitSimD1.coef.df$`(Intercept)` , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`treat` , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`re74`  , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`re75`  , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`u74`   , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`u75`   , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`re78`  , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`age`   , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`educ`  , c(0.025,0.975)),
                                   quantile(logitSimD1.coef.df$`black` , c(0.025,0.975))
                                   )

colnames(simulationsConfidencesLogD1)<- c("(Intercept)","treat", "re74", "re75", "u74", "u75","re78", "age", "educ","black")
display(logitModD1)
simulationsConfidencesLogD1

#SIMULATIONS LOGIT DEG 0
logitSimD0 <-sim(logitModD0)
logitSimD0.coeff <- coef(logitSimD0)
logitSimD0.coef.df <- as.data.frame(logitSimD0.coeff)
#simulated confint for model Full dataset.
simulationsConfidencesLogD0 <- data.frame(quantile(logitSimD0.coef.df$`(Intercept)` , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`treat`, c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`re74` , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`re75` , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`u74`  , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`u75`  , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`re78` , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`age`  , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`educ` , c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`black`, c(0.025,0.975)),
                                   quantile(logitSimD0.coef.df$`re78` , c(0.025,0.975))
                                   )

colnames(simulationsConfidencesLogD0)<- c("(Intercept)","treat", "re74", "re75", "u74", "u75", "re78", "age", "educ","black", "u78")
display(logitModD0)
simulationsConfidencesLogD0


#################################################################################################################################
#################################################################################################################################
#QUESTION 4
#################################################################################################################################
#################################################################################################################################
require(randomForest)
set.seed(101)

#################################################################################################################################
#Random Forests generated with a sample half the size of the observations.
#Bootstrap aggregation was applied here.
lalondeTraining <- sample(1:nrow(lalonde),1000, replace = TRUE)
lalondeTraining.rf <- randomForest(re78 ~ .,data = lalonde , subset = lalondeTraining,importance=TRUE)
lalondeTraining.rf
oob.err <- double(12)
test.err <- double(12)

for(mtry in 1:12) 
{
  rf <- randomForest(re78 ~ . , data = lalonde , subset = lalondeTraining, mtry = mtry, ntree=230) 
  oob.err[mtry] = rf$mse[230] #Error of all Trees fitted
  
  lalondePred <- predict(rf,lalonde[-lalondeTraining,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(lalonde[-lalondeTraining,], mean( (re78 - lalondePred)^2)) #Mean Squared Test Error
  cat(mtry," ") #printing the output to the console
}

test.err
oob.err

#Plot test error and OOB error to see if they correlate and where error is minimized

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",
        ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
importance(lalondeTraining.rf)

#Random Forests with bagging here
bagNoDegSetVars <- randomForest(re78 ~ treat +re74 +re75+u74+u75 + age + educ + black, 
                            data = Degree0, ntree = 540, importance=TRUE, replace= TRUE)

importance(bagNoDegSetVars)
bagDegSetVars <- randomForest(re78 ~ treat +re74 +re75+u74+u75 + age + educ + black, 
                            data = Degree1, ntree = 540, importance=TRUE)
importance(bagDegSetVars)
#Plotted the variable importance

varImpPlot(lalondeTraining.rf, main = "Trained forest Variable Importance Plot")
varImpPlot(bagNoDegSetVars, main = "nodegr==0 Variable Importance Plot")
varImpPlot(bagDegSetVars, main = "nodegr==1 Variable Importance Plot")

#Apply LOOCV

library(ISLR)
library(boot)
set.seed(1)


#################################################################################################################################
#################################################################################################################################
#LOOCV AND K FOLD
#################################################################################################################################
#################################################################################################################################
#LOOCV

modelD0 = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ+black, data = Degree0)
modelD1 = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ + black, data = Degree1)
modelFull = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ + black, data = lalonde)


MSE_LOOCVD0 <- NULL
MSE_LOOCVD1 <- NULL
MSE_LOOCVFULL <- NULL

for (i in 1:10){
  modelD0.loop <- glm(formula = re78 ~ poly(treat +re74 +re75+u74+u75 + age + educ+black, i), data = Degree0)
  modelD1.loop <- glm(formula = re78 ~ poly(treat +re74 +re75+u74+u75 + age + educ+black, i), data = Degree1)
  modelFULL.loop <- glm(formula = re78 ~ poly(treat +re74 +re75+u74+u75 + age + educ+black, i), data = lalonde)
  MSE_LOOCVD0[i] <- cv.glm(Degree0 ,modelD0.loop)$delta[1]
  MSE_LOOCVD1[i] <- cv.glm(Degree1 ,modelD1.loop)$delta[1]
  MSE_LOOCVFULL[i] <- cv.glm(lalonde ,modelFULL.loop)$delta[1]
}
#MSE D0
MSE_LOOCVD0
#MSE D1
MSE_LOOCVD1
#MSE FULL
MSE_LOOCVFULL

#K-Fold: Performed a 10 fold cross validation.

modelD0 = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ+black, data = Degree0)
modelD1 = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ+black, data = Degree1)
modelFull = glm(formula = re78 ~ treat +re74 +re75+u74+u75 + age + educ+black, data = lalonde)


MSE_10F_D0 <- NULL
MSE_10F_D1 <- NULL
MSE_10F_FULL <- NULL

for (i in 1:10){
  modelD0.loop <- glm(formula = re78 ~ poly(treat +re74 +re75+u74+u75 + age + educ+black, i), data = Degree0)
  modelD1.loop <- glm(formula = re78 ~ poly(treat +re74 +re75+u74+u75 + age + educ+black, i), data = Degree1)
  modelFULL.loop <- glm(formula = re78 ~ poly(treat +re74 +re75+u74+u75 + age + educ+black, i), data = lalonde)
  MSE_10F_D0[i] <- cv.glm(Degree0 ,modelD0.loop, K = 10)$delta[1]
  MSE_10F_D1[i] <- cv.glm(Degree1 ,modelD1.loop, K = 10)$delta[1]
  MSE_10F_FULL[i] <- cv.glm(lalonde ,modelFULL.loop, K = 12)$delta[1]
}
#MSE D0
MSE_10F_D0
#MSE D1
MSE_10F_D1
#MSE FULL
MSE_10F_FULL
