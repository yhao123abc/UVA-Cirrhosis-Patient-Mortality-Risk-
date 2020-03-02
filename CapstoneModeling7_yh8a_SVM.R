 #CAPSTONE TEAM PROJECT
#Elizabeth Homan, Myron Chang, Yi Hao

library(tidyverse) # Load the core tidyverse packages: ggplot2, tibble, 
# tidyr, readr, purrr, and dplyr
library(readxl)
library(dplyr)
library(ggplot2)
library(mefa4)
library(ROSE)
library(rpart)
library(splines)
library(gam)
library(gbm)
library(randomForest)
library(e1071)
library(ROCR)
library(gplots)


#setwd("~/Desktop/Capstone/Data")

#Read in csv file
Testing <- read.csv("Full_Imputed4_Validation.csv", encoding = 'utf-8', stringsAsFactors = FALSE)

Test <- Testing[,c(1,8:68,72)]

Train <- read.csv("Compressed4_Training_MaxDeltas_Imputed.csv", encoding = 'utf-8', stringsAsFactors = FALSE)

numericColumns = c("ageAtAdmission","ART_OXY_SAT_VALUE","VEN_OXY_SAT_VALUE","BICARBONATE_VALUE",
                   "PO2_VALUE","PCO2_VALUE","PLATELET_VALUE","HEMATOCRIT_VALUE","HEMOGLOBIN_VALUE",
                   "WBC_VALUE","RBC_VALUE","ANION_GAP_VALUE","ALT_VALUE","AST_VALUE","GLUCOSE_VALUE",
                   "BUN_VALUE","CO2_VALUE","BILIRUBIN_VALUE","CREATININE_VALUE","INR_VALUE",
                   "CHLORIDE_VALUE","POTASSIUM_VALUE","SODIUM_VALUE","SODIUM_VALUE_MELD","LACTIC_ACID_VALUE",
                   "ALBUMIN_VALUE","TempF","Resp","Pulse","SysBP","DiaBP","SPO2","NuDESC","NEWS","CLiF","MELDi",
                   "MELD","missing_ARTOXYSATVALUE")

#as.numeric
for(i in numericColumns){ Train[,i]<-as.numeric(Train[,i])}
for(i in numericColumns){ Test[,i]<-as.numeric(Test[,i])}

#FACTORS
factorColumns <- c("Pt_Mask","Sex","Race","Ethnicity","AdmissionSource","DischargeDisposition",
                   "HOSP_ADMSN_TYPE","admPhysDept","admPhysDiv","alcHepDx","alcUseDx","evDx",
                   "gvDx","hepCarDx","hepEncDx","sbpDx","ciwaPresence","cirrhosis_dxName",
                   "cirrhosis_DxCde","cirrhosisPriority","clrtDx_Primary","topPrioDx","indexMort")

#as.factor
for(i in factorColumns){ Train[,i]<-as.factor(Train[,i])}
for(i in (factorColumns)){ Test[,i]<-as.factor(Test[,i])}

# Checking the data for missing values
# Training data
sapply(Train, function(x) sum(is.na(x)))

for (i in 1:ncol(Train)){
  missingvalues <- sum(is.na(Train[,i]))
  if (missingvalues > 0){
    columnname <- colnames(Train[i])
    print(columnname)
    print(missingvalues)
  }
}

# Testing data
for (i in 1:ncol(Test)){
  missingvalues <- sum(is.na(Test[,i]))
  if (missingvalues > 0){
    columnname <- colnames(Test[i])
    print(columnname)
    print(missingvalues)
  }
}

# Checking the data for -1 values
# Training data
for (i in 1:ncol(Train)){
  negativeonevalues <- which(Train[,i] == -1)
  if (length(negativeonevalues) > 0){
    columnname <- colnames(Train[i])
    print(columnname)
    print(length(negativeonevalues))
  }
}

for (i in 1:ncol(Test)){
  negativeonevalues <- which(Test[,i] == -1)
  if (length(negativeonevalues) > 0){
    columnname <- colnames(Test[i])
    print(columnname)
    print(length(negativeonevalues))
  }
}




## ------- Support Vector Machine Model ---------- ##
library(e1071)
set.seed(777)

#Subset data for train and test

Train1 <- subset(Train, select=c("PLATELET_VALUE","HEMATOCRIT_VALUE", "HEMOGLOBIN_VALUE",
                                   "WBC_VALUE", "RBC_VALUE", "ANION_GAP_VALUE", "ALT_VALUE", "AST_VALUE", "GLUCOSE_VALUE",
                                   "BUN_VALUE", "CO2_VALUE", "BILIRUBIN_VALUE", "CREATININE_VALUE", "INR_VALUE",
                                   "CHLORIDE_VALUE", "POTASSIUM_VALUE", "SODIUM_VALUE", "SODIUM_VALUE_MELD",
                                   "ALBUMIN_VALUE", "TempF", "Resp", "Pulse", "SysBP", "DiaBP", "SPO2","indexMort"))

Test1 <- subset(Test, select=c("PLATELET_VALUE","HEMATOCRIT_VALUE", "HEMOGLOBIN_VALUE",
                                 "WBC_VALUE", "RBC_VALUE", "ANION_GAP_VALUE", "ALT_VALUE", "AST_VALUE", "GLUCOSE_VALUE",
                                 "BUN_VALUE", "CO2_VALUE", "BILIRUBIN_VALUE", "CREATININE_VALUE", "INR_VALUE",
                                 "CHLORIDE_VALUE", "POTASSIUM_VALUE", "SODIUM_VALUE", "SODIUM_VALUE_MELD",
                                 "ALBUMIN_VALUE", "TempF", "Resp", "Pulse", "SysBP", "DiaBP", "SPO2", "indexMort"))

Train1$indexMort <- as.factor(Train1$indexMort)
Test1$indexMort <- as.factor(Test1$indexMort)

#Check NAs
sum(is.na(Train1))
dim(Train1)

sum(is.na(Test1))
dim(Test1)

# Checking the data for -1 values
# Training data
for (i in 1:ncol(Train1)){
  negativeonevalues <- which(Train1[,i] == -1)
  if (length(negativeonevalues) > 0){
    columnname <- colnames(Train1[i])
    print(columnname)
    print(length(negativeonevalues))
  }
}

for (i in 1:ncol(Test1)){
  negativeonevalues <- which(Test1[,i] == -1)
  if (length(negativeonevalues) > 0){
    columnname <- colnames(Test1[i])
    print(columnname)
    print(length(negativeonevalues))
  }
}

Train2 <- Train1[Train1$HEMATOCRIT_VALUE != -1, ]
Train2 <- Train2[Train2$HEMOGLOBIN_VALUE != -1, ]
Train2 <- Train2[Train2$ALT_VALUE != -1, ]
Train2 <- Train2[Train2$AST_VALUE != -1, ]
Train2 <- Train2[Train2$BUN_VALUE != -1, ]
Train2 <- Train2[Train2$POTASSIUM_VALUE != -1, ]
Train2 <- Train2[Train2$ALBUMIN_VALUE != -1, ]

dim(Train2)

Test2 <- Test1[Test1$HEMATOCRIT_VALUE != -1, ]
Test2 <- Test2[Test2$HEMOGLOBIN_VALUE != -1, ]
Test2 <- Test2[Test2$ANION_GAP_VALUE != -1, ]
Test2 <- Test2[Test2$ALT_VALUE != -1, ]
Test2 <- Test2[Test2$AST_VALUE != -1, ]
Test2 <- Test2[Test2$BUN_VALUE != -1, ]
Test2 <- Test2[Test2$CO2_VALUE != -1, ]
Test2 <- Test2[Test2$POTASSIUM_VALUE != -1, ]
Test2 <- Test2[Test2$ALBUMIN_VALUE != -1, ]

dim(Test2)

for (i in 1:ncol(Train2)){
  negativeonevalues <- which(Train2[,i] == -1)
  if (length(negativeonevalues) > 0){
    columnname <- colnames(Train2[i])
    print(columnname)
    print(length(negativeonevalues))
  }
}

for (i in 1:ncol(Test2)){
  negativeonevalues <- which(Test2[,i] == -1)
  if (length(negativeonevalues) > 0){
    columnname <- colnames(Test2[i])
    print(columnname)
    print(length(negativeonevalues))
  }
}

#Build SVM Linear Model
set.seed(1)
#svmfit <-svm(indexMort~PLATELET_VALUE+ HEMATOCRIT_VALUE + HEMOGLOBIN_VALUE+
#                          WBC_VALUE+ RBC_VALUE+ ANION_GAP_VALUE + ALT_VALUE + AST_VALUE + GLUCOSE_VALUE
#                        + BUN_VALUE + CO2_VALUE + BILIRUBIN_VALUE + CREATININE_VALUE + INR_VALUE +
#                          CHLORIDE_VALUE + POTASSIUM_VALUE + SODIUM_VALUE + SODIUM_VALUE_MELD + 
#                          ALBUMIN_VALUE + TempF + Resp + Pulse + SysBP +DiaBP, kernel = "linear", data=Train2)

svmfit <-svm(indexMort~., kernel = "linear", data=Train2)
summary(svmfit)

pred <- predict(svmfit, Test2)
tab <- table(predict = pred, truth = Test2$indexMort)
tab
mis <- 1-sum(diag(tab))/sum(tab)
mis  #[1] 0.1465003

#Tune

tune.out <- tune(svm, indexMort~., data = Train2, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

svmfit <-svm(indexMort~., kernel = "linear", data=Train2, cost=1, scale = FALSE)
summary(svmfit)

pred <- predict(svmfit, Test2)
tab <- table(predict = pred, truth = Test2$indexMort)
tab
mis <- 1-sum(diag(tab))/sum(tab)
mis   #[1] 0.1399872



## SVM Non-Linear Model
svmfit <-svm(indexMort~., data=Train2)
summary(svmfit)

pred <- predict(svmfit, Test2)
tab <- table(predict = pred, truth = Test2$indexMort)
tab
mis <- 1-sum(diag(tab))/sum(tab)
mis  #[1] 0.1153177

#Tune

tune.out <- tune(svm, indexMort~., data = Train2, kernel = "radial", 
                 ranges = list(cost = c(0.01, 0.1, 1, 10, 100, 1000), gamma = c(0.5, 1,2,3,4)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

svmfit <-svm(indexMort~., kernel = "radial", data=Train2, cost=0.01, gamm = 0.5, scale = FALSE)
summary(svmfit)

pred <- predict(svmfit, Test2)
tab <- table(predict = pred, truth = Test2$indexMort)
tab
mis <- 1-sum(diag(tab))/sum(tab)
mis  




        # Parameters:
        #   SVM-Type:  C-classification 
        # SVM-Kernel:  radial 
        #       cost:  1 
        #      gamma:  0.04 
        # 
        # Number of Support Vectors:  335
        # 
        # ( 184 151 )
        # 
        # 
        # Number of Classes:  2 
        # 
        # Levels: 
        #   0 1

#plot(svmfit, Train1)

#Confusion Matrix & Misclassification Error

pred1 <- predict(svmfit, data= Train1)
length(pred1)
dim(Train1)

tab <- table(Predicted = pred1, Actual = Train1$indexMort)
tab
      #               Actual
      # Predicted     0   1
      #         0   668  29
      #         1     8 140

mis <- 1-sum(diag(tab))/sum(tab)
mis
      #[1] 0.04378698


## Validation using Test1 data set
 
#sapply(Test, function(x) sum(is.na(x)))

pred2 <- predict(svmfit, newdata = Test1)
length(pred2)
nrow(Test1)

tab2 <- table(pred2, Test1$indexMort)
tab2
      # pred2     0     1
      #     0 61778  3542
      #     1  4878  1530

mis <- 1-sum(diag(tab2))/sum(tab2)
mis
       #[1] 0.1173879

specificity <- 61778/(4878 + 61778)  #0.9268183
specificity
sensitivity <- 3542/(3542+ 1530)     #0.6983438
sensitivity
ppv <- 1530/(4878+1530)     #0.238764
ppv
npv <-61778/(61778+3542)    #0.238764
npv


##Linear SVM

svmfitl <-svm(indexMort~PLATELET_VALUE+ HEMATOCRIT_VALUE + HEMOGLOBIN_VALUE+
               WBC_VALUE+ RBC_VALUE+ ANION_GAP_VALUE + ALT_VALUE + AST_VALUE + GLUCOSE_VALUE
             + BUN_VALUE + CO2_VALUE + BILIRUBIN_VALUE + CREATININE_VALUE + INR_VALUE +
               CHLORIDE_VALUE + POTASSIUM_VALUE + SODIUM_VALUE + SODIUM_VALUE_MELD + 
               ALBUMIN_VALUE + TempF + Resp + Pulse + SysBP +DiaBP, data=Train1, kernel="linear")

summary(svmfitl)

#Confusion Matrix & Misclassification Error

predl <- predict(svmfitl, data= Train1)
length(predl)
dim(Train1)

tab <- table(Predicted = predl, Actual = Train1$indexMort)
tab
#               Actual
# Predicted     0   1
#         0   668  29
#         1     8 140

mis <- 1-sum(diag(tab))/sum(tab)
mis
#[1] 0.04378698


## Validation using Test1 data set

#sapply(Test, function(x) sum(is.na(x)))

pred2 <- predict(svmfitl, newdata = Test1)
length(pred2)
nrow(Test1)

tab2 <- table(pred2, Test1$indexMort)
tab2
# pred2     0     1
#     0 61778  3542
#     1  4878  1530

mis <- 1-sum(diag(tab2))/sum(tab2)
mis
#[1] 0.1173879




##Tuning (hyper parameter optimization)
set.seed(123)

# tune.out <-tune(svm,indexMort~ PLATELET_VALUE+ HEMATOCRIT_VALUE + HEMOGLOBIN_VALUE+
#                 WBC_VALUE+ RBC_VALUE+ ANION_GAP_VALUE + ALT_VALUE + AST_VALUE + GLUCOSE_VALUE
#               + BUN_VALUE + CO2_VALUE + BILIRUBIN_VALUE + CREATININE_VALUE + INR_VALUE +
#                 CHLORIDE_VALUE + POTASSIUM_VALUE + SODIUM_VALUE + SODIUM_VALUE_MELD + E
#                 ALBUMIN_VALUE + TempF + Resp + Pulse + SysBP +DiaBP + SPO2, data=Train1, 
#                 ranges =list(cost=c(0.1, 1, 10 ,100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
                
                #ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:7)))


#tmodel <-tune.svm(indexMort~ PLATELET_VALUE+ HEMATOCRIT_VALUE + HEMOGLOBIN_VALUE+
#                    WBC_VALUE+ RBC_VALUE+ ANION_GAP_VALUE + ALT_VALUE + AST_VALUE + GLUCOSE_VALUE
#                  + BUN_VALUE + CO2_VALUE + BILIRUBIN_VALUE + CREATININE_VALUE + INR_VALUE +
#                    CHLORIDE_VALUE + POTASSIUM_VALUE + SODIUM_VALUE + SODIUM_VALUE_MELD + 
#                    ALBUMIN_VALUE + TempF + Resp + Pulse + SysBP +DiaBP + SPO2, data=Train1,
#                gamma = seq(0,1,0.1), cost = 2^(2:7))

tune.out <-tune(svm,indexMort~ ., data=Train1, 
                ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:7)))

summary(tune.out)

          # Parameter tuning of 'svm':
          #   
          #   - sampling method: 10-fold cross validation 
          # 
          # - best parameters:
          #   gamma cost
          #    0.1    4
          # 
          # - best performance: 0.1551261 


summary(tune.out$best.model)
        # Parameters:
        #   SVM-Type:  C-classification 
        # SVM-Kernel:  radial 
        #       cost:  4 
        #      gamma:  0.04
        # 
        # Number of Support Vectors:  349
        # 
        # ( 298 168 )
        # 
        # 
        # Number of Classes:  2 
        # 
        # Levels: 
        #   0 1

pred=predict(tune.out$best.model, newdata =Test1)
tunetest_tab <- table(true=Test1$indexMort, pred)
tunetest_tab

mis3 <- 1-sum(diag(tunetest_tab))/sum(tunetest_tab)
mis3




## Train error
#Confusion Matrix & Misclassification Error

pred3 <- predict(fmodel, newdata= Train1)
length(pred3)
dim(Train1)

tab3 <- table(Predicted = pred3, Actual = Train1$indexMort)
tab3
#               Actual
# Predicted     0   1
#         0   686  33
#         1     2 139

mis3 <- 1-sum(diag(tab3))/sum(tab3)
mis3
#[1] 0.04069767



## Validation again using Test1 data set

pred4 <- predict(fmodel, newdata = Test1)
length(pred4)
nrow(Test1)

tab4 <- table(pred4, Test1$indexMort)
tab4

mis4 <- 1-sum(diag(tab4))/sum(tab4)
mis4


##ROC curve
library(ROCR)
library(gplots)

rocplot =function(pred, truth, ...){
  predob = prediction (pred, truth)
  perf = performance (predob, "tpr", "fpr")
  plot(perf ,...)}

pred <- prediction(c(0.1,.5,.3,.8,.9,.4,.9,.5), c(0,0,0,1,1,1,1,1))
perf <- performance(pred, "tpr", "fpr")
plot(perf)

plotROC <- function(truth, predicted, ...){
  pred <- prediction(abs(predicted), truth)    
  perf <- performance(pred,"tpr","fpr")
  
  plot(perf, ...)
}

svmfit.opt=svm(indexMort~., data=Train1, kernel ="radial",
                gamma =0.1, cost=4, decision.values =T)

fitted =attributes(predict(svmfit.opt, Test1, decision.values =TRUE))$decision.values

head(fitted, 100)
range(fitted)
par(mfrow =c(1,1))
rocplot(fitted, Test1$indexMort, main="Test Data")




#### RESTRICT MORTALITY PATIENT DATA TO 48-24 HOURS BEFORE DEATH ####

Test_Mort <- Testing[which(Testing$indexMort==1),]

Test_Mort$FirstEventDtm <- as.POSIXct(Test_Mort$FirstEventDtm)
Test_Mort$Event_Dtm <- as.POSIXct(Test_Mort$Event_Dtm)
Test_Mort$AdmDtm <- as.POSIXct(Test_Mort$AdmDtm)
Test_Mort$DschDtm <- as.POSIXct(Test_Mort$DschDtm)
Test_Mort$Time48Prior <- as.POSIXct(Test_Mort$Time48Prior)
Test_Mort$Time48PriorAdj <- as.POSIXct(Test_Mort$Time48PriorAdj)

Test_Mort$Time24Prior <- (Test_Mort$DschDtm-86400)

Test_Mort_24Prior <- which(Test_Mort$Event_Dtm < Test_Mort$Time24Prior)

Test2_Mort <- Test_Mort[Test_Mort_24Prior,]

Test2 <- Testing[which(Testing$indexMort==0),]
Test2_Mort <- Test2_Mort[,!(names(Test2_Mort) %in% "Time24Prior")]
Test_Final <- rbind(Test2,Test2_Mort)

Test_Final$indexMort <- as.factor(Test_Final$indexMort)
dim(Test_Final)
sapply(Test_Final, function(x) sum(is.na(x)))

# Prediction
pred2 <- predict(svm.mod, newdata = Test_Final, type="response")
length(pred2)
head(pred2, 10)

tab2 <- table(Predicted = pred2, Actual = Test_Final$indexMort)
tab2

mis <- 1-sum(diag(tab2))/sum(tab2)
mis


probs <- as.vector(predict(svm.mod,newdata=Test_Final, type="prob"))
tail(probs, 10)
length(probs)
roc.curve(Test_Final$indexMort, probs)


#Model 2 using top 15 predictors

#svm.mod2 <- svm(indexMort~ Pulse+SPO2 +BUN_VALUE+WBC_VALUE+BILIRUBIN_VALUE+SODIUM_VALUE
#                +INR_VALUE+SysBP+CO2_VALUE+CREATININE_VALUE+ANION_GAP_VALUE+HEMOGLOBIN_VALUE+PLATELET_VALUE
#               +POTASSIUM_VALUE+RBC_VALUE, data = Train)
#



# Testing the random forest model
probs <- as.vector(probs)

roc.curve(Test$indexMort, pred2)
#Area under the curve (AUC): 0.777
