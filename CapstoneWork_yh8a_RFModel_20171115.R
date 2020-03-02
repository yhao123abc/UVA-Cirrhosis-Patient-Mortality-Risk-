#install.packages("ROCR")
library(ROCR)
library(tidyverse) 
library(readr)  
library(dplyr)  
library(ggplot2) 

library(readxl)
library(mefa4)

library(ROSE)
library(rpart)

Train <- read.csv("Compressed2_Training_MaxDeltas_Imputed.csv", encoding = 'utf-8', stringsAsFactors = FALSE)
Test <- read.csv("Full_Imputed_Validation.csv", encoding = 'utf-8', stringsAsFactors = FALSE)

#Exploration of Train
summary(Train)     
str(Train)
dim(Train)   #[1]   860   63

#check NA's
sapply(Train, function(x) sum(is.na(x)))

#check table
table(Train$indexMort)
        #  0   1 
        #688 172 


#Exploration of Test
dim(Test)    #[1] 71728   71

#check NA's
sapply(Test, function(x) sum(is.na(x)))

#check table
table(Test$indexMort)
        #    0     1 
        #67215  4513 


#Convert columns to numeric:
numericColumns = c("ageAtAdmission","ART_OXY_SAT_VALUE","VEN_OXY_SAT_VALUE","BICARBONATE_VALUE",
                   
                   "PO2_VALUE","PCO2_VALUE","PLATELET_VALUE","HEMATOCRIT_VALUE","HEMOGLOBIN_VALUE",
                   
                   "WBC_VALUE","RBC_VALUE","ANION_GAP_VALUE","ALT_VALUE","AST_VALUE","GLUCOSE_VALUE",
                   
                   "BUN_VALUE","CO2_VALUE","BILIRUBIN_VALUE","CREATININE_VALUE","INR_VALUE",
                   
                   "CHLORIDE_VALUE","POTASSIUM_VALUE","SODIUM_VALUE","SODIUM_VALUE_MELD","LACTIC_ACID_VALUE",
                   
                   "ALBUMIN_VALUE","TempF","Resp","Pulse","SysBP","DiaBP","SPO2","NuDESC","NEWS","CLiF","MELDi",
                   
                   "MELD","indexMort")


Train[numericColumns] <- lapply(Train[numericColumns], as.numeric) 
Test[numericColumns] <- lapply(Test[numericColumns], as.numeric) 


#Convert columns to factor:
factorColumns <- c("Pt_Mask","Sex","Race","Ethnicity","AdmissionSource","DischargeDisposition",
                   
                   "HOSP_ADMSN_TYPE","admPhysDept","admPhysDiv","alcHepDx","alcUseDx","evDx",
                   
                   "gvDx","hepCarDx","hepEncDx","sbpDx","ciwaPresence","cirrhosis_dxName",
                   
                   "cirrhosis_DxCde","cirrhosisPriority","clrtDx_Primary","topPrioDx")


Train[factorColumns] <- lapply(Train[factorColumns], as.factor) 
Test[factorColumns] <- lapply(Test[factorColumns], as.factor) 


##########################################################
#                                                        #
#----             Random Forest Model              ----- #
#                                                        #
##########################################################

library(plyr)
library(MASS)
library(randomForest)

set.seed(777)

#Convert response variable to factor
Train$indexMort <- as.factor(Train$indexMort)


##Fit a model using random forest

predictor <- c("PLATELET_VALUE","HEMATOCRIT_VALUE","HEMOGLOBIN_VALUE","WBC_VALUE","RBC_VALUE",
               "ANION_GAP_VALUE","ALT_VALUE","AST_VALUE","GLUCOSE_VALUE", "BUN_VALUE", "CO2_VALUE",
               "BILIRUBIN_VALUE","CREATININE_VALUE","INR_VALUE","CHLORIDE_VALUE","POTASSIUM_VALUE",
               "SODIUM_VALUE", "SODIUM_VALUE_MELD","ALBUMIN_VALUE","TempF",
               "Resp", "Pulse","SysBP", "DiaBP", "SPO2" )


rf.mod1 <-randomForest(indexMort~PLATELET_VALUE+ HEMATOCRIT_VALUE + HEMOGLOBIN_VALUE+
                         
                         WBC_VALUE+ RBC_VALUE+ ANION_GAP_VALUE + ALT_VALUE + AST_VALUE + GLUCOSE_VALUE
                       
                       + BUN_VALUE + CO2_VALUE + BILIRUBIN_VALUE + CREATININE_VALUE + INR_VALUE +
                         
                         CHLORIDE_VALUE + POTASSIUM_VALUE + SODIUM_VALUE + SODIUM_VALUE_MELD + 
                         
                         ALBUMIN_VALUE + TempF + Resp + Pulse + SysBP +DiaBP + SPO2, data=Train, ntree= 500, na.rm = TRUE,
                       
                         mtry = 5, importance = TRUE, proximity = TRUE)

print(rf.mod1)


## Predict and Confusion Matrix ---- Train
library(caret)
p1 <- predict(rf.mod1, data= Train)
head(p1)
head(Train$indexMort)

#Confusion Matrix
confusionMatrix(p1, Train$indexMort)

#plot rf.mod1
par(mfrow=c(1,1))
plot(rf.mod1)


##Predict and Confusion Matrix ---- Test
##RESTRICT MORTALITY PATIENT DATA TO 48-24 HOURS BEFORE DEATH

Test_Mort <- Test[which(Test$indexMort==1),]

Test_Mort$FirstEventDtm <- as.POSIXct(Test_Mort$FirstEventDtm)
Test_Mort$Event_Dtm <- as.POSIXct(Test_Mort$Event_Dtm)
Test_Mort$AdmDtm <- as.POSIXct(Test_Mort$AdmDtm)
Test_Mort$DschDtm <- as.POSIXct(Test_Mort$DschDtm)
Test_Mort$Time48Prior <- as.POSIXct(Test_Mort$Time48Prior)
Test_Mort$Time48PriorAdj <- as.POSIXct(Test_Mort$Time48PriorAdj)

Test_Mort$Time24Prior <- (Test_Mort$DschDtm-86400)

Test_Mort_24Prior <- which(Test_Mort$Event_Dtm < Test_Mort$Time24Prior)

Test2_Mort <- Test_Mort[Test_Mort_24Prior,]

Test2 <- Test[which(Test$indexMort==0),]
Test2_Mort <- Test2_Mort[,!(names(Test2_Mort) %in% "Time24Prior")]
Test_Final <- rbind(Test2,Test2_Mort)


## Prediction using Test set removed the last 24hrs: Test_final

set.seed(777)
Test_Final$indexMort <- as.factor(Test_Final$indexMort)
p2 <- predict(rf.mod1, newdata= Test_Final)
confusionMatrix(p2, Test_Final$indexMort)


#ROC curve
#install.packages("ROCR")
library(ROCR)

pred <- predict(rf.mod1, Test_Final, type = "prob")
pred <- prediction(pred[ ,2], Test_Final$indexMort)

#ROC curve
roc <- performance(pred, 'tpr', 'fpr')
plot(roc, colorize = T, main = "ROC Curve", lwd =2, 
     ylab = "Tpr", xlab = "Fpr")

abline(a = 0, b= 1, lwd = 1.5)

# Area Under Curve (AUC)
perf <- performance(pred, "auc")
auc <- unlist(perf@y.values)
auc <- round(auc, 4)
auc 

legend(0.7, 0.3, auc, title = "AUC", cex = 1.2)


## Variable importance

varImpPlot(rf.mod1)
varImpPlot(rf.mod1, sort = TRUE, n.var = 10, main = "Top10-Variable Importance")



##Fit logistic model

#lg.mod <- glm(indexMort~ WBC_VALUE+ BUN_VALUE + CO2_VALUE + BILIRUBIN_VALUE + CREATININE_VALUE + INR_VALUE
                
#                 + SODIUM_VALUE + Pulse + SysBP + SPO2, data = Train, family = 'binomial')

#probs2 <- as.vector(predict(lg.mod,newdata=Test_Final, type="response"))


#Model ROC Curve
#roc.curve(Test_Final$indexMort, probs2)
     # AUC 0.77
