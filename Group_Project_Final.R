getwd()
setwd("C:/Users/Moncef/Documents/R")
getwd()
DataSet <- read.csv("~/R/Work_GroupProject_FinalData.csv")


library(caTools)
library(ROCR)
#library(dplyr)
library(Hmisc)
library(caTools)
library(ROCR)
library(dplyr)
library(Hmisc)

summary(DataSet)
DataSet$IsDenied <- factor(DataSet$IsDenied)
DataSet$IsFatality <- factor(DataSet$IsFatality)
DataSet$Gender <- factor(DataSet$Gender)
DataSet$IsSubrogation <- factor(DataSet$IsSubrogation)
DataSet$IsLitigated <- factor(DataSet$IsLitigated)


summary(DataSet)

DataSet$TotalReserves<-NULL
DataSet$IsSubrogation <-NULL
DataSet$ClaimStatus <-NULL

DataSetINGrp <- DataSet

combine.InjuryNature <- function(x){
  if (is.na(x)){
    return(NA)
  }else if(x == "Strain"){
    return("Strain")
  }else if(x == "Non-Standard Code"){
    return("Non-Standard Code")
  }else if(x == "Contusion"){
    return("Contusion")
  }else{
    return("Other")
  }
}

DataSetINGrp$InjuryNature <- sapply(DataSetINGrp$InjuryNature,combine.InjuryNature)

DataSetINGrp$InjuryNature <- factor(DataSetINGrp$InjuryNature)


DataSetIsLitigated <- subset(DataSetINGrp, DataSetINGrp$IsLitigated == "1")
DataSetIsNotLitigated <- subset(DataSetINGrp, DataSetINGrp$IsLitigated == "0")



set.seed(101)

df<-DataSetIsNotLitigated
sample <- sample.split(df$IsLitigated, SplitRatio = 0.01)

df.train.IsNotLitigated = subset(df, sample == TRUE)

# Validation Data
df.valid.IsNotLitigated = subset(df, sample == FALSE)

df2<-DataSetIsLitigated
sample <- sample.split(df2$IsLitigated, SplitRatio = 0.5)

df.train.IsLitigated = subset(df2, sample == TRUE)

# Validation Data
df.valid.IsLitigated = subset(df2, sample == FALSE)

df.train.Combined = rbind(df.train.IsNotLitigated, df.train.IsLitigated)
df.valid.Combined = rbind(df.valid.IsNotLitigated, df.valid.IsLitigated)

baseline.model <- glm(formula=IsLitigated ~ IsFatality + ClaimantAge_at_DOI_Computed + Gender + ClaimantType + InjuryNature + 
                        #BodyPartRegion + 
                        #BodyPart + 
                        #ServiceDuration + 
                        #SUM_of_PaymentAmount + 
                        DaysToFile  , family = binomial(link='logit'),data = df.train.Combined)

summary(baseline.model)

pred.probabilities <- predict(baseline.model,newdata=df.valid.Combined,type='response')


#Turning the default probabilities to binary
pred.results <- ifelse(pred.probabilities > 0.5,1,0)

#the confusion matrix
print("The confusion matrix is:")
print(table(pred.results, df.valid.Combined$IsLitigated))


#the ROC curve
library(ROCR)
pred <- prediction(pred.probabilities, df.valid.Combined$IsLitigated)
pred <- performance(pred, "tpr", "fpr")
plot(pred)
abline(a=0,b=1)


# Calculating the AUC value
pred <- prediction(pred.probabilities, df.valid.Combined$IsLitigated)
auc <- performance(pred, measure="auc")
auc <- auc@y.values[[1]]
print(paste("AUC for the baseline regression model is:", auc))
