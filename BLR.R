"#Bank loan data
1 : < 28 years 
2: 28 to 45
3 : >45"

#removing unnecessary variables
Bank_Loan$SN<-NULL

#data conversion
Bank_Loan$AGE<-as.factor(Bank_Loan$AGE)
Bank_Loan$DEFAULTER<-as.factor(Bank_Loan$DEFAULTER)

str(Bank_Loan)

#Generating binary model
Defaulter_model<-glm(Bank_Loan$DEFAULTER~., data = Bank_Loan, family = "binomial")
Defaulter_model

defaulter_model_rev<-glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,data = Bank_Loan,family = "binomial")
summary(defaulter_model_rev)

Bank_Loan$pred_def<-predict(defaulter_model_rev,Bank_Loan, type="response")
head(Bank_Loan)
# using type=response takes into acc the log, otherwise it will just multiply with the coeffs like in lm

#keep threshold as 0.5 and create column predicted defaulter = 1 if defaulter>0.5, 0 if defaulter<0.5

Bank_Loan$pred_defaulter<-ifelse(Bank_Loan$pred_def>0.5, "1","0")
head(Bank_Loan)


table(actual=Bank_Loan$DEFAULTER, predicted=Bank_Loan$pred_defaulter)

#Confusion matrix
#P=1, N=0
        #predicted
#actual   0   1
    #0 478  39
    #1  91  92

#0 Non defaulter, 1 is defaulter
#478; actual 0 predicted 0
#39; actual 0 predicted 1
#91; actual 1 predicted 0
#92; actual 1 predicted 1

#accuracy= (478+92)/ (478+92+92+39)

# accuracy = (correct1 +correct 0 )/total observatios = 
acc<-(478+92)/700;acc
#misclassification = wrongly identified 0 + wrongly identified 1 /total data 
misclass<- 1-acc;misclass


#sensitivity, specificity, FPR, FNR, prevalence and precision

#sensitivity
TP<-92
TN<-478
FP<-39
FN<-91

sensitivity<-TP/(TP+FN); sensitivity
specificity<-TN/(TN+FP); specificity
FPR<-1-specificity; FPR
FNR<-1-sensitivity;FNR
precision<-TP/(TP+FP); precision
prevalance<-TP/700; prevalance


#Method 2
install.packages("ROCR")
library(ROCR)

pred<-prediction(Bank_Loan$pred_def,Bank_Loan$DEFAULTER)
perf<-performance(pred,"tpr","tnr")
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,1,0.1))

Bank_Loan$pred_defaulter1<-ifelse(Bank_Loan$pred_def>0.3,"1","0")
table(actual=Bank_Loan$DEFAULTER, predicted=Bank_Loan$pred_defaulter1)
acc= (415+138)/700;acc
sen= (138)/(45+138);sen
spe= (415)/(415+102);spe

# value of acc, sen and spe > 0.60 / 60%, model is a good model

library(car)
vif(defaulter_model_rev)
#all vif<5 ; model is free from multicollinearity
 