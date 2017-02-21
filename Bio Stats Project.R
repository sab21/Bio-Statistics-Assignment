
rm(list = ls())#clear environ ment

#load data set
laheartRaw <- read.table("https://www.umass.edu/statdata/statdata/data/laheart.dat")
head(laheartRaw)
var <- c("ID", "A50", "MD50", "SB50", "DB50", "H50", "W50", "CH50", "SES", "CLS", 
         "MD62", "SB62", "DB62", "CH62", "W62", "IDX", "DYR", "DE")
colnames(laheartRaw) <- var #Providing header
dim(laheartRaw)
lah <- laheartRaw #copy of data


str(lah)
dim(lah)
summary(lah)
any(is.na(lah))
lah <- lah[complete.cases(lah),]#Missing data treatment
any(is.na(lah))
dim(lah)


#Qn no 1 Adding BodyMassIndex
lah$BMI50 <- 703*lah$W50/lah$H50^2
lah$BMI62 <- 703*lah$W62/lah$H50^2


attach(lah)

#Qn 2 Outliers
lahsub <- subset(lah, select = c(A50,H50,BMI50, BMI62, SB50, SB62, CH50, CH62))

library(psych)
describe(lahsub)
summary(lahsub) 
boxplot(lahsub)
boxplot(BMI50,BMI62)
boxplot(SB50,SB62)
boxplot(DB50,DB62)
boxplot(CH50,CH62) 



#Qn 3
lahDead <- subset(lah, DE==1)
dim(lahDead)#64 deaths out of 200
summary(lahDead)
boxplot(lahDead)

lahsub <- subset(lahDead, select = c(A50,H50,BMI50, BMI62, SB50, SB62, CH50, CH62))
attach(lahDead)
boxplot(lahsub)
boxplot(BMI50,BMI62)
boxplot(SB50,SB62)
boxplot(DB50,DB62)
boxplot(CH50,CH62) 
#Outlier in SB50 and SB62 & CH50

lahAlive <- subset(lah, DE==0)
dim(lahAlive) #136 alive

#COmparison between Alive and Dead data
boxplot(lahAlive$SB50, lahAlive$SB62, SB50, SB62)
boxplot(lahAlive$DB50, lahAlive$DB62, DB50, DB62)
boxplot(lahAlive$CH50, lahAlive$CH62, CH50, CH62)
boxplot(lahAlive$BMI50, lahAlive$BMI62, BMI50, BMI62)
boxplot(lahAlive$A50, lahDead$A50)

#Conducting t Test - considering Age 
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$A50,lahAlive$A50) #p value is tending toward 0 - Significant (Rejecting H0)



#Conducting t Test - considering Systolic Blood Pressure - 1950
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$SB50,lahAlive$SB50) #p value is 0.11 -NOT Significant (NOT Rejecting H0)

#Conducting t Test - considering Systolic Blood Pressure - 1962
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$SB62,lahAlive$SB62) #p value is 0.55 -NOT Significant (NOT Rejecting H0)



#Conducting t Test - considering Dys Blood Pressure
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$DB50,lahAlive$DB50) #p value is 0.9 -NOT Significant (NOT Rejecting H0)

#Conducting t Test - considering Dys Blood Pressure
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$DB62,lahAlive$DB62) #p value is .26 -NOT Significant (NOT Rejecting H0)



#Conducting t Test - considering Cholesterol
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$CH50,lahAlive$CH50) #p value is 0.67 -NOT Significant (NOT Rejecting H0)

#Conducting t Test - considering Cholesterol
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$CH62,lahAlive$CH62) #p value is 0.06 -NOT Significant (NOT Rejecting H0)



#Conducting t Test - considering BMI
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$BMI50,lahAlive$BMI50) #p value is 0.45 -NOT Significant (NOT Rejecting H0)

#Conducting t Test - considering BMI in 1962
#Null hypothesis: true difference in means is equal to 0
#alternative hypothesis: true difference in means is not equal to 0
t.test(lahDead$BMI62,lahAlive$BMI62) #p value is 0.01 -Significant (Rejecting H0)


#Significant Cause - Age(Higher) & BMI in 1962(Lower)


#=======================================================
#Q4
attach(lah)
#conducting t.test of BMI from 1950 to 1962
#Null Hypothesys - There is no change in mean
#alternative hypothesis: true difference in means is not equal to 0
t.test(BMI50, BMI62, paired = T) # p-Value - .45 Not Significant (Can't reject H0)

#conducting t.test of Sys Blood Pr from 1950 to 1962
#Null Hypothesys - There is no change in mean
#alternative hypothesis: true difference in means is not equal to 0
t.test(SB50, SB62, paired = T) # p-Value - 0 Significant (reject H0)

#conducting t.test of Dys Blood Pr from 1950 to 1962
#Null Hypothesys - There is no change in mean
#alternative hypothesis: true difference in means is not equal to 0
t.test(DB50, DB62, paired = T) # p-Value - 0.13 Not Significant (Can't reject H0)


#conducting t.test of Cholesterol from 1950 to 1962
#Null Hypothesys - There is no change in mean
#alternative hypothesis: true difference in means is not equal to 0
t.test(CH50, CH62, paired = T) # p-Value - 0.299 Not Significant (Can't reject H0)


#----------------------------------------------------------
#Q5
attach(lah)

model <- lm(SB62~A50+H50+CH62+BMI62)
summary(model)

modA50 <- lm(SB62~A50)
summary(modA50) #P value is 0 (very much significant)
#SB62 = 110.87 + 0.66*A50 


modH50 <- lm(SB62~H50)
summary(modH50) #P value is 0.58 (Not significant)

modCH62 <- lm(SB62~CH62)
summary(modCH62) #P value is 0.382 (Not significant)

modBMI62 <- lm(SB62~BMI62)
summary(modBMI62) #P value is 0.038 (significant)

#Age and BMI are showing significant p-value where Age is most significant

#Age and BMI leads to hypertension


#Q6

#Best Fit
fit1 <- lm(SB62~A50+BMI62)
summary(fit1)
#SB62 = 74.577 + 0.724*A50 + 1.326*BMI62  (fit1 model)
#SB62 = 100 + 0.6*A50 + 0.6*BMI62 (Proposed model) is different


sum(fit1$residuals^2) #106397
plot(fit1$residuals)
plot(fit1)

#Comparing fit1 with modA50
summary(fit1) #R2 = 0.12  - Better Fitted Model
summary(modA50) #R2 = 0.0848
