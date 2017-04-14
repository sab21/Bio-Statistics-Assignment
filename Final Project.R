###################################################################
#        FINAL PROJECT  - Impact On Diabetes
###################################################################



getcsvdata = function(x) read.csv(file=paste("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)

rawData <- getcsvdata("proj2_2017.csv")

str(rawData)
View(rawData)

any(is.na(rawData))#No NA

data <- rawData

data$NAT <- as.factor(rawData$NAT)
data$METRO <- as.factor(rawData$METRO)
data$PLOSS <- as.factor(rawData$PLOSS)
data$County <- as.character(rawData$County)

summary(data)

#NO 1.
#Using Diabetes Rate as the response variable, determine the minimum linear model
#of explanatory variables (multiple regression) using only the non-categorical variables.
#Before doing so, please explore the data to make sure that the data are not anomalous. 
#Examine the residuals of the linear model. Give the final model and give an interpretation 
#of the important variables.

datnum <- data[,c(2:4,6:8)] #Creating data of only non-categorical variables
summary(datnum)

plot(datnum)
cor(datnum) 
#corr of diab rate with - 
#p65 : 0.72
#p18 : -0.445
#INC: -0.62

par(mfrow=c(2,3))
nam <- colnames(datnum)
for(i in 1:6){
  boxplot(datnum[,i], main=nam[i])
}
par(mfrow=c(1,1))

par(mfrow=c(2,3))
nam <- colnames(datnum)
for(i in 1:6){
  hist(datnum[,i], main=nam[i], col="grey")
}
par(mfrow=c(1,1))

#Not any anomaly found from boxplot and histogram


#Multiregression Analysis

#Assumption:
#Independence - checked by randomness
#Normality: From hist plot data looks near normal
#Linearity: There is enough data sample 


attach(datnum)
mod1 <- lm(PDIAB~., datnum)#RECFAC p value is not stastically significant hence it is cannot be related
summary(mod1)#Adj R2 0.6541
anova(mod1)#RECFAC p value is insignificant and P18 as well but we well keep P18 and delete RECFAc


#fit model without RECFAC
mod2 <- lm(PDIAB~POBAD+P65+P18+INC, datnum)
summary(mod2)#all prdictors p value are significant stastically
#Adjusted R-sqr improved 0.6586 with decrease in variable
anova(mod2)#p value of P18 is insignificant


par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

#Multicollinearity
cor(P65,P18)#-0.766 High corr between two
#It is obvious to have high negative correlation between P65 & P18 and P18 should be ignored to avoid multicollinearity
cor(P65, INC)#-0.57 Moderate neagtive corr
#we can also exclude INC for sake of multicollinearity 
#lets check adj R2 in below models

mod3 <- lm(PDIAB~POBAD+P65+INC,datnum)
summary(mod3)#Adjusted R`sqr decreases to 0.6431 from 0.6586
#there is contribution of P18 in mod2, still we can ignore it for better parsimonious model
anova(mod3)


library(car)
vif(mod5)
?vif

mod4 <- lm(PDIAB~POBAD+P65+P18,datnum)
summary(mod4)#Adjusted R sqr further decreased 0.6308 ##mod3 is better than mod4


mod5 <- lm(PDIAB~POBAD+P65,datnum)
summary(mod5)#Adj R-sqr = 0.6269
anova(mod5) 
#This model is most parismonous model since we have only two variables and R2 is also good but
#adj r2 is lower than mod3 where INC has some contribution

mod5A <-lm(PDIAB~P65,datnum)
summary(mod5A)#Adjusted R sqr = 0.5143 - 
anova(mod5A) #Between mod5 and mod5A, mod5 is having greater adj R2

mod6 <-lm(PDIAB~POBAD+INC+P18,datnum)
summary(mod6)#Adjusted R sqr further decreased to 0.4351 - Model Rejected
anova(mod6)

mod7 <-lm(PDIAB~POBAD+INC,datnum)
summary(mod7)#Adjusted R sqr further decreased to 0.3968 - Model Rejected
anova(mod7)




#Hence we can conclude mod2 is the best fitted model as it has highest adjusted R-square Value

fit <- mod2
summary(fit)
fit$coefficients
plot(PDIAB,fit$fitted.values)


#Check if there is any linear relation between  predictor  and residual
res <- fit$residuals
par(mfrow=c(2,2))
plot(res~POBAD);abline(h=0)
plot(res~P65);abline(h=0)
plot(res~P18);abline(h=0)
plot(res~INC);abline(h=0)
par(mfrow=c(1,1))


#Check for normality
hist(res, col = "grey")

qqnorm(res)
qqline(res, col="red", lwd = 2)
#Residual can be considered near normal with some outliers

#Constant variability of residuals for high and low value of predictors
plot(fit$residuals~fit$fitted.values)#constant variability condition can be met
plot(abs(fit$residuals)~fit$fitted.values)

#Independent residuals  - check for time series data
plot(res)#  data are scteered - Assumption met


#
#Final model
summary(fit)
#R sqr = 0.6752 i.e 67.52 % of variability can be explained by the model mod2

#Predicted Diabetes Rate = 2.080 + 0.1228xPOBAD + 0.2582xP65 + 0.1279xP18 - 0.000054xINC + e
#where e = std error


#Interpreting slope of POBAD - obesity rate
#All else held constant , for each 1 unit increase in obesity rate the model predicts the diabetes 
#rate to be increased by 0.1228 unit on an average.

#Likewise all else slope can be interpreted

#interpreting intercept:
#When all predictors are zero then diabetes rate is considered to be 2.080 - a meaningless statement




#Qn 2. Ignoring the other explanatory variables, determine if Diabetes Rate varies by
#metropolitan classification (METRO)

boxplot(data$PDIAB~data$METRO)#mean of diabetes rate is less in metropolitian counties 

mod <- lm(PDIAB~METRO, data)
summary(mod) #P-value is stastically significant hence we can conclude that 
              #diabetes rate is related with Metropolitian nature of counties
#Model: Predicted diabetes rate = 11.46 - 1.24xMETRO:1,  Ignoring all other variables
#Interpretation:  
#For Non-Metro Counties
#Predicted diabetes rate = 11.46

#For Metro Counties
#Predicted diabetes rate = 11.46 - 1.24 = 10.22

#Diabetes rate of Metropolitian Counties is lower than Non-Metropolitian Counties
#by 1.24 on an average ignoring all other variables


#Qn No 3
#Conduct a one-way ANOVA examining Diabetes Rate by METRO as the factor.
#Compare to part 2.

#One Way ANOVA

# Get some stats on the groups
size <- tapply(data$PDIAB, data$METRO, length)
m <- tapply(data$PDIAB, data$METRO, mean)
# Check the ranges of sample standard deviations
sd <- tapply(data$PDIAB, data$METRO, sd)

tab <- rbind(size=size, mean = m, sd=sd);tab

#Ho: The average diabetes rate is same across both Metro and Non Metro Counties
#HA: mu1 not equal to mu2

#Condition for ANOVA
#1.Independece:
  #Within group: sampled observed are independent 
  #Between Group: groups are non-paired
#2.Approx Normality
hist(subset(data$PDIAB,data$METRO==0), col="grey", main = "Non Metro")#Normal
hist(subset(data$PDIAB,data$METRO==1), col="grey", main = "Non Metro")#Near Normal
#3. Equal variance 
sd(subset(data$PDIAB,data$METRO==0))#1.49
sd(subset(data$PDIAB,data$METRO==1))#1.36

#Anova

metro.aov <- aov(PDIAB~ METRO, data)
summary(metro.aov)
#p value is stastically significant so we can reject HO
#Mean diabetes rate changes with change in category of Metro

anova(mod)#anova table of linear model between PDIAB response variable and METRO predictor

#Both mod and metro.aov are identical 

#Qn 4
#Create two new categorical variables to describe each county, HighINC:
 # HighINC = 1 for INC > 40,000; 0 for INC ??? 40,000
#and the second is DegOB:
  #DegOB = 0 for POBAD ??? 29; 1 for 29 < POBAD ??? 33; 2 for 33 < POBAD
#Use a one-way ANOVA to examine Diabetes Rate using DegOB as a factor and then
#carry out a posthoc analysis comparing individual pairs accounting for multiple comparisons

data$HighINC = as.factor(1*(data$INC>40000))#High Income Category

data$DegOB <- as.factor(1*(data$POBAD>29)+1*(data$POBAD>33))#Obesity rate category

summary(data)

#Anova - one way  - PDAIB wrt DegOB

#One Way ANOVA

# Get some stats on the groups
size <- tapply(data$PDIAB, data$DegOB, length)
m <- tapply(data$PDIAB, data$DegOB, mean)
# Check the ranges of sample standard deviations
sd <- tapply(data$PDIAB, data$DegOB, sd)

tab <- rbind(size=size, mean = m, sd=sd);tab

#Ho: The average diabetes rate is same across all category in obesity rate
#HA: either of mean is not equal

#Condition for ANOVA
#1.Independece:
#Within group: sampled observed are independent 
#Between Group: groups are non-paired
#2.Approx Normality
hist(subset(data$PDIAB,data$DegOB==0), col="grey", main = "<29")#Normal with thick tail 
hist(subset(data$PDIAB,data$DegOB==1), col="grey", main = "29-33")#Near Normal
hist(subset(data$PDIAB,data$DegOB==2), col="grey", main = ">33")#Normal
#3. Equal variance 
#nearly equal from table

#Anova

ob.aov <- aov(PDIAB~ DegOB, data)
summary(ob.aov)
#p value is stastically significant so we can reject HO
#Mean diabetes rate is different in atleat each category of category of DegOB

boxplot(data$PDIAB~data$DegOB)

#Pairwise t test
pairwise.t.test(data$PDIAB,data$DegOB,p.adj = "bonf")
#P-value is significant between 0:1 and 0:2

TukeyHSD(ob.aov)
#P-value is significant between 0:1 and 0:2

#So it can be concluded that mean of daibetes rate of people whose obesiy rate is below 29 is 
#significantly different (lower) from people with above 29 obese rate


#Qn 5
#You are curious if metropolitan areas (METRO=1) have a different relationship between
#obesity and diabetes than non-metropolitan areas. Conduct an analysis to examine if
#there is an interaction between degree of obesity in county data and METRO. Use
#graphical and numerical methods to support your conclusion


#data visualisation
par(mfrow=c(1,2))
boxplot(data$PDIAB~data$METRO)#diabetes rate decreases in Metro counties
boxplot(data$POBAD~data$METRO)#Obesity rate remain same or slight increase in mean in metro
par(mfrow=c(1,1))
#This is interesting as diabetes rate increases in Non Metro counties where obesity rate doesn't and 
#in earlier analysis we found that higherdegree of obesity yields higher diabetes rate

table(data$DegOB,data$METRO)

def.par <- par(no.readonly = TRUE)
nf <- layout(matrix(c(1,2,3,3), 2,2,byrow = T))
layout.show(nf)
boxplot(data$PDIAB~data$METRO)#diabetes rate decreases in Metro counties
boxplot(data$PDIAB~data$DegOB)#DCiabetes rate increase with higher degree of Obesity
boxplot(data$PDIAB~data$DegOB*data$METRO)
par(def.par)


boxplot(data$PDIAB~data$DegOB*data$METRO)
boxplot(data$PDIAB~data$METRO*data$DegOB)
#For Non Metro counties lower degree of Obesity have higher obesity rate
#whereas highest degree of obesity have somewhat similar diabetes rate irrespective of 
#Metro status of counties

#Numerical 
size <- with(data,aggregate(PDIAB,list(Metro = METRO,Obesity=DegOB),length));size
mean <- with(data,aggregate(PDIAB,list(Metro = METRO,Obesity=DegOB),mean));mean
sd <- with(data,aggregate(PDIAB,list(Metro = METRO,Obesity=DegOB),sd));sd
tab <- cbind(size,mean = mean[,3],sd = sd[,3]);tab

interaction.plot(data$DegOB,data$METRO,data$PDIAB, lwd = 2, col = "red", ylab = "Diabetes Rate (Mean)")

#Two Way ANOVA
d.aov <- aov(data$PDIAB~data$DegOB*data$METRO)
summary(d.aov)
#P Value of both Degreeof obesity and Metro is coming stastically significant as 
#in case of One way anova of each

#P value of their interaction is not coming significant that means there is 
#no significant interarction between two groups


#More with Plots
attach(data)
with(data,interaction.plot(DegOB,METRO,PDIAB, lwd = 4, col = "red",
                           ylim = c(8,12),ylab = "Diabetes Rate (Mean)"))
points(DegOB,aov(PDIAB~DegOB*METRO)$fit, pch=19, col=1, cex=1.5)  # The full two-way ANOVA model
points(DegOB,aov(PDIAB~DegOB+METRO)$fit, pch=19, col=2, cex=1.5)  # No interaction term
points(DegOB,aov(PDIAB~DegOB)$fit, pch=19, col=3, cex=1.5)    # One-way ANOVA using DegOB
points(DegOB,aov(PDIAB~METRO)$fit, pch=19, col=4, cex=1.5)   # One-way ANOVA using METRO


# Also look at the residuals
par(mfrow=c(3,2))
plot(DegOB,aov(PDIAB~DegOB*METRO)$res, col=2, lwd=1, pch=19,
     main="Residuals for Full Model with Interactions")    # Residuals for full two-way model - gender
plot(METRO,aov(PDIAB~DegOB*METRO)$res, col=2, lwd=1, pch=19,
     main="Residuals for Full Model with Interactions")     # Residuals for full two-way model - group
plot(DegOB,aov(PDIAB~DegOB+METRO)$res, col=2, lwd=1, pch=19,
     main="Residuals for Full Model without Interactions")    # No interaction term - gender
plot(METRO,aov(PDIAB~DegOB+METRO)$res, col=2, lwd=1, pch=19,
     main="Residuals for Full Model without Interactions")     # No interaction term - group
plot(DegOB,aov(PDIAB~DegOB)$res, col=2, lwd=1, pch=19,
     main="Residuals for One-Way ANOVA on DegOB")            # One-way ANOVA on group
plot(METRO,aov(PDIAB~METRO)$res, col=2, lwd=1, pch=19,
     main="Residuals for One-Way ANOVA on METRO")          # One-way ANOVA on group
par(mfrow=c(1,1))
#I don't know how to interpret above graphs - 


#Qn 6
#We will now examine other relationships between variables. Determine if there
#is a relationship between counties that lost population and whether they were
#metropolitan. The table() function might be useful here

attach(data)
table(PLOSS,METRO)

library(gmodels)
CrossTable(PLOSS,METRO)
#7% of Non Metro Lost population whereas 11.5% of Metro counties lost population during
# the period
plot(PLOSS,METRO,xlab="Population Loss", ylab = "Metro", col = c(2,4))
chisq.test(PLOSS,METRO)
#P-Value is not stastically significant
#So we cannot conclude that there is any significant relation between any 
#PLOSS and Metro 


#Qn 7
#Often times, rural areas have higher income levels. Determine if there is a 
#relationship between income and whether they were metropolitan. Do this two
#different ways: using INC and HighINC. Discuss whether one is preferable to the other.

#with HighINC
table(HighINC,METRO)
CrossTable(HighINC,METRO)
#31.6% of Non Metro counties comes under high income category
#whereas 84.6% of Metro counties comes under high income category
plot(HighINC,METRO, xlab="High Income", ylab = "Metro", col = c(2,4))
chisq.test(HighINC,METRO)
#P value is significant - reject Null Hypotheses
#We can conclude that there is significant relation between HignINC and Metro


#With INC
boxplot(INC~METRO)


# Get some stats on the groups
size <- tapply(data$INC, data$METRO, length)
m <- tapply(data$INC, data$METRO, mean)
# Check the ranges of sample standard deviations
sd <- tapply(data$INC, data$METRO, sd)

tab <- rbind(size=size, mean = m, sd=sd);tab

#Anova

inc.aov <- aov(INC~ METRO, data)
summary(inc.aov)
#p value is stastically significant so we can reject HO
#We can conclude that there is relation between InC and Metro

fit <- lm(INC~METRO,data)
summary(fit)
#logistic regression p value is significant
