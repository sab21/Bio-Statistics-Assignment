#
#ANOVA



# Get data set
fbdata = getdata("friends.txt") 
head(fbdata)
# We need to manipulate the data slightly since we can't have groups with numerical names

fb = data.frame(score=fbdata$Score, friends=sprintf("f%d",fbdata$Friends))
head(fb)
# Plot histograms of each data set
par(mfrow=c(2,3))
hist(subset(fb$score, fb$friends=="f102"), col="royalblue", freq=F, main="f102", xlab="Score", xlim=c(1,7), breaks=seq(1,7,0.5))
hist(subset(fb$score, fb$friends=="f302"), col="royalblue", freq=F, main="f302", xlab="Score", xlim=c(1,7), breaks=seq(1,7,0.5))
hist(subset(fb$score, fb$friends=="f502"), col="royalblue", freq=F, main="f502", xlab="Score", xlim=c(1,7), breaks=seq(1,7,0.5))
hist(subset(fb$score, fb$friends=="f702"), col="royalblue", freq=F, main="f702", xlab="Score", xlim=c(1,7), breaks=seq(1,7,0.5))
hist(subset(fb$score, fb$friends=="f902"), col="royalblue", freq=F, main="f902", xlab="Score", xlim=c(1,7), breaks=seq(1,7,0.5))
par(mfrow=c(1,1))

# Neat short-cut to produce the plots
par(mfrow=c(2,3))
for (i in unique(fb$friends)) hist(subset(fb$score,fb$friends==i), col="firebrick3", freq=F, main=i, breaks=seq(1,7,0.5), xlim=c(1,7), xlab=i)
par(mfrow=c(1,1))

# Plot the data to look at the trends
boxplot(score ~ friends, data=fb, col=2:6, cex.axis=1.5, pch=19)

# Get some stats on the groups
tapply(fb$score, fb$friends, length)
tapply(fb$score, fb$friends, mean)

# Check the ranges of sample standard deviations
tapply(fb$score, fb$friends, sd)

#Calculate the pooled SD
sampsd = aggregate(fb$score, list(fb$friends), sd)$x
n = aggregate(fb$score, list(fb$friends), length)$x
sum((n-1)*sampsd^2)/sum(n-1)         # pooled variance estimate
sqrt(sum((n-1)*sampsd^2)/sum(n-1))   # pooled standard deviation estimate

# Get the ANOVA Table
fb.aov = aov(score ~ friends, data=fb)
anova(fb.aov)
summary(fb.aov)
# Good time to illustrate mistakes that arise from names that begin with numbers

# The original data had the friends factor as only a number.  Try to do ANOVA using that form.
anova(aov(Score ~ Friends, data=fbdata))

# The numbers are not interpreted as factors (compare to the ANOVA above using our modifed names).  
# To fix this, we could also just force the term to be considered as a factor

anova(aov(Score ~ as.factor(Friends), data=fbdata))


# Alertness vs. Drug Dosage Example

ad = getdata("alert-drug.txt")

ad   # look at data


boxplot(Alertness~Dosage, data=ad, col=2:4, cex.axis=2, pch=19)

tapply(ad$Alertness, ad$Dosage, sd)   # doesn't pass our test

ad$lAlertness = log(ad$Alertness)  # log transform the data
boxplot(lAlertness~Dosage, data=ad, col=2:4, cex.axis=2, pch=19)

tapply(ad$lAlertness, ad$Dosage, sd)   # very close to factor of 2 now

ad.aov = aov(lAlertness ~ Dosage, data = ad)   # one-way ANOVA model

summary(ad.aov)


# QC example

s.dat = getdata("screw.dat")
s.dat
# multiple factors to look at ...
anova(aov(diameter ~ as.factor(machine), data=s.dat))

# R Code for Lecture 21 - Comparing Means in One-Way ANOVA
# Version 5: March 28, 2017
getcsvdata = function(x) read.csv(file=paste("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)

# Get data set and reformat as last lecture
fbdata = getdata("friends.txt")
fb = data.frame(score=fbdata$Score, friends=sprintf("f%d",fbdata$Friends))
boxplot(score ~ friends, data=fb, col=2:6, cex.axis=1.5, pch=19)

# Get the ANOVA Table
fb.aov = aov(score ~ friends, data=fb)
anova(fb.aov)
library(reshape2)
melt(fb)
# a little side comment - ANOVA is the same as mulitple regression
# make 
is.f3 = as.numeric(fb$friends == "f302")
is.f5 = as.numeric(fb$friends == "f502")
is.f7 = as.numeric(fb$friends == "f702")
is.f9 = as.numeric(fb$friends == "f902")
fb.lm = lm(fb$score ~ is.f3 + is.f5 + is.f7 + is.f9)
summary(fb.lm)

aggregate(fb$score, list(fb$friends), mean)
coefficients(fb.lm)[1]
coefficients(fb.lm)[1] + coefficients(fb.lm)[2:5]


# Set out two contrasts to test
# Note that our two contrast are orthogonal, so we can test them at the same time
contrasts(fb$friends) = cbind(c(-1,0,2,0,-1),c(-2,-1,0,1,2))

# Check they are there - N.B. the other vectors in the contrast matrix
# are automatically generated to make the matrix orthonormal
contrasts(fb$friends)
?contrasts
# Do our ANOVA analysis, but this time using lm
fb.con.lm = lm(score ~ friends, data=fb)

# Check the results for our two-sided test.  
# Note we need to mult P by 2 for the 1-sided test
summary(fb.con.lm)

# Using the built-in contrast functions in R we can do other tests

# First look differences from the intercept (average treatment effect)
contrasts(fb$friends) = contr.sum
contrasts(fb$friends)
fb.con.lm = lm(score ~ friends, data=fb)
summary(fb.con.lm)

# R can also check for polynomial trends: linear, quadratic, etc.
contrasts(fb$friends) = contr.poly
contrasts(fb$friends)
fb.con.lm = lm(score ~ friends, data=fb)
summary(fb.con.lm)



# Multiple-Comparisons procedure using Bonferfoni correction
pairwise.t.test(fb$score, fb$friends, p.adj="bonferroni")

# Confidence Intervals using Tukey's "Honest Significant Difference" method
TukeyHSD(fb.aov)

# also plot method for Tukey test

# Kudzu example

kudzu = getdata("kudzu.txt")
kudzu
tapply(kudzu$BMD, kudzu$Treatment, mean)
tapply(kudzu$BMD, kudzu$Treatment, sd)
tapply(kudzu$BMD, kudzu$Treatment, length)

hist(subset(kudzu$BMD,kudzu$Group==1), col=2)
hist(subset(kudzu$BMD,kudzu$Group==2), col=2)
hist(subset(kudzu$BMD,kudzu$Group==3), col=2)

boxplot(kudzu$BMD ~ kudzu$Treatment, col=2:4)  # nicer than the histograms

kudzu.aov = aov(BMD ~ Treatment, data=kudzu)
summary(kudzu.aov)
# or
anova(kudzu.aov)
pairwise.t.test(kudzu$BMD,kudzu$Treatment, p.adj="bonf")
# what about subject control from average of treatment groups?
con = c(-2,1,1)
contrasts(kudzu$Treatment) = con
contrasts(kudzu$Treatment)
summary(lm(BMD ~ Treatment, data=kudzu))

# note coeff ->
mns = tapply(kudzu$BMD, kudzu$Treatment, mean)
sum(mns*con)/sum(con^2)

# What I do next is improper, because I am determining the contrast
# of interest after looking at the data
con = c(-1,2,-1)
contrasts(kudzu$Treatment) = con
contrasts(kudzu$Treatment)
summary(lm(BMD ~ Treatment, data=kudzu))

# Example - Combining Group and Linear Data
ht = getcsvdata("heights_parents.csv")
ht = getdata("heights_parents.txt")
attach(ht)
ht.lm = lm(Offspring ~ Parents)
summary(ht.lm)
ht$is.F = as.numeric(ht$Gender == "F")
ht.lm2 = lm(Offspring ~ Parents + is.F,data=ht)
summary(ht.lm2)

# R Code for Lecture 22 - Two-Way ANOVA
# Version 5: March 30, 2017

# one way ANOVA
# Example - Combining Group and Linear Data
ht = getdata("heights_parents.txt")
attach(ht)
ht.lm = lm(Offspring ~ Parents)
summary(ht.lm)
ht$is.F = as.numeric(ht$Gender == "F")
ht.lm2 = lm(Offspring ~ Parents + is.F,data=ht)
summary(ht.lm2)


# now 2 way ANOVA 
ssb=c(231,125,68,289,160,82)
age = factor(rep(c("20-44","45-64","65-older"),2))
year=factor(c(rep(1994,3),rep(2004,3)))
data.frame(ssb,age,year)
tapply(ssb,age, mean)
tapply(ssb,year, mean)

interaction.plot(age,year,ssb, lty=1, col=c(2,3), lwd=3, cex.axis=1.4, cex.lab=1.4)


# 2x2 ANOVA Example 
heart = getdata("heartrate.txt")
attach(heart)

aggregate(hr,list(gender,group), mean)
aggregate(hr,list(gender,group), sd)

# also check boxplots and QQplots
boxplot(hr ~gender)
boxplot(hr ~ group)
boxplot(hr ~ group*gender)
qqnorm(subset(hr,group=="Runners"))



# Perform two-way ANOVA
heart.aov = aov(hr ~ group*gender)
anova(heart.aov)

interaction.plot(group,gender, hr, lwd=4, col=2:3, lty=1, cex.axis=1.4, cex.lab=1.6, ylab="Heart Rate (bpm)",xlab=NA)

library(ggplot2)  # again slightly nicer plots with ggplot
ggplot(heart, aes(x=group, y=hr, col=gender)) + labs(x="Group", y="Heart Rate") + geom_boxplot() + stat_summary(aes(group=gender), fun.y=mean, geom="point", shape=21, size=3, fill="gray") + stat_summary(aes(group=gender), fun.y=mean, geom="line", size=1.5) + theme_bw()


# Go back to interaction plot and look at the fits
interaction.plot(group,gender, hr, lwd=4, col=2:3, lty=1, cex.axis=1.4, cex.lab=1.6, ylab="Heart Rate (bpm)",xlab=NA)
points(group,aov(hr~group*gender)$fit, pch=19, col=1, cex=1.5)  # The full two-way ANOVA model
points(group,aov(hr~group+gender)$fit, pch=19, col=2, cex=1.5)  # No interaction term
points(group,aov(hr~group)$fit, pch=19, col=3, cex=1.5)    # One-way ANOVA using group
points(group,aov(hr~gender)$fit, pch=19, col=4, cex=1.5)   # One-way ANOVA using gender

# Also look at the residuals
par(mfrow=c(3,2))
plot(gender,aov(hr~group*gender)$res, col=2, lwd=1, pch=19, ylim=c(-60,60),
     main="Residuals for Full Model with Interactions")    # Residuals for full two-way model - gender
plot(group,aov(hr~group*gender)$res, col=2, lwd=1, pch=19, ylim=c(-60,60),
     main="Residuals for Full Model with Interactions")     # Residuals for full two-way model - group
plot(gender,aov(hr~group+gender)$res, col=2, lwd=1, pch=19, ylim=c(-60,60),
     main="Residuals for Full Model without Interactions")    # No interaction term - gender
plot(group,aov(hr~group+gender)$res, col=2, lwd=1, pch=19, ylim=c(-60,60),
     main="Residuals for Full Model without Interactions")     # No interaction term - group
plot(group,aov(hr~group)$res, col=2, lwd=1, pch=19, ylim=c(-60,60),
     main="Residuals for One-Way ANOVA on Group")            # One-way ANOVA on group
plot(gender,aov(hr~gender)$res, col=2, lwd=1, pch=19, ylim=c(-60,60),
     main="Residuals for One-Way ANOVA on Gender")          # One-way ANOVA on group
par(mfrow=c(1,1))


# Final Example
drilldat = getdata("drilling.txt")

# Note that both factors (time and tool) are numerical, so we need to treat this properly
drill.aov = aov(diam ~ factor(tool)*factor(time), data=drilldat)

anova(drill.aov)

# the "with" function below allows us to use drilldat without attaching it
with(drilldat, interaction.plot(tool,time,diam, lwd=3, col=2:4))
with(drilldat, interaction.plot(time,tool,diam, lwd=3, col=2:6))   # change the x-axis




########################################################
#  Assignment No 10
########################################################



#Qn No 1 - Deadpoets
getdata = function(x) read.table(file=paste ("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)
dp = getdata("deadpoets.txt")
dp$Type1 <- as.factor(dp$Type1)
head(dp)
str(dp)
View(dp)
attach(dp)

#a.
boxplot(Age~Type, col=2:4)

#histogram
par(mfrow=c(1,3))
for (i in unique(dp$Type)) hist(subset(dp$Age,dp$Type==i), col="blue", freq=F, main=i, breaks=5, xlim=c(30,100), xlab=i)
par(mfrow=c(1,1))

#qqplot
par(mfrow=c(1,3))
for (i in unique(dp$Type)) {qqnorm(subset(dp$Age,dp$Type==i),main=i);qqline(subset(dp$Age,dp$Type==i),lwd=2,col="red")}
par(mfrow=c(1,1))

#table & barplot
tt <- with(dp, table(cut(Age,quantile(Age)),Type));tt


prop.table(tt,2)

barplot(prop.table(tt), col = 2:5)


#Numerical data
n <- tapply(dp$Age, dp$Type, length)
mean <- tapply(dp$Age, dp$Type, mean)
sd <- tapply(dp$Age, dp$Type, sd)

summary1 <- data.frame(n,mean,sd);summary1


#Assumption:
#1a. Independence within group - random, each group should be less than 10% of population
#1b. Independence between group - 
#2: Approxmately Normal - evident from qqplot that data are appox normal
#3. Constant Variance across the groups/Homoscedastic Group - can be checked from boxplot and summary table that sd is approx consistent across group - 

 
#b.
#HO: mean(Poets)=mean(novels)=mean(nonfiction)
#HA: Atleat one pair mean is different

dp.anova <- aov(Age~Type,data=dp)
summary(dp.anova)#pvalue is significant - 99 %
dp.anova
#From p value we can conclude that one pair is significantly different from another but which pair is diffrent can't be concluded from ANOVA F test

#c. - No idea about contrasts 
contrasts(dp$Type) <- contr.sum
contrasts(dp$Type)

#d.
#Multiple comparison t-test 

#bonferroni correction
#alpha* = alpha/k* where k*=k(k-1)/2 where k = no of comparison and alpha = .05
# For k = 3, k* = 3 and alpha8 = .05/3 = 0.0167

summary(dp.anova)
mse <- 209.1#Mean Square Error (residual  )

summary1


#Manual Method
#Multiple Comparison 1 - Poets vs Novels
#Ho: Mu(poest)=Mu(Novels)
#Ha: Mu(poest)!=Mu(Novels)

se1 <- sqrt(mse/32+mse/67) #se <- sqrt(mse/n1 + mse/n2)

tstat <- (71.44776-63.18750)/se1 #from summary1
df <- 120 #from anova table

pvalue <- 2*pt(tstat,df, lower.tail = F);pvalue
#pvalue is less than 0.0167 - significant 


#Multiple Comparison 2 - Poets vs Nonfiction
#Ho: Mu(poest)=Mu(Nonfiction)
#Ha: Mu(poest)<Mu(Nonfiction)

se1 <- sqrt(mse/32+mse/24) #se <- sqrt(mse/n1 + mse/n2)

tstat <- (76.875-63.18750)/se1 #from summary1
df <- 120 #from anova table

pvalue <- pt(tstat,df, lower.tail = F);pvalue
#pvalue is less than 0.0167 - significant 


#Multiple Comparison 2 - Novels vs Nonfiction
#Ho: Mu(Novels)=Mu(Nonfiction)
#Ha: Mu(Novels)<Mu(Nonfiction)

se1 <- sqrt(mse/67+mse/24) #se <- sqrt(mse/n1 + mse/n2)

tstat <- (76.875-71.44776)/se1 #from summary1
df <- 120 #from anova table

pvalue <- pt(tstat,df, lower.tail = F);pvalue
#pvalue is greater than 0.0167 - Not significant 



#pairwise test with bonferroni correction
pairwise.t.test(dp$Age, dp$Type, p.adj="bonf")
#Clearly death of poets is significantly different from both novels and non fiction writers

TukeyHSD(dp.anova)#Can be seen from this too


#Qn no 2 - Piano
piano <- getdata("piano.txt")
head(piano)
View(piano)
data <- piano[,2:3]

boxplot(score~lessons,data)#piano lessons seems to be different


#table(data)
mean <- tapply(data$score, data$lessons,mean);mean
n <- tapply(data$score, data$lessons,length);n
sd <- tapply(data$score, data$lessons,sd);sd
se <- sd/sqrt(n);se

tab <- data.frame(n,mean,sd,se)
tab <- rbind(tab, Overall = list(length(data$score),mean(data$score),sd(data$score),sd(data$score)/sqrt(length(data$score))))
tab#summary table

#b.
data.aov <- aov(score~lessons, data)
summary(data.aov)
#F Statsistic - 9.239 - too far from mean
#degree of freedon between group = 3
#degree of freedon within group = 74
#pvalue is significant thus it can be concluded that atleat one pair is significantly different

attach(data)
pairwise.t.test(score,lessons, p.adjust.method = "bonf")
#Clearly Piano is significantly different from other lessons
boxplot(score~lessons, col=2:5)


t <- TukeyHSD(data.aov)
t$lessons

boxplot(t(t$lessons))#piano is different 



#Qn No 3 - toothgrowth
tg <- getdata("toothgrowth.txt")
head(tg)
View(tg)
data <- tg
data$dose <- as.factor(tg$dose)
str(data)
attach(data)
size <- aggregate(len,list(supp,dose),length);size
mean <- aggregate(len,list(supp,dose),mean);mean
sd <- aggregate(len,list(supp,dose),sd);sd
tab <- cbind(size,mean=mean[,3],stddev=sd[,3]);tab

boxplot(len~supp)
boxplot(len~dose)
boxplot(len~supp*dose, col=2:3)

interaction.plot(dose,supp,len,lwd=2,col=2:3)
interaction.plot(supp,dose,len,lwd=2,col=2:4)
# length of tooth increases with increase in dose
# with dose 0.5 and 1 length increases significantly when supp is changed from vit C to orange juice 
# but for higher dose the change in supp is irrevelant 

#One Way Anova
aov1 <- aov(len~dose);anova(aov1)#significant
aov2 <- aov(len~supp);anova(aov2)#Not significant with 95% significance level

#Two Way Anova
aov3 <- aov(len~dose+supp);anova(aov3)#with out interarction - both are significant
aov4 <- aov(len~dose*supp);anova(aov4)#with intearction - all are significant

#Assumption:
#1a. Independence within group - random, each group should be less than 10% of population
#1b. Independence between group - Orange Juice also contain Vitamin C 
#2: Approxmately Normal - 
#3. Constant Variance across the groups/Homoscedastic Group - can be checked from boxplot and summary table that sd is approx consistent across group - 




#Qn No 4 - iron content
iron1 <- getdata("ironcontent.txt")
View(iron1) 
head(iron1)
data <- iron1[,c(2,4,7)]
attach(data)

size <- with(data, aggregate(iron,list(type,food),length));size
mean <- aggregate(data$iron,list(data$type,data$food),mean);mean
sd <- with(data, aggregate(iron,list(type,food),sd));sd

tab <- cbind(size,mean= mean[,3],sd=sd[,3])
tab#table

head(data)
with(data,boxplot(iron~type))
with(data,boxplot(iron~food))
with(data,boxplot(iron~type*food, col=2:4))


#ANOVA

iron.aov <- aov(data$iron~type*food)
anova(iron.aov)

interaction.plot(type,food,data$iron, col = 2:4,lwd=2, ylab="iron content")
#iron content in each food type increases when cooked in iron pot and more significantly in meat
points(type,aov(data$iron~type*food)$fit, pch=19, col=5) # The full two-way ANOVA model
points(type,aov(data$iron~type+food)$fit, pch=19, col=6)# No interaction term
points(type,aov(data$iron~type)$fit, pch=19, col=7) # One-way ANOVA using group
points(type,aov(data$iron~food)$fit, pch=19, col=8)  # One-way ANOVA using gender


# Also look at the residuals  - It is difficult for me to intepret the below graph as i am not compatible with 2 way anova
par(mfrow=c(3,2))
plot(type,aov(data$iron~type+food)$res, col=2, lwd=1, pch=19,
     main="Residuals for Full Model with Interactions")    # Residuals for full two-way model - gender
plot(food,aov(data$iron~type+food)$res, col=2, lwd=1, pch=19, 
     main="Residuals for Full Model with Interactions")     # Residuals for full two-way model - group
plot(type,aov(data$iron~type+food)$res, col=2, lwd=1, pch=19, 
     main="Residuals for Full Model without Interactions")    # No interaction term - gender
plot(food,aov(data$iron~type+food)$res, col=2, lwd=1, pch=19, 
     main="Residuals for Full Model without Interactions")     # No interaction term - group
plot(type,aov(data$iron~type)$res, col=2, lwd=1, pch=19, 
     main="Residuals for One-Way ANOVA on type")            # One-way ANOVA on group
plot(food,aov(data$iron~food)$res, col=2, lwd=1, pch=19,
     main="Residuals for One-Way ANOVA on food")          # One-way ANOVA on group
par(mfrow=c(1,1))

