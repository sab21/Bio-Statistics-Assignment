#Qn 1
n <- 100
sampleMean <- 7
sd <- 2
mu <- 7.5 
#H0: mu = 7.5
#HA: mu != 7.5
#Assumption of independence & Normality met

#a. df = n-1 = 99
#b.
se <- sd/sqrt(n)
t <- (sampleMean-mu)/se; t #test statistic = -2.5
p <- pnorm(t, lower.tail = T)*2 #For two sided test
paste("p Value = ", p)#HO Rejected

#c. Confidence Interval - U hv done mistake here
#When u r computing test statistic for CI always calculate it as per alpha 
  #NOT as per significance test statistic formula
#t95= 1.96 (for two tailed ) always.
t95 <- qnorm(.975)#for 95% CI, alpha = .05 and one tail consist alpha/2 = 0.025 

lowerLim <- sampleMean - t95*se
upperLim <- sampleMean + t95*se
lowerLim;upperLim # Clearly mu 7.5 is outside the boundary.

#Note: In question it asked to calcute CI against mean mu ie 7.5 which can't be computed 


#Qn 2
smean <- 3.9
mu <- 4
n <- 20
sd <- .2
se <- sd/sqrt(n)
#H0: mu>=4
#HA: mu<4
#a. Because the inspector does't care if it weigh greter than 4oz
#b. 
t <- (smean-mu)/se;t
#c.
p <- pnorm(t);p #for one side t test
#HO can be rejected for alpha = .05 whereas
#Ho CAN'T be rejected for alpha  = .01

#3.
husband <- c(224,310,266,332,244,178,280,176,242,260)
wife <- c(200,270,288,296,270,180, 268,244,210,236)
#H0: Cholesterol lvl of Husband is equal or less than Wife
#HA: Cholesterol lvl of Husband is more than Wife
# As the researcher thinks that Husband has more cholesterol than wife so he wants to test his hypotheses

#Test - two sample t test
test <- t.test(husband, wife, alternative = "greater")
test
test$p.value #p-value = 0.4 (Can't reject Null Hypotheses)
#Note that number of sample is less than 30


#Q4
#a
# Two Sample Unpaired t test 
#Ho: The difference between Tensile strength of two coloured bandages is 0
#HA : True difference in means of tensile strength of 2 coloured is different
df2 <- n1+n2-2;df#Assumed equal varience

#b
#Two sample Unpaired t test
#H0: There is no difference between individual running speed who run atleat twice in week and who doesn't
#HA: True difference between running speed of individuals who run atleast twice and who doesn't is greater than 0
s1 <- 2.7;x1 <- 25; n1 <- 25
x2 <- 29;s2 <- 4.3; n2 <- 13
df <- ((s1^2/n1 + s2^2/n2)^2)/(((s1^2/n1)^2)^2/(n1-1)+((s2^2/n2)^2)/(n2-1))
df#Assumed Unequal Varience
df2 <- n1+n2-2#Assumed equal varience
