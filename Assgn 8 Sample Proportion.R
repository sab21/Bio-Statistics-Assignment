################################   Assgn - 8 



#1) A newsletter recently reported that 90% of adults drink milk. A regional farmers' organization is
#planning a new marketing campaign across its tri-county area. They randomly poll 600 people in
#the area. In this sample, 525 people said that they drink milk. In this problem we will explore the
#following question: Do these data provide strong evidence that the 90% figure is not accurate for
#this region?
#a) What are the hypotheses to test if the proportion of people who drink milk in this region is
#equal to 90% (as reported by the article in the newsletter) or not?
#b) What is the point estimate for the proportion of people who drink milk in this region?
#c) What is the value of the large-sample z statistic and what is the corresponding P-value?
#d) The regional farmers' organization reported that their results were statistically significant. Give
#an example of a value of ?? that they could have used.

#Solution
#a.
#H0:P = 0.9; 90% of adults of true population drink milk
#HA: P!= 0.9; 
p <- 0.9

#b.
p.hat <- 525/600 #point estimate = 0.875

#c.
n <- 600#sample size
#Assumption for sample size p*n & (1-p)*n should be greater than 10 - True in the given condition
se <- sqrt(p*(1-p)/n)#std error = 0.12247

#p.hat ~ N(mean = 0.9, se = 0.1225) - Normality equation

z.value <- (p.hat-p)/se # z statistic = 2.04 on lower tail
p.value <- pnorm(abs(z.value), lower.tail = F)*2 #for two tail
p.value #0.04122 
#the given p value is less than alpha = 0.05  and hence H0 can be rejected



#2) A company receives shipments of a component used in the manufacture of a high-end acoustic
#speaker system. When the components arrive, the company selects a random sample from the
#shipment and subjects the selected components to a rigorous set of tests to determine if the
#components in the shipments conform to their specifications. From a recent large shipment, a
#random sample of 250 of the components was tested, and 24 units failed one or more of the tests.
#a) What is the point estimate of the proportion of components in the shipment that fail to meet the
#company's specifications?
#b) What is the standard error of the estimated proportion?
#c) At the 98% level of confidence, what is the margin of error in this estimate?
#d) What is the 95% confidence interval estimate for the true proportion of components, p, that fail
#to meet the specifications?
#e) If the company wanted to test the null and alternative hypotheses: H0: p = 0.10 against Ha: p ???
#0.10 at the ?? = 0.05 level of significance, what conclusion would they draw?

#a.
p.hat <- 24/250#point estimate

#b.
n <- 250
se <- sqrt(p.hat*(1-p.hat)/n) #std error of estmated proportion
se #0.0186

#c.
#me <- z(.98)*se
# for 98% confidence interval alpha = 0.2
#alpha/2 = 0.1
z98 <- qnorm(0.99) #2.326
me98 <- z98*se #0.0433
paste("98 % confidence interval :", round(p.hat-me98,4), "to", round(p.hat+me98,4))


#d.
z95 <- qnorm(0.975) #for two tail - 1.96
me95 <- z95*se #0.0365
paste("95 % confidence interval :", round(p.hat-me95,4), "to", round(p.hat+me95,4))


#e.
#H0: p = 0.1
#HA: p != 0.1
#alpha = 0.05
#from d. it is evident that with significance value  of 0.05 confidence interval lies between 
# 0.0595 to 0.1325 means 0.1 lies in between confidence interval and hence null hypotheses of
# p=0.1 can't be rejected with given data

#Hyp Testing
p <- 0.1
se <- sqrt(p*(1-p)/n)

z.value <- (p.hat-p)/se #0.2108
p.value <- pnorm(abs(z.value), lower.tail = F)*2 #for two tail
p.value#0.833
#HO can't be rejected with given data and given 

#NOTE : 
#For hypotheses testing, se <- sqrt(p*(1-p)/n) where p=population mean
#whereas
#For Confidence interval, se <- sqrt(p.hat*(1-p.hat)/n) where p.hat = sample mean/point estimate



#3) A manufacturer of insecticides has developed two formulations for new insecticides using two
#different compounds, A and B. To test the effectiveness of these two formulations a study was
#conducted in which insects of a particular type were released into an enclosure and the enclosure
#then sprayed with an insecticide. Independent studies were conducted for each formulation. The
#results of the study are provided in the following table:
#Compound A Compound B
#Number of Insects Released 200 180
#Number of Insects Killed 135 108
#a) What is the estimate of the difference between the true proportion of insects killed by the two
#formulations?
#b) What is the standard error of the estimated difference? 
#c) To test the hypotheses: H0: pA - pB ??? 0 against Ha: pA - pB > 0, what is the value of the test
#statistic?
#d) What is the P-value of the test and the conclusion the company can reach as a result? Use ?? =
#0.05.

n.a <- 200;a <- 180
n.b <- 135;b <- 108
p.a <- a/n.a;p.a #point estimate of compound A
p.b <- b/n.b;p.b #point estimate of compound B

#a.
pe <- p.a-p.b;pe#estimate difference

#b
se <- sqrt((p.a*(1-p.a)/n.a)+(p.b*(1-p.b)/n.b));se #standard error

#c
#H0: pA - pB ??? 0 against Ha: pA - pB > 0
z <- pe/se;z # test statistic = 2.473

#d
p.value <- pnorm(z, lower.tail = F); round(p.value,5) ## p value for upper tail only

#p value is significant so HO can be rejected



#4) Last year, one county reported that among 3000 Caucasian women (Group 1) who had babies, 95
#had multiples (twins, triplets, etc.). The report also stated that there were 20 multiple births to 600
#African American women (Group 2).
#a) The researchers would like to assess if there is a racial difference in the likelihood of multiple
#births. Write the null and alternative hypothesis for p1 and p2.
#b) Under the null hypothesis, the two population proportions are equal. Give an estimate for this
#common proportion.
#c) Evaluate the test described in (a). What are your findings?


#a
#H0: the true difference in multiple birth case between Caucasian and African women is 0 
#HA: p1 - p2 != 0

#b
n1 <- 3000;b1 <- 95;p1 <- b1/n1;p1 #Caucasian women
n2 <- 600;b2 <- 20;p2 <- b2/n2;p2 #African women
p <- p1-p2;p#estimate proportion

#c
#assumption of independence and sample size is satisfied

se <- sqrt((p1*(1-p1)/n1)+(p2*(1-p2)/n2));se #standard error

z <- p/se;z

p.value <- pnorm(abs(z), lower.tail = F)*2 #for two tail
round(p.value,5)
#p value is not significant hence Ho can't be rejected for given data