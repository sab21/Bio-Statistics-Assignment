########          Assignment 5


#1. To test the accuracy of a lab scale, a standard weight of 10 g is repeatedly weighed.
  #Assume the scale readings are Normally distributed with unknown mean (the mean is 10g
  #if the scale is accurate). The standard deviation for the readings is 0.0002 g.
#a) The weight is measured 5 times. The sample mean is 10.0035 g. Give a 99%
  #confidence interval for the mean of repeated measurements of the weight.
#b) How many measurements would need to be made in order to get a margin of error of
  #0.0001 g with 99% confidence?
#c) What is the power to detect a systematic weight error of 0.0002 g with 5
  #measurements and ?? = 0.01?
#d) How many measurements are necessary to detect a systematic weight error of
  #0.0001 g, with ?? = 0.02 and power = 0.8?

#1.a
sampleMean <- 10.0035
sd <- .0002;n <- 5;se <- sd/sqrt(n)
marginError <- qnorm(.995)*se #qnorm to find t statistic for lower tail of alpha/2 = .01/2= .005
left <- sampleMean-marginError
right <- sampleMean+marginError
left;right #Answer 

#1.b
#For margin of error = .0001 Find n?
me <- .0001 #sd/sqrt(n) = me/t-stats.995 => n = (t-stat.995 * sd / me)^2
n1 = ceiling((qnorm(.995) * sd / me)^2) #ceiling to cap the value of sample to upper end
print(n1)#Ans

#1.c
pow <- power.t.test(n = 5, delta = (sampleMean-10), sd = .0002, sig.level = 0.01,power = NULL,type = "one.sample")
pow
pow$power #Answer

#1.d
pow2 <- power.t.test(n = NULL, delta = (sampleMean-10), sd = .0001, sig.level = 0.02,power = .8,type = "one.sample")
pow2$n#Ans



#2. You would like to prove the alternative hypothesis ?? < 0 using ??=0.01. Assume ?? = 7.
#a) Give H0.
#H0 - Mean is equal to greater than 0

#c) Repeat the procedure with the same sample mean value, but with n=20, 30, 40 and 50.
n5 <- 50 #20,30,40
se <- sd/sqrt(n5)
t5 <- abs(x-0)/se
p5 <- pnorm(t5, lower.tail = F)
print(c("t-stats=",t5, "p-value = ", p5)) #Ans


n2 <- 20 #20,30,40,50
se <- sd/sqrt(n2)
t2 <- abs(x-0)/se
p2 <- pnorm(t2, lower.tail = F)
print(c("t-stats=",t2, "p-value = ", p2)) #Ans


n3 <- 30 #20,30,40
se <- sd/sqrt(n3)
t3 <- abs(x-0)/se
p3 <- pnorm(t3, lower.tail = F)
print(c("t-stats=",t3, "p-value = ", p3))#Ans


n4 <- 40 #20,30,40
se <- sd/sqrt(n4)
t4 <- abs(x-0)/se
p4 <- pnorm(t4, lower.tail = F)
print(c("t-stats=",t4, "p-value = ", p4))##Ans

#d) Plot the values of the test statistic and P-values versus the sample size. Describe
  #what this demonstrates about the effect of sample size on significance testing.
t <- c(t1,t2,t3,t4,t5)
p <- c(p1,p2,p3,p4,p5)
n <- c(n1,n2,n3,n4,n5)


par(mfcol=c(2,1))
plot(n,t,type = "l", main = "t-stats vs n")
plot(n,p,type = "l", main = "p- value vs n")

#3. The active ingredient in antiseptic bandages is 8-hydroxyquinoline (8h). Since this
  #chemical can cause skin damage, its concentration needs to be carefully controlled. One
  #brand of bandages lists the 8h volume as 1%. A random sample of 9 bandages resulted
  #in a mean of 1.02%. Assume that the population is Normally distributed and the standard
  #deviation is 0.05%.
  #a) State your null and alternative hypotheses. Argue why a 1-sided or 2-sided test might
    #be most appropriate. Your test should match your hypotheses.
  #b) Use a significance level of ??=0.01, what can you conclude about the sample? What
    #is the P-value?

#Ans
#H0 - concentration of 8-hydroxyquinoline (8h) is equal to 1%
#Ha - concentration of 8-hydroxyquinoline (8h) is greater than 1%

t <- (1.02-1)/.05
pnorm(t, lower.tail = F)
pnorm(1.02,mean = 1, sd = .05, lower.tail = F) 
#p-value = .344: Not Significant with alpha = .01, Can't Reject H0 with given data


#4. You have made a series of measurements on two different cell types to estimate the size
  #of the nucleus (in um). Assume the nuclear diameter is Normally distributed in both cell
  #types. You find the following:
  #CellType   SampleSize   MeanDiameter   Assumed ??
  #U2OS        33            0.74         0.20
  #MCF-7       42            1.05         0.35

#a) Find the 95% confidence interval for the nuclear diameter in U2OS cells.
n <- 33
m <- .74
sd <- .2
se <- sd/sqrt(n)
t <- qnorm(.975) 
me <- t*se
left <- m-me
right <- m+me
left;right #Ans



#b) How large a sample would you need for a margin of error for the nuclear diameter in
  #U2OS cells to be 0.05 um for the 95% CI?
me <- .05
n1 <- ceiling((qnorm(.975)*sd/me)^2)#Ans

#c) For the MCF-7 cells, find a 95% CI for nuclear diameter.
n <- 42
m <- 1.05
sd <- .35
se <- sd/sqrt(n)
t <- qnorm(.975) 
me <- t*se
left <- m-me
right <- m+me
left;right#Ans

#d) How large a sample is needed in this case (MCF-7 cells) to have a margin of error of 0.1um?
me <- .1
n1 <- ceiling((qnorm(.975)*sd/me)^2)#Ans


#e) Suppose you wish to distinguish which kind of cells you have based on their size, with
  #95% confidence. You can do this if the 95% CI's do not overlap. Determine how many
  #total samples you require in order to accomplish the task.
m1 <- .74 #mean of U20S
m2 <- 1.05 #mean of MCF7
sd1 <- .2
sd2 <- .35
boundary <- (m1+m2)/2 #upper boundary for U20S and lower boundary for MCF7

nU20S <- ceiling((qnorm(.975)*sd1/abs(boundary-m1))^2) 
nU20S #number of sample for U20S 
nMCF7 <- ceiling((qnorm(.975)*sd2/abs(boundary-m2))^2)
nMCF7 #Ans for MCF7
