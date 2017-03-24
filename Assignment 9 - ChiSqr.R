##############################################
#       Assignment No 9 -  Chi Square Test
##############################################

#Qn No 1
glass <- factor(c(rep("yes",126),rep("no",174))) #creating vector of glass wearing status
levels(glass) <- c("yes","no") #leveling factors
table(glass)

readingAmount <- factor(c(rep("Above Average",47),rep("Average", 48),rep("Below Average",31),
                          rep("Above Average",26),rep("Average", 78),rep("Below Average",70)))
table(readingAmount)

data <- data.frame(glass,readingAmount)
head(data)
table(data)# required dataset

library(gmodels)
with(data,CrossTable(glass,readingAmount)) #Contigency Table with chi square contribution
plot(data)
with(data, plot(glass~readingAmount))


#a. Chi Square test for independence
#HO: Reading amount and glass wearing status are independent. Wearing glass doesn't depend on amount of reading


#b. Under the null hypothesis, what is the expected number of above average readers who wear
#glasses? 

#Expected Count = (Row Total x Column Total )/ Table Total
#for Above Average reader who wear glasses
rowTotal <- 174 #total number of glass wearer
colTotal <- 73 #Total no of Above Avg reader
tableTotal <- 300 #Grand Total
expectedAAR <- round(rowTotal*colTotal/tableTotal) #expected Above Average Reade
paste("Expected Above Average Reader who wear glasses = ", expectedAAR)

#c.
df <- (2-1)*(3-1) #degree of freedom = (no. of rows -1)*(no of col -1)
#calculation of chisqr value manually
#data preparation
yesTotal <- length(glass[glass=="yes"])
noTotal <- length(glass[glass=="no"])
AARTotal <- length(readingAmount[readingAmount=="Above Average"])
ARTotal <- length(readingAmount[readingAmount=="Average"])
BARTotal <- length(readingAmount[readingAmount=="Below Average"])
tableTotal <- length(data$glass)
e.AARy <- yesTotal*AARTotal/tableTotal#expected Avg above reader with glass
e.ARy <- yesTotal*ARTotal/tableTotal#expected Avg reader with glass
e.BARy <- yesTotal*BARTotal/tableTotal#expected Below Avg above reader with glass
e.AARn <- noTotal*AARTotal/tableTotal#expected Avg above reader without glass
e.ARn <- noTotal*ARTotal/tableTotal#expected Avg reader without glass
e.BARn <- noTotal*BARTotal/tableTotal#expected Below Avg above reader without glass

AARy <- nrow(subset(data,glass=="yes" & readingAmount=="Above Average")) #actual Above Avg reader with glass
ARy <- nrow(subset(data,glass=="yes" & readingAmount=="Average"))#actual  Avg reader with glass
BARy <- nrow(subset(data,glass=="yes" & readingAmount=="Below Average"))#actual Below Avg reader with glass
AARn <- nrow(subset(data,glass=="no" & readingAmount=="Above Average"))#actual Above Avg reader without glass
ARn <- nrow(subset(data,glass=="no" & readingAmount=="Average"))#actual Avg reader without glass
BARn <- nrow(subset(data,glass=="no" & readingAmount=="Below Average"))#actual Below Avg reader without glass

#You can simply put values from contigency table if u r getting confused. See question no 3

#chisqr value = sumation((actual-expected)^2/expected)
chiSqr <- (((AARy-e.AARy)^2)/e.AARy)+(((ARy-e.ARy)^2)/e.ARy)+(((BARy-e.BARy)^2)/e.BARy)+
  (((AARn-e.AARn)^2)/e.AARn)+(((ARn-e.ARn)^2)/e.ARn)+(((BARn-e.BARn)^2)/e.BARn)
chiSqr #Chi Square VAlue

pchisq(chiSqr,df,lower.tail = F)
#p Value is small so we reject the null hypotheses
#With given data we can conclude that there is significant association between amount of reading and glass wearing
#but we cannot conclude that with increase in reading amount glass wearing cases increase


#Alternative Method: By direct chisq.test
chisq.test(data$glass,data$readingAmount)


#2
# B and C are True

#HO: Type of failure is independent of location of failure

e.A.I <- 97*111/200;e.A.I #expected type A failure in internal location
e.B.I <- 97*42/200;e.B.I #expected type B failure in internal location
e.C.I <- 97*47/200;e.C.I #expected type C failure in internal location
e.A.E <- 103*111/200;e.A.E#expected type A failure in external location
e.B.E <- 103*42/200;e.B.E#expected type B failure in external location
e.C.E <- 103*47/200;e.C.E#expected type C failure in external location

#c
chix <- (((50-e.A.I)^2)/e.A.I)+(((16-e.B.I)^2)/e.B.I)+(((31-e.C.I)^2)/e.C.I)+
  (((61-e.A.E)^2)/e.A.E)+(((26-e.B.E)^2)/e.B.E)+(((16-e.C.E)^2)/e.C.E)
df <- (2-1)*(3-1)#degree of freedom
pchisq(chix,df, lower.tail = F)#p value


#Alternative
location <- c(rep("Internal",97),rep("External",103))
type <- c(rep("A",50),rep("B",16),rep("C",31),
          rep("A",61),rep("B",26),rep("C",16))
data <- data.frame(location,type)
with(data,CrossTable(location,type)) #contigency table

chisq.test(data$location,data$type)#solution for c



#3.
#a.
#H0: Choice of personal goals not related with gender of student that means both are independent


#b.
rowTot <- 96+295#total good grades seeker
colTot <- 96+32+94#total boys
total <- 96+32+94+295+45+40

e.boy.goodGrade <- round(rowTot*colTot/total);e.boy.goodGrade#expected boy seeking good grade

#c
#I am not going to compute it manually anymore
gender <- c(rep("Boys", 96+32+94),rep("Girls",295+45+40))
goal <- c(rep("Good Grade",96), rep("Popular",32),rep("Sports",94),
          rep("Good Grade",295), rep("Popular",45),rep("Sports",40))
data <- data.frame(gender,goal)
chisq.test(data$gender,data$goal)
#chisq statistic = 89.966
#dof = 2
#pValue = 2.2e-16 - significant

#p value is significant, HO can be rejected in favour of HA means
#pesonal goals depend on gender