################################################################################
################################################################################
#
#       Waseda University -- September 2017 -- Hierarchical Modeling
#
#       Lucas Leemann, lleemann@gmail.com -- DAY 5
#
################################################################################
################################################################################


rm(list=ls())
setwd("/Users/lleemann/Dropbox/Democratic Deficit exchange folder/Book chapter/Data and Code")
library(foreign)
library(lme4)
library(extrafont)
library(arm)

data1 <- read.dta("Minaret.dta")


head(data1)
summary(data1)

table(data1$minaret)

data2 <- data1[-which(is.na(data1$minaret)),]
summary(data2)

table(data2$minaret)/sum(table(data2$minaret))

model.check <- glm(minaret ~ factor(educ) + factor(agegroup), data=data2, family=binomial("probit"))
summary(model.check)

load("Census.Rda")
Censusobject




#### MRP
model1 <- glmer(minaret ~ 1 + (1|female) + (1|agegroup) + (1|educ) + (1|canton), data= data2, family=binomial("probit"))
summary(model1)

re.female <- ranef(model1)$female[[1]]
re.agegroup <- ranef(model1)$agegroup[[1]]
re.educ <- ranef(model1)$educ[[1]]
re.canton <- ranef(model1)$canton[[1]]

# 2 * 4 * 6 * 26

female.re <- rep(re.female,24)
age.re <- rep(kronecker(re.agegroup,c(1,1)), 6)
educ.re <- kronecker(re.educ,rep(1, 8))
ind.re <- rowSums(cbind(female.re, age.re, educ.re))
ind.re <- ind.re + fixef(model1)



y.lat1 <- rep(NA,1248)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  y.lat1[a:b] <- ind.re + re.canton[i]
}

# predicted probabilities!
p1 <- pnorm(y.lat1)

# How many such people live in Switzerland?
dim(Censusobject)
a <- c()
for (i in 1:26){
  a <- c(a,Censusobject[,i])
}


# Estimate
sum(p1*a)/sum(a)
# True Value 58% -- actual result was 57.5% yes


##############################################################################################################


MINARET <- c(51.80,60.80,61.20,63.80,66.30,62.40,62.80,68.80,56.70,55.90,64.00,48.40,59.90,63.50,63.70,71.40,65.90,
             58.60,64.00,67.70,68.30,46.90,58.00,49.30,40.30,51.20)/100

rightP <- c(39.30,36.70,44.30,46.50, 59.90,47.10,49.10,48.90,44.30,27.00,41.40,28.50,35.20,42.80,42.60,48.30,48.30,
            34.90,46.80, 48.90,42.20,19.00,25.00,18.00,17.90,19.80)/100
# source: https://www.bk.admin.ch/ch/d/pore/va/20080601/can532.html


canton <- c(1:26)

data3 <- cbind(rightP,canton)

data4 <- merge(data2,data3,by="canton")


#### MRP
model2 <- glmer(minaret ~ rightP + (1|female) + (1|agegroup) + (1|educ) + (1|canton), data= data4, family=binomial("probit"))
summary(model2)



re.female <- ranef(model2)$female[[1]]
re.agegroup <- ranef(model2)$agegroup[[1]]
re.educ <- ranef(model2)$educ[[1]]
re.canton <- ranef(model2)$canton[[1]]

# 2 * 4 * 6 * 26

female.re <- rep(re.female,24)
age.re <- rep(kronecker(re.agegroup,c(1,1)), 6)
educ.re <- kronecker(re.educ,rep(1, 8))
ind.re <- rowSums(cbind(female.re, age.re, educ.re))
ind.re <- ind.re + fixef(model2)[1]
beta1 <- fixef(model2)[2]



y.lat2 <- rep(NA,1248)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  y.lat2[a:b] <- ind.re + beta1 * rightP[i] + re.canton[i]
}

# predicted probabilities!
p2 <- pnorm(y.lat2)

# How many such people live in Switzerland?
dim(Censusobject)
a <- c()
for (i in 1:26){
  a <- c(a,Censusobject[,i])
}


# National estimate
sum(p2*a)/sum(a)

# Cantonal estimates
mrp.minaret2 <- rep(NA,26)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p2 <- pnorm(y.lat2[a1:a2])
  a <- Censusobject[,i]
  mrp.minaret2[i] <-  sum(p2*a)/sum(a)
}


# Cantonal estimates via disaggregation
TAB <- table(attributes(model2)$frame$canton,attributes(model2)$frame$minaret)
dis.minaret <- TAB[,2]/rowSums(TAB)


# Cantonal estimates w/o L2 variables
mrp.minaret1 <- rep(NA,26)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p1 <- pnorm(y.lat1[a1:a2])
  a <- Censusobject[,i]
  mrp.minaret1[i] <-  sum(p2*a)/sum(a)
}

# Plot
par(family="CMU Serif")
plot(mrp.minaret2, MINARET, pch=20, cex=3, col=rgb(0,0,255,150,maxColorValue=255),
     bty="n", ylab="Official Ballot Result", xlab="Estimated Cantonal Support", ylim=c(.4,.72), xlim=c(.4,.72))
points(dis.minaret,MINARET, pch=18, col=rgb(0,0,255,60,maxColorValue = 255), cex=1.5)
points(mrp.minaret1,MINARET, col="blue", pch=21, cex=1.5,bg=rgb(255,255,255,100,maxColorValue = 255))
abline(c(0,1), lty=2, lwd=.5)
legend(.6,.45,legend = c("Disaggregation","MrP w/o L2", "MrP w/ L2"), pch=c(18,21,20), 
       col=c(rgb(0,0,255,60,maxColorValue = 255),"blue",
             rgb(0,0,255,200,maxColorValue = 255)), bty="n", pt.cex=c(1.5,1.5,3))
text(.68,.7,"45 Degree Line", cex=0.8)


########################################################################################
#### MrP with Uncertainty

BLOCK <- sim(model2, n=1000)

re.female <- attributes(BLOCK)$ranef$female
re.agegroup <- attributes(BLOCK)$ranef$agegroup
re.educ <- attributes(BLOCK)$ranef$educ
re.canton <- attributes(BLOCK)$ranef$canton

# Generating ideal types, but 1,000 simulation for each ideal type, hence matrix 48*1000
# 2 * 4 * 6 (* 26)

female.re <- matrix(NA, 48,1000)
  for (i in 1:1000){
    female.re[,i] <- rep(re.female[i,,1],24)
  }
  
age.re <- matrix(NA, 48,1000)
  for (i in 1:1000){
    age.re[,i] <- rep(kronecker(re.agegroup[i,,1],c(1,1)), 6)
  }
  
educ.re <- matrix(NA, 48,1000)
for (i in 1:1000){
  educ.re[,i] <-   kronecker(re.educ[i,,1],rep(1, 8))
}

ind.re <- female.re + age.re + educ.re

#constant <- attributes(BLOCK)$fixef[,1]
#Constant <- matrix(constant,48,1000,byrow = TRUE)
#ind.re <- ind.re + Constant

#beta1 <- attributes(BLOCK)$fixef[,2]



y.lat2 <- matrix(NA,1248,1000)
for (i in 1:26){
  a <- ((i-1)*48)+1
  b <- a + 47
  level2 <- attributes(BLOCK)$fixef %*% matrix(c(1,rightP[i]),2,1) + re.canton[i]
  level2.48 <- matrix(rep(level2,48),48,1000, byrow = TRUE)
  y.lat2[a:b,] <- ind.re + level2.48
}


# Cantonal estimates
mrp.minaret2.unc <- matrix(NA,26,1000)
for (i in 1:26){
  a1 <- ((i-1)*48)+1
  a2 <- a1 + 47
  p2 <- pnorm(y.lat2[a1:a2,])
  a <- Censusobject[,i]
  mrp.minaret2.unc[i,] <-  t(p2)%*%a/sum(a)
}

size.canton <- table(data2$canton)

par(family="CMU Serif", mfrow=c(1,2))
plot(mrp.minaret2, MINARET, pch=20, cex=2,#cex=size.canton/20, 
     col=rgb(0,0,255,150,maxColorValue=255),
     bty="n", ylab="Official Ballot Result", xlab="Estimated Cantonal Support", ylim=c(.4,.72), xlim=c(.4,.72))
  for (i in 1:26){
    draws <- mrp.minaret2.unc[i,]
    CI <- quantile(draws,c(0.025,0.975))
    segments(CI[1],MINARET[i],CI[2],MINARET[i], col="blue", lwd=0.5)
  }
abline(c(0,1), lty=2, lwd=.5)
#points(mrp.minaret1, MINARET, cex=size.canton/30, pch=20)



par(family="CMU Serif")
plot(mrp.minaret2, MINARET, pch=20, cex=size.canton/20, 
     col=rgb(0,0,255,150,maxColorValue=255),
     bty="n", ylab="Official Ballot Result", xlab="Estimated Cantonal Support", ylim=c(.4,.72), xlim=c(.4,.72))
#for (i in 1:26){
#  draws <- mrp.minaret2.unc[i,]
#  CI <- quantile(draws,c(0.025,0.975))
#  segments(CI[1],MINARET[i],CI[2],MINARET[i], col="blue", lwd=0.5)
#}
abline(c(0,1), lty=2, lwd=.5)
points(mrp.minaret1, MINARET, cex=size.canton/20, pch=20)
for (i in 1:26){
  if (abs(mrp.minaret2[i]-MINARET[i])<abs(mrp.minaret1[i]-MINARET[i])){
    arrows(mrp.minaret1[i],MINARET[i],mrp.minaret2[i],MINARET[i], col="blue", lwd=0.5)
  }
  if (abs(mrp.minaret2[i]-MINARET[i])>abs(mrp.minaret1[i]-MINARET[i])){
    arrows(mrp.minaret1[i],MINARET[i],mrp.minaret2[i],MINARET[i], col="red", lwd=0.5)
  }
  
}



par(mfrow=c(1,1))
orderL <- order(MINARET)

plot(seq(.31,.8,length.out=26),1:26, bty="n", pch=26, yaxt="n",
     xlab="Support for Initiative", ylab="")

for (i in 1:26){
  points(mrp.minaret2[orderL][i],i, col="blue", pch=19, cex=2)
  if(i!=6&i!=24) points(MINARET[orderL][i],i, col="blue", pch=4, cex=2)
  if(i==6|i==24)  points(MINARET[orderL][i],i, col="red", pch=4, cex=2)
}

for (i in 1:26){
  draws <- mrp.minaret2.unc[orderL[i],]
  CI <- quantile(draws,c(0.025,0.975))
  segments(CI[1],i,CI[2],i, col="blue", lwd=0.5)
}

legend(.32,26,c("MrP Estimate", "Official Outcome (within CI)","Official Outcome (outside CI)"), 
       pch=c(19,4,4), col=c("blue","blue","red"), bty="n")





