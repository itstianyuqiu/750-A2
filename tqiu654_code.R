#import dataset
setwd("~/Desktop")
p <- read.csv(file.choose() , header = T)
p<-p[,c(2,4,7:13)]

#install
install.packages("lattice")
install.packages("psych")
install.packages("nortest")
install.packages("car")
install.packages("RColorBrewer")
install.packages("corrplot")
install.packages("nlme")

#3.Definition of Main Variables & Visual Exploration
##3.2 Visual Exploration
### Description data
library("psych")
describe(p)

###3.2.1 Select first 16 Projects(5% of all as sample)
library("lattice")
xyplot(forks~Time | prjId, 
       data = p[1 : 128 , ], 
       panel = function(x, y){
         panel.xyplot(x, y)
         panel.lmline(x, y)
         }, 
       as.table = T)

###Normality Check
qqnorm(p$forks)
qqline(p$forks)
hist(p$forks)
plot(density(p$forks))
hist(log(p$forks+1))
boxplot(p$forks~p$Time,
        data = p , 
        ylab = "forks" , 
        xlab = "Time" , 
        col = c("gold" , "grey" , "tan"))
###KS Test
library("nortest")
lillie.test(p$forks)

##Linearity Check
library("car")
library("RColorBrewer")
scatterplotMatrix(~forks + Time + commits + issues + pullReq + CmtCmnt,
                  transform = TRUE , 
                  data = p)

#4 Data Cleaning & Preparation
##4.1 Importance Analysis
library("corrplot")
b <- cor(p)
corrplot(b)
corrplot(b,method = "pie")
corrplot(b,method = "color",addCoef.col="grey")
col = colorRampPalette(c("navy" , "white" , "firebrick3"))
corrplot(b , type = "upper" , col = col(10) , tl.pos = "d")
corrplot(b , add = TRUE , 
         type = "lower" , 
         method = "number" , 
         diag = FALSE , 
         tl.pos = "n" , 
         cl.pos = "n" , 
         col = col(10))

#4.2 Data Cleaning
sum(is.null(p))
sum(is.na(p))


#Multi-level Longitudinal Model
##5.1 Model A
library(nlme)
model.a <- lme(forks~1 , p, random= ~1 | prjId)
summary(model.a)
VarCorr(model.a)

##5.2 Model B
model.b <- lme(forks~Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.b)
VarCorr(model.b)

##5.3 Model C
###5.3.1 Model C1
model.c1 <- lme(forks ~ commits*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.c1)
VarCorr(model.c1)
###5.3.1 Model C2
model.c2 <- lme(forks ~ issues*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.c2)
VarCorr(model.c2)
###5.3.1 Model C3
model.c3 <- lme(forks ~ pullReq*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.c3)
VarCorr(model.c3)
###5.3.1 Model C4
model.c4 <- lme(forks ~ CmtCmnt*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.c4)
VarCorr(model.c4)

##5.4 Model D
###5.4.1 Model D1
model.d1 <- lme(forks ~ commits*Time+issues*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.d1)
VarCorr(model.d1)
###5.4.1 Model D2
model.d2 <- lme(forks ~ commits*Time+pullReq*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.d2)
VarCorr(model.d2)
###5.4.1 Model D3
model.d3 <- lme(forks ~ commits*Time+CmtCmnt*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.d3)
VarCorr(model.d3)
###5.4.1 Model D4
model.d4 <- lme(forks ~ issues*Time+pullReq*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.d4)
VarCorr(model.d4)
###5.4.1 Model D5
model.d5 <- lme(forks ~ issues*Time+CmtCmnt*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.d5)
VarCorr(model.d5)
###5.4.1 Model D6
model.d6 <- lme(forks ~ pullReq*Time+CmtCmnt*Time , data=p, random= ~ Time | prjId, method="ML")
summary(model.d6)
VarCorr(model.d6)

##5.5 Model E
###5.5.1 Model E1
model.e1 <- lme(forks ~ issues + pullReq*Time , data = p, random = ~ Time | prjId, method="ML")
summary(model.e1)
VarCorr(model.e1)
###5.5.1 Model E2
model.e2 <- lme(forks ~ issues*Time+pullReq , data = p, random = ~ Time | prjId, method="ML")
summary(model.e2)
VarCorr(model.e2)

