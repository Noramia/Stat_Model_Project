setwd("C:/Users/Oliver/OneDrive/Documents/2018/Semester 1/Statistical Modelling/Project")
mammo <- read.table("mammo.txt", header = T, sep = ',')
# mammo<-read.csv("mammo.txt",header=TRUE,na.strings = "?") reading the data this way eliminates lines 10-16
library(broom)
library(MASS)


# ---- Data Entry and Cleaning ----
#removing '?' values:
table(mammo)
mammo$BI.RADS[mammo$BI.RADS == "?"] <- NA
mammo$Age[mammo$Age == "?"] <- NA
mammo$Shape[mammo$Shape == '?'] <- NA
mammo$Margin[mammo$Margin == "?"] <- NA
mammo$Density[mammo$Density == "?"] <- NA
mammo$Severity[mammo$Severity == "?"] <- NA


# ---- Data Visualisation and Summaries ----
pairs(mammo)

corMat <- cor(mammo)
round(corMat, 4)

summary(mammo$BI.RADS)
summary(mammo$Age)
summary(mammo$Shape)
summary(mammo$Margin)
summary(mammo$Density)
summary(mammo$Severity)


# ---- Model Fitting and Selection ----
M1 <- lm(Severity ~ BI.RADS+Age+Shape+Margin+Density, data = mammo) #cant use lm here because you have binanry response
tidy(M1) 
summary(M1)
S2 <- 0.3471^2
step(M1, scale = S2)

#/// ---- take a look at this and let me know 
library(tidyr)
library(dplyr)
library(ggplot2)
mammo<-read.csv("mammo.txt",header=TRUE,na.strings = "?")
help("read.csv") # if anyone needs further reading on how the above lines work
mammo<-mammo%>%select(Age,Shape,Margin,Density,Severity) #definig a subset as BI.RADS is not a predictor
head(mammo)
str(mammo) # check the class of all the variables all at once
View(mammo)
attach(mammo) # eliminates attaching mammo to each variables all the times
Shape<-as.factor(Shape)
class(Shape)
Margin<-as.factor(Margin)
class(Margin)
Density<-as.factor(Density)
class(Density)
Severity<-as.factor(Severity)
class(Severity)
class(Age)
#/// # obtaning the logistic regression model
glm0<-glm(Severity~Age+Shape+Margin+Density,family = 'binomial') 
summary(glm0)
glm1<-glm(Severity~Age+Margin+Density,family = 'binomial')
glm1<-glm(Severity~Age+Margin,family=binomial(link = "logit"))
summary(glm1)
#/// ---- To find the probabilities--- Althu not sure if it is correct
prob<-fitted(glm1)
View(table(prob))

#///---- not sure if this is helpful but i found a function that goes through your data and add the possible missing values---
random.imp <- function (mammo){
  missing <- is.na(mammo)
  n.missing <- sum(missing)
  a.obs <- mammo[!missing]
  imputed <- mammo
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
mammo.imm<- random.imp (mammo)
mammo.imm
