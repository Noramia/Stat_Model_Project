setwd("C:/Users/Oliver/OneDrive/Documents/2018/Semester 1/Statistical Modelling/Project")

library(broom)
library(MASS)
library(magrittr)
library(dplyr)


# ---- Data Entry and Cleaning ----
#reading data and removing '?' values:
mammo <- read.csv("mammo.txt", header=TRUE, na.strings = "?")
mammo <- dplyr::select(mammo, Age, Shape, Margin, Density, Severity)


#checking variable types and converting
str(mammo)
mammo$Shape <- as.factor(mammo$Shape)
mammo$Margin <- as.factor(mammo$Margin)
mammo$Density <- as.factor(mammo$Density)
mammo$Severity <- as.factor(mammo$Severity)



# ---- Data Visualisation and Summaries ----
pairs(mammo)

summary(mammo)

summary(mammo$BI.RADS)
summary(mammo$Age)
summary(mammo$Shape)
summary(mammo$Margin)
summary(mammo$Density)
summary(mammo$Severity)


# ---- Model Fitting and Selection ----
M1 <- glm(Severity ~ Age*Shape*Margin*Density, data = mammo, family = "binomial")
tidy(M1)
summary(M1)


M2 <- glm(Severity ~ Age + Shape + Margin, data = mammo, family = "binomial")
summary(M2)


M3 <- glm(Severity ~ (Age + Shape + Margin + Density)^2, data = mammo, family = "binomial")
summary(M3)


M4 <- glm(Severity ~ Age + Shape + Margin + Age : Shape + Age : Margin, data = mammo, family = "binomial")
summary(M4)


T1 <- glm(Severity ~ (Age + Shape + Margin + Density)^2, data = mammo, family = "binomial")
summary(T1)


T2 <- glm(Severity ~ (Age + Shape + Margin)^2, data = mammo, family = "binomial")


# Below, I go through and do the normal backwards selection with the F test, and find a parsimonious model to be
# Severity ~ Age + Shape + Margin + Shape : Margin

back.glm <- T1

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Margin:Density)

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Shape:Density)

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Age:Margin)

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Age:Shape)

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Age:Density)

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Density)

drop1(back.glm, test = "F")
back.glm <- update(back.glm, .~. - Density)


summary(back.glm)

step(T1)

