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

M1 <- glm(Severity ~ Age + Shape + Margin, data = mammo, family = "binomial")
summary(M2)




# Below, I go through and do the normal backwards selection with the F test, and find a parsimonious model to be
# Severity ~ Age + Shape + Margin + Shape : Margin


# backwards selection via F-test with full model of all two way inrteractions ???


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


summary(back.glm)


# After model selection all terms significant (F - test) but from summary, not all terms significant (z - test) ???

#Justification = parsimony????



probs <- fitted(back.glm)


length(probs)
summary(probs)
probs

mammo_no_NA <- na.omit(mammo)


# why is it that we don't have 961 values in the fitted probabilities?
# if because NA aren't included, why don't we only have 831, but 887 instead?



predict(back.glm,data.frame(Age=50,Shape="3",Margin="2",Density="1"))

predict(back.glm,data.frame(Age=62,Shape="3",Margin="3", Density="1"))


# why is this giving values outside of our bounds and not the same as the fitted?
