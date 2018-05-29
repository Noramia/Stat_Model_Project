setwd("C:/Users/Oliver/OneDrive/Documents/2018/Semester 1/Statistical Modelling/Project")
mammo <- read.table("mammo.txt", header = T, sep = ',')
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
M1 <- lm(Severity ~ BI.RADS+Age+Shape+Margin+Density, data = mammo)
tidy(M1)
summary(M1)
S2 <- 0.3471^2
step(M1, scale = S2)
