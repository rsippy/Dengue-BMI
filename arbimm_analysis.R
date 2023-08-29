#########################
# Analyses for ""
# Rachel Sippy
# 
# 0. Environment & Data
# 1. Table 1
# 2. Suppl Table 1
# 3. 
#########################

library(tidyverse)

load("arbimm.RData")

# subset for age group analyses of allergies
aller_adult <- subset(arbimm, arbimm$AgeGr == 3)
aller_young <- subset(arbimm, arbimm$AgeGr < 3)

# remove pregnant participants for analyses of body mass
npreg <- arbimm[arbimm$WomPreg != "Si", ]

# subset for age group analyses of body mass, exclude missing body mass
bm_kids <- subset(npreg, npreg$AgeGr == 1 & !is.na(npreg$CircArm))
bm_teen <- subset(npreg, npreg$AgeGr == 2  & !is.na(npreg$CircArm))
bm_young <- subset(npreg, npreg$AgeGr < 3  & !is.na(npreg$CircArm))
bm_adult <- subset(npreg, npreg$AgeGr == 3  & !is.na(npreg$BMI))

bodymass <- rbind(bm_kids, bm_teen, bm_adult)

###################################
# Table 1
# Full dataset summary variables
# Non-pregnant body mass outcomes
###################################

# n
table(arbimm$Hospitalized)
table(bodymass$Hospitalized)

# outcomes
aggregate(bm_adult$BMI, list(Hospitalized = bm_adult$Hospitalized), mean)
aggregate(bm_adult$BMI, list(Hospitalized = bm_adult$Hospitalized), sd)
aggregate(bm_young$CircArm, list(Hospitalized = bm_young$Hospitalized), mean)
aggregate(bm_young$CircArm, list(Hospitalized = bm_young$Hospitalized), sd)
table(bm_adult$Hospitalized, bm_adult$BMIClass)

table(arbimm$Hospitalized, arbimm$HistAllergies)
table(arbimm$Hospitalized[grep("orat", arbimm$HistMedsSpec)])

# demographics
aggregate(arbimm$Age, list(Hospitalized = arbimm$Hospitalized), mean, na.rm=TRUE)
aggregate(arbimm$Age, list(Hospitalized = arbimm$Hospitalized), sd, na.rm=TRUE)
table(arbimm$Hospitalized, arbimm$Gender)

aggregate(bodymass$Age, list(Hospitalized = bodymass$Hospitalized), mean, na.rm=TRUE)
aggregate(bodymass$Age, list(Hospitalized = bodymass$Hospitalized), sd, na.rm=TRUE)
table(bodymass$Hospitalized, bodymass$Gender)

# symptoms and health status
aggregate(arbimm$SympTemp, list(Hospitalized = arbimm$Hospitalized),mean,na.rm=TRUE)
aggregate(arbimm$SympTemp, list(Hospitalized = arbimm$Hospitalized),sd,na.rm=TRUE)
table(arbimm$Hospitalized, arbimm$SympRash)
table(arbimm$Hospitalized, arbimm$SympFever7Days)
table(arbimm$Hospitalized, arbimm$WomPreg)
table(arbimm$Hospitalized, arbimm$HistDiab)

# clinic
table(arbimm$Hospitalized, arbimm$Clinic)

###################################
# Suppl Table 1
# Full dataset
###################################
table(arbimm$Hospitalized, arbimm$Dengue)
table(arbimm$Hospitalized, arbimm$Chik)
table(arbimm$Hospitalized, arbimm$Zika)
table(arbimm$Dengue, arbimm$Chik, arbimm$Hospitalized)

###################################
# Allergies analysis
# Full dataset
###################################

## Table 2
# Children and adolescents, unadjusted
u_yo_al <- glm(Hospitalized ~ Allergies, data=aller_young, family = "binomial")
uya <- data.frame(co = coef(u_yo_al), confint(u_yo_al))
uya2 <- data.frame(unlist(sapply(uya, function(x) round(exp(x), 3))))
uya2$p <- coef(summary(u_yo_al))[,4]
colnames(uya2)[1:3] <- c("Estimate", "LL", "UL")
rownames(uya2) <- c("Intercept", "Allergies")
uya2 

# Children and adolescents, adjusted
a_yo_al <- glm(Hospitalized ~ Allergies + Age + Gender, data=aller_young, 
               family = "binomial")
aya <- data.frame(co = coef(a_yo_al), confint(a_yo_al))
aya2 <- data.frame(unlist(sapply(aya, function(x) round(exp(x), 3))))
aya2$p <- coef(summary(a_yo_al))[,4]
colnames(aya2)[1:3] <- c("Estimate", "LL", "UL")
rownames(aya2) <- c("Intercept", "Allergies", "Age", "Gender")
aya2

# Adults, unadjusted
u_ad_al <- glm(Hospitalized ~ Allergies, data=aller_adult, family = "binomial")
uaa <- data.frame(co = coef(u_ad_al), confint(u_ad_al))
uaa2 <- data.frame(unlist(sapply(uaa, function(x) round(exp(x), 3))))
uaa2$p <- coef(summary(u_ad_al))[,4]
colnames(uaa2)[1:3] <- c("Estimate", "LL", "UL")
rownames(uaa2) <- c("Intercept", "Allergies")
uaa2

# Adults, adjusted
a_ad_al <- glm(Hospitalized ~ Allergies + Age + Gender, data=aller_adult, 
               family = "binomial")
aaa <- data.frame(co = coef(a_ad_al), confint(a_ad_al))
aaa2 <- data.frame(unlist(sapply(aaa, function(x) round(exp(x), 3))))
aaa2$p <- coef(summary(a_ad_al))[,4]
colnames(aaa2)[1:3] <- c("Estimate", "LL", "UL")
rownames(aaa2) <- c("Intercept", "Allergies", "Age", "Gender")
aaa2

###################################
# Body Mass analysis
# Non-pregnant dataset
###################################

## Table 3
# Children, unadjusted
u_kids_bm <- glm(Hospitalized ~ CircArm, data = bm_kids, family = "binomial")
ukb <- data.frame(co = coef(u_kids_bm), confint(u_kids_bm))
ukb2 <- data.frame(unlist(sapply(ukb, function(x) round(exp(x), 3))))
ukb2$p <- coef(summary(u_kids_bm))[,4]
colnames(ukb2)[1:3] <- c("Estimate", "LL", "UL")
rownames(ukb2) <- c("Intercept", "CircArm")
ukb2

# Children, adjusted
a_kids_bm <- glm(Hospitalized ~ CircArm + Age + Gender, data = bm_kids, 
                 family = "binomial")
akb <- data.frame(co = coef(a_kids_bm), confint(a_kids_bm))
akb2 <- data.frame(unlist(sapply(akb, function(x) round(exp(x), 3))))
akb2$p <- coef(summary(a_kids_bm))[,4]
colnames(akb2)[1:3] <- c("Estimate", "LL", "UL")
rownames(akb2) <- c("Intercept", "CircArm", "Age", "Gender")
akb2

# Adolescents, unadjusted
u_teen_bm <- glm(Hospitalized ~ CircArm, data = bm_teen, family = "binomial")
utb <- data.frame(co = coef(u_teen_bm), confint(u_teen_bm))
utb2 <- data.frame(unlist(sapply(utb, function(x) round(exp(x), 3))))
utb2$p <- coef(summary(u_teen_bm))[,4]
colnames(utb2)[1:3] <- c("Estimate", "LL", "UL")
rownames(utb2) <- c("Intercept", "CircArm")
utb2

# Adolescents, adjusted
a_teen_bm <- glm(Hospitalized ~ CircArm + Age + Gender, data = bm_teen,
                 family = "binomial")
atb <- data.frame(co = coef(a_teen_bm), confint(a_teen_bm))
atb2 <- data.frame(unlist(sapply(atb, function(x) round(exp(x), 3))))
atb2$p <- coef(summary(a_teen_bm))[,4]
colnames(atb2)[1:3] <- c("Estimate", "LL", "UL")
rownames(atb2) <- c("Intercept", "CircArm", "Age", "Gender")
atb2

## Table 4
# Adults, unadjusted
u_ad_bm <- glm(Hospitalized ~ BMI, data = bm_adult, family = "binomial")
uab <- data.frame(co = coef(u_ad_bm), confint(u_ad_bm))
uab2 <- data.frame(unlist(sapply(uab, function(x) round(exp(x), 3))))
uab2$p <- coef(summary(u_ad_bm))[,4]
colnames(uab2)[1:3] <- c("Estimate", "LL", "UL")
rownames(uab2) <- c("Intercept", "BMI")
uab2

# Adults, adjusted
a_ad_bm <- glm(Hospitalized ~ BMI + Age + Gender, data = bm_adult, 
               family = "binomial")
aab <- data.frame(co = coef(a_ad_bm), confint(a_ad_bm))
aab2 <- data.frame(unlist(sapply(aab, function(x) round(exp(x), 3))))
aab2$p <- coef(summary(a_ad_bm))[,4]
colnames(aab2)[1:3] <- c("Estimate", "LL", "UL")
rownames(aab2) <- c("Intercept", "BMI", "Age", "Gender")
aab2

## Figure
new_bm <- with(bm_adult, data.frame(Age = rep(mean(Age), 107), 
                                    Gender = rep("Fem", 107), 
                                    BMI = seq(17.5, 70.5, by = 0.5)))
new_bm2 <- rbind(new_bm, data.frame(Age = rep(mean(bm_adult$Age), 107), 
                                    Gender = rep("Masc", 107), 
                                    BMI = seq(17.5, 70.5, by = 0.5)))

new_bm3 <- cbind(new_bm2, predict(a_ad_bm, newdata = new_bm2, type = "link",
                                    se = TRUE))
new_bm3 <- within(new_bm3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

ggplot(new_bm3, aes(x = BMI, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Gender), alpha = 0.2) + 
  geom_line(aes(color = Gender), size = 1)
