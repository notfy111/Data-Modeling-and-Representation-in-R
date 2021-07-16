library(ggplot2)
library(cobalt)
library(MatchIt)
library(randomForest)

asthma <- read.csv('/Users/fengyi111/Desktop/2020-Fall/702/assignment5/Asthma.txt',sep = " ", header = TRUE)

asthma$pg <- factor(asthma$pg -1)
str(asthma)[]

asthma$com_t <- asthma$com_t - mean(asthma$com_t)
asthma$pcs_sd <- asthma$pcs_sd - mean(asthma$pcs_sd)
asthma$mcs_sd <- asthma$mcs_sd - mean(asthma$mcs_sd)
asthma$i_sex <- factor(asthma$i_sex)
asthma$i_race <- factor(asthma$i_race)
asthma$i_educ <- factor(asthma$i_edu)
asthma$i_insu <- factor(asthma$i_insu)
asthma$i_drug <- factor(asthma$i_drug)
asthma$i_seve <- factor(asthma$i_seve)





str(asthma)

###### Covariate balance
# Covariates in this data are not balanced: particularly, com_t pcs_sd, and i_educ have very high mean difference across the two groups
bal.tab(list(treat=asthma$pg,covs=asthma[,2:11],estimand="ATT"))
love.plot(list(treat=asthma$pg,covs=asthma[,2:11],estimand="ATT"),stars = "std")

# Since physician group 1 has more a larger sample size, we'll swtich 0/1 to 
asthma$treatment_s = 0
asthma$treatment_s[asthma$pg == 0] = 1
asthma$treatment_s <- factor(asthma$treatment_s)

### 2###
## Propensity Score
#Fit the model
cov_names <- names(asthma)
p_formula <- as.formula(paste("treatment_s ~",
                              paste(cov_names[!cov_names %in% c("i_aqoc","pg","treatment_s")],
                                    collapse = " + ")))
pscorereg <- glm(p_formula,data = asthma, family=binomial)
summary(pscorereg)
pscores <- predict(pscorereg, type = "response")

summary(pscores)
ggplot(asthma, aes(pscores)) +
  geom_histogram(alpha=.6,fill=rainbow(10),bins=10)

# density plot looks well
ggplot(asthma, aes(x=pscores, fill=treatment_s)) +
  geom_density(alpha=.3) +
  xlim(0, 1)


### A ###
#Check number of observations outside of the overlap region between the two groups
#First the left tails
sum(pscores < max(min(pscores[asthma$treatment_s==0]),
                  min(pscores[asthma$treatment==1])))
#Next the right tails
sum(pscores > min(max(pscores[asthma$treatment_s==0]),
                  max(pscores[asthma$treatment_s==1])))

# 40 outliers from the left tail and 8 from the right tail

#If there are "outliers",
#get row index for observations that violate overlap.
index <- which((pscores < max(min(pscores[asthma$treatment_s==0]),
                              min(pscores[asthma$treatment_s==1])) |
                  pscores > min(max(pscores[asthma$treatment_s==0]),
                                max(pscores[asthma$treatment_s==1]))) == TRUE)
asthma_bal <- asthma[-index,]; pscores <- pscores[-index]

### B ###
# Using one-to-one, nearest neighbor matching on the estimated propensity scores, check balance again
matchesAsthma <- matchit(p_formula,
                     method = "nearest", distance = "logit", data = asthma_bal)
matchesAsthma$match.matrix
matchesAsthma$nn
asthmamatcheddata <- match.data(matchesAsthma)

ggplot(asthmamatcheddata, aes(y=distance, x=treatment_s, fill=treatment_s)) +
  geom_boxplot()
# check covariates again
# i_educ 4, com_t, pcs_sd are larger than 0.1
bal.tab(list(treat=asthmamatcheddata$treatment_s,covs=asthmamatcheddata[,2:11],estimand="ATT"))
love.plot(list(treat=asthmamatcheddata$treatment_s,covs=asthmamatcheddata[,2:11],estimand="ATT"),stars = "std")


### C ###
# calculate average causal effect; create standard error estimate; construct a 95% CI
avgeffect <- mean(asthmamatcheddata$i_aqoc[asthmamatcheddata$treatment_s == 1]) - 
          mean(asthmamatcheddata$i_aqoc[asthmamatcheddata$treatment_s == 0])
# standard error of difference
se <- sqrt(var(asthmamatcheddata$i_aqoc[asthmamatcheddata$treatment_s == 1])/97 + 
             var(asthmamatcheddata$i_aqoc[asthmamatcheddata$treatment_s == 0])/97)
# confidence interval of average causal effect
# interpretation: we are 95% confident that the true difference between treatment and control is between 0.0276 and 0.2816.
# since this interval does not include 0, we can reject the null hypothesis
# and our average causa effect is statisticaly significant
upper <- avgeffect + 1.96 * se
lower <- avgeffect - 1.96 * se


### D ###
regmodel <- glm(i_aqoc ~ i_age+i_sex+i_race+i_educ+i_insu+i_drug+i_seve+mcs_sd+com_t+pcs_sd+distance+ treatment_s, family = binomial,
               data=asthmamatcheddata)
## confidence interval 
summary(regmodel)
# turn out the log causal effect is statistically significnat with a coefficient 0.92033
# causal odds ratio is 2.51
causalOR <- exp(0.92033)
# caulssal odds ratio is 2.51, meaing that the causal effect of having satisfaction == 1 being in the treatment group is 151% times than that of the control group. 

### E ## 
### b ###
# Using one-to-one, nearest neighbor matching on the estimated propensity scores, check balance again
matchesAsthmaRplc <- matchit(p_formula,
                         method = "nearest", distance = "logit", data = asthma_bal,  replace=TRUE,ratio =5)
matchesAsthmaRplc$match.matrix
matchesAsthmaRplc$nn
asthmamatcheddataRplc <- match.data(matchesAsthmaRplc)

ggplot(asthmamatcheddataRplc, aes(y=distance, x=treatment_s, fill=treatment_s)) +
  geom_boxplot()
# check covariates again
# i_educ 4, com_t, pcs_sd also larger than 0.1
bal.tab(list(treat=asthmamatcheddataRplc$treatment_s,covs=asthmamatcheddataRplc[,2:11],estimand="ATT"))


### c ###
# calculate average causal effect; create standard error estimate; construct a 95% CI
avgeffectRplc <- mean(asthmamatcheddataRplc$i_aqoc[asthmamatcheddataRplc$treatment_s == 1]) - 
  mean(asthmamatcheddataRplc$i_aqoc[asthmamatcheddataRplc$treatment_s == 0])
# standard error of difference of proportion of patients who were satisfied with the physician
se2 <- sqrt(var(asthmamatcheddataRplc$i_aqoc[asthmamatcheddataRplc$treatment_s == 1])/97 + 
             var(asthmamatcheddataRplc$i_aqoc[asthmamatcheddataRplc$treatment_s == 0])/102)
# confidence interval of average causal effect
# interpretation: we are 95% confident that the true difference of proportion of patients who were satisfied between treatment and control is between 2.9% and 28%
# since this interval does not include 0, we can reject the null hypothesis
# and our average causa effect is statisticaly significant
upperRplc <- avgeffect + 1.96 * se2
lowerRplc <- avgeffect - 1.96 * se2


### d ###
regmodelRplc <- glm(i_aqoc ~ i_age+i_sex+i_race+i_educ+i_insu+i_drug+i_seve+mcs_sd+com_t+pcs_sd+distance+ treatment_s, family = binomial,
                data=asthmamatcheddataRplc)
## confidence interval 
summary(regmodelRplc)
# turn out the log causal effect is statistically significnat with a coefficient 0.87791
# causal odds ratio is 2.405
causalRplc <- exp(0.87791)
# caulssal odds ratio is 2.405, meaing that the causal effect of having satisfaction == 1 being in the treatment group is 1.405 times than that of the control group. 
