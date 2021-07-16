library(dplyr)
library(ggplot2)
library('Hmisc')
library(GGally)
library(car)
library(arm)
library(pROC)
library(e1071)
library(caret)


smoking <- read.csv("/Users/fengyi111/Desktop/2020-Fall/702/702-Assignment2/smoking.csv",header = TRUE)

head(smoking)
smoking$premature <- rep(0,nrow(smoking))
smoking$mrace_factor <- rep(0,nrow(smoking))
smoking$med_factor <-factor(smoking$med)
smoking$premature[smoking$gestation < 270] <- 1
smoking$mht_cent <- smoking$mht - mean(smoking$mht)
smoking$mpregwt_cent <- smoking$mpregwt - mean(smoking$mpregwt)
smoking$mage_cent <-smoking$mage - mean(smoking$mage)

#smoking$premature<- factor(smoking$premature,levels=c(0,1),labels=c("Expected-term","Pre-term"))
smoking$mrace_factor[smoking$mrace<=5] <- 'white'
smoking$mrace_factor[smoking$mrace==6] <- 'mexican'
smoking$mrace_factor[smoking$mrace==7] <- 'black'
smoking$mrace_factor[smoking$mrace==8] <- 'asian'
smoking$mrace_factor[smoking$mrace==9] <- 'mix'
smoking$inc_factor <- factor(smoking$inc)
# EDA
hist(smoking$premature,breaks = 2)


# smoke vs. premature - chi sqrt test shows non significance
tapply(smoking$smoke,smoking$premature,function(x) table(x)/sum(table(x)))
chisq.test(table(smoking[,c("premature","smoke")]),simulate.p.value = TRUE)


# The distribution of number of parity in either pre-term or expected term does not vary
binnedplot(y=smoking$premature,smoking$parity,xlab="mpregwt_cent",ylim=c(0,1),col.pts="navy",
           ylab ="Premature birth or not",main="Binned mother weight and premature cases",
           col.int="white")

# The distribution of mother's age in either pre-term or expected term does not vary
ggplot(smoking,aes(x=mage_cent, y=premature,group=premature)) +
  geom_boxplot() + 
  scale_fill_brewer(palette="Reds") +
  labs(title="Premature vs. Mother's Age",
       x="Mother's Age",y="Premature") + 
  theme_classic() + theme(legend.position="none")


# Education vs premature, for people with hightest education the probability of baby being premature is significantly higher than that in other groups 
# might want to explore eduation, premature, and age. 
# chi sqr test shows significance 
tapply(smoking$premature,smoking$med_factor,function(x) table(x)/sum(table(x)))
chisq.test(table(smoking[,c("premature","med_factor")]),simulate.p.value = TRUE)



# No significant difference
tapply(smoking$premature,smoking$inc_factor,function(x) table(x)/sum(table(x)))
chisq.test(table(smoking[,c("premature","inc_factor")]),simulate.p.value = TRUE)


# EDA for race and premature, shows that for mixed ethnicity, there is a significantly smaller premature rate while asian has a significantly larger premature rate
tapply(smoking$premature,smoking$mrace_factor,function(x) table(x)/sum(table(x)))
chisq.test(table(smoking[,c("premature","mrace_factor")]),simulate.p.value = TRUE)


# Binned plots
# premature and mgregwt_cent
binnedplot(y=smoking$premature,smoking$mpregwt_cent,xlab="mpregwt_cent",ylim=c(0,1),col.pts="navy",
           ylab ="Premature birth or not",main="Binned mother weight and premature cases",
           col.int="white")

# no salient relationship between mother's height and premature
binnedplot(y=smoking$premature,smoking$mht_cent,xlab="mht_cent",ylim=c(0,1),col.pts="navy",
           ylab ="Premature birth or not",main="Binned mother height and premature cases",
           col.int="white")


# Interaction
# Smoke and Premature by race
# White is the only category where the relationship is sifnificant
premature_white <- smoking$premature[smoking$mrace_factor=='white']
smoke_white <- smoking$smoke[smoking$mrace_factor=='white']
tapply(premature_white,smoke_white,function(x) table(x)/sum(table(x)))
chisq.test(table(premature_white,smoke_white))

#Asian
premature_asian <- smoking$premature[smoking$mrace_factor=='asian']
smoke_asian <- smoking$smoke[smoking$mrace_factor=='asian']
tapply(premature_asian,smoke_asian,function(x) table(x)/sum(table(x)))
chisq.test(table(premature_asian,smoke_asian),simulate.p.value = TRUE)

#Mexican
premature_mexican <- smoking$premature[smoking$mrace_factor=='mexican']
smoke_mexican <- smoking$smoke[smoking$mrace_factor=='mexican']
tapply(premature_mexican,smoke_mexican,function(x) table(x)/sum(table(x)))
chisq.test(table(premature_mexican,smoke_mexican),simulate.p.value = TRUE)

#Black
premature_black <- smoking$premature[smoking$mrace_factor=='black']
smoke_black <- smoking$smoke[smoking$mrace_factor=='black']
tapply(premature_black,smoke_black,function(x) table(x)/sum(table(x)))
chisq.test(table(premature_black,smoke_black),simulate.p.value = TRUE)

#Mix
premature_mix <- smoking$premature[smoking$mrace_factor=='mix']
smoke_mix <- smoking$smoke[smoking$mrace_factor=='mix']
tapply(premature_mix,smoke_mix,function(x) table(x)/sum(table(x)))
chisq.test(table(premature_mix,smoke_mix),simulate.p.value = TRUE)


# interaction of med and mage with premature
# The trend is not significantly different
ggplot(smoking,aes(x=premature, y=mage_cent, group=premature)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Age vs. Premature, by Mother's Education",
       x="Premature or not ",y="Number of parity") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ med_factor)

# interaction of parity and smoke
ggplot(smoking,aes(x=premature, y=parity, group=premature)) +
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Reds") +
  labs(title="Parityvs Baby Being Premature, by Smoke or Not",
       x="Premature or not ",y="Parity") + 
  theme_classic() + theme(legend.position="none") +
  facet_wrap( ~ smoke)




# Model Fitting
smoking1 <- glm(premature ~ parity + mrace_factor + smoke + mht_cent+med_factor + mage_cent  + mpregwt_cent + inc_factor, data = smoking, family = binomial)
summary(smoking1)

# Check assumptions: bin residual plots, accuracy, specificity 

# Overall model binned residual plot.
rawresid1 <- residuals(smoking1,"resp")

#Overall binned residual plots.  
#With 3 pints out of bounds, and a trend that looks quadratic
binnedplot(x=fitted(smoking1),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot",col.pts="navy")


# binned residual plot for parity. A very huge decline at parity = 5. Can take a closer look as this value
binnedplot(x=smoking$parity,y=rawresid1,xlab="parity",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
table(smoking$premature,smoking$parity)
# greater than 5 
# Parity number later than 5 is signifncalty less. Can consider transform by collapsing parity into different categories
table(smoking$premature,smoking$mrace_factor)


# m_age looks normal with no salient trend
binnedplot(x=smoking$mage_cent,y=rawresid1,xlab="mage_cent",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# mpgwt shows a trend that looks like quadratic. Might consider adding a quadratic term in the model 
binnedplot(x=smoking$mpregwt_cent,y=rawresid1,xlab="mpregwt_cent",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# mpgwt shows a trend that looks like quadratic. Might consider adding a quadratic term in the model 
binnedplot(x=smoking$mht_cent,y=rawresid1,xlab="mpregwt_cent",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# Best performance is at roc = 0.238
Conf_mat1 <- confusionMatrix(as.factor(ifelse(fitted(smoking1) >= 0.3, "1","0")),
                             as.factor(smoking$premature),positive = "1")
Conf_mat1$table
Conf_mat1$overall["Accuracy"]
Conf_mat1$byClass[c('Sensitivity','Specificity')]

roc(smoking$premature,fitted(smoking1),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")



# Transformation: add quadratic mpregwt to see if this alleviates the quadratic trend in residual plot 
smoking$mpregwt_quarc <- smoking$mpregwt^2 - mean(smoking$mpregwt^2)


# Fit the second model with quadratic mpregwt screen. The distribution is more random which is good 
smoking2<- glm(premature ~parity + mrace_factor + smoke + med_factor  + mpregwt_cent + mpregwt_quarc + inc_factor, data = smoking, family = binomial)
summary(smoking2)
rawresid2 <- residuals(smoking2,"resp")
binnedplot(x=fitted(smoking2),y=rawresid2,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot2",col.pts="navy")

# binned residual plot for parity. A very huge decline at parity = 5. Can take a closer look as this value
binnedplot(x=smoking$parity,y=rawresid2,xlab="parity",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# m_age looks normal with no salient trend
binnedplot(x=smoking$mage_cent,y=rawresid2,xlab="mage_cent",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# mpgwt_quarc does not seem to have improved
binnedplot(x=smoking$mpregwt_quarc,y=rawresid2,xlab="mpregwt_cent",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


# Confusion matrix and ROC to see how this model has improved from smoking1
# Aread under the curve actually has decrease. 
Conf_mat2 <- confusionMatrix(as.factor(ifelse(fitted(smoking2) >= 0.5, "1","0")),
                             as.factor(smoking$premature),positive = "1")
Conf_mat2$table
Conf_mat2$overall["Accuracy"]

roc(smoking$premature,fitted(smoking2),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")


# We should in fact take out of the mpregwt_quarc term, and we can try to use log instead

# Transformation: log of mpregwt and center it
smoking$mpregwt_log <- log(smoking$mpregwt) 


# Fit the second model with centered log value of mpregwt. The distribution is more random which is good 
smoking3<- glm(premature ~parity + mrace_factor + smoke + med_factor  + mht_cent+ mpregwt_log + inc_factor, data = smoking, family = binomial)
summary(smoking3)

rawresid3 <- residuals(smoking3,"resp")
binnedplot(x=fitted(smoking3),y=rawresid3,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot3",col.pts="navy")


# binned residual plot for parity. A very huge decline at parity = 5. Can take a closer look as this value
binnedplot(x=smoking$parity,y=rawresid3,xlab="parity",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# m_age looks normal with no salient trend
binnedplot(x=smoking$mage_cent,y=rawresid3,xlab="mage_cent",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# mpgwt_log does not seem to have improved
binnedplot(x=smoking$mpregwt_log,y=rawresid3,xlab="mpregwt_log",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


# Confusion matrix and ROC to see how this model has improved from smoking2
# Aread under the curve = 0.6635 We'll keep this variable  
Conf_mat3 <- confusionMatrix(as.factor(ifelse(fitted(smoking3) >= 0.5, "1","0")),
                             as.factor(smoking$premature),positive = "1")
Conf_mat3$table
Conf_mat3$overall["Accuracy"]

roc(smoking$premature,fitted(smoking3),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")



# Stepwise model selection using AIC

n <- nrow(smoking)
null_model <- glm(premature~1,data=smoking,family=binomial)
step(null_model,scope=formula(smoking3),direction="both",
     trace=0)

# Based on model selection, we run a model with med_factor, mrace_factor, mregwt_log, and smoke
smoking4<- glm(premature ~mrace_factor + smoke + med_factor  + mpregwt_log, data = smoking, family = binomial)
summary(smoking4)
rawresid4 <- residuals(smoking4,"resp")
binnedplot(x=fitted(smoking4),y=rawresid4,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot4",col.pts="navy")


# mpgwt_log does not seem to have improved
binnedplot(x=smoking$mpregwt_log,y=rawresid4,xlab="mpregwt_log",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


# Confusion matrix and ROC to see how this model has improved from smoking2
# Aread under the curve = 0.658
# Very low true positive rate and very high true negative rate. Can be caused by predicting 0 disproportionately
# Also, the distrubtion of premature is uneven, so it could also be caused from that 
# Besides, high specificity means a high true negative rate, which is helpful for detecting that the baby will not be premature. 
Conf_mat4 <- confusionMatrix(as.factor(ifelse(fitted(smoking4) >= 0.3, "1","0")),
                             as.factor(smoking$premature),positive = "1")
Conf_mat4$table
Conf_mat4$overall["Accuracy"]
Conf_mat4$byClass[c('Sensitivity','Specificity')]

roc(smoking$premature,fitted(smoking4),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")

# Compare with null model, with avery low p-value, which confirms that our model is better than the null model. 
smoking_null <- glm(premature~1,family=binomial(link=logit),data=smoking)
anova(smoking_null,smoking4,test='Chisq')




# Adding three interaction terms and then use anova to find which ones are significant

smoking5 <- glm(premature ~ smoke*( mrace_factor +parity) +med_factor * mage_cent+ mpregwt_log, data = smoking, family = binomial)
summary(smoking5)

# Anova result shows that our model with two interaction terms are not statistically different from the model with the previous model. 
# As a result, we will only include the interaction term that we were intersted in, which is race vs. smoke
anova(smoking5,smoking4,test='Chisq')


# Final Model
# Why race:white not significant anymore? 
smoking6 <- glm(premature ~ smoke*mrace_factor +med_factor+ mpregwt_log , data = smoking, family = binomial)
summary(smoking6)



smoking_null <- glm(premature~1,family=binomial(link=logit),data=smoking)
anova(smoking_null,smoking6,test='Chisq')


rawresid6 <- residuals(smoking6,"resp")
binnedplot(x=fitted(smoking6),y=rawresid6,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg.residuals",main="Binned residual plot6",col.pts="navy")


# binned residual plot for mpregwt A very huge decline at parity = 5. Can take a closer look as this value
binnedplot(x=smoking$mpregwt_log,y=rawresid6,xlab="mpregwt_log",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat6 <- confusionMatrix(as.factor(ifelse(fitted(smoking6) >= 0.3, "1","0")),
                             as.factor(smoking$premature),positive = "1")
Conf_mat6$table
Conf_mat6$overall["Accuracy"]
Conf_mat6$byClass[c('Sensitivity','Specificity')]

# Very high specificity and increased ROC 
roc(smoking$premature,fitted(smoking6),plot=T,print.thres="best",legacy.axes=T,
    print.ouc = T, col="red3")


# Multicolinearity 
# All below 10 which shows no evidence of multicolinearity 
check_vif <- smoking6
vif(check_vif)


confint.default(smoking6)   #on log odds scale
exp(confint.default(smoking6))   #on odds scale