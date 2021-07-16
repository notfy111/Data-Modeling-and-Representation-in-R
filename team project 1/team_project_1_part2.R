# .libPaths(c('/usr/local/lib/R/library', .libPaths()))
# load library
library(ggplot2)
library(GGally)
library(knitr)
library(xtable)
library(rms)
library(arm)
library(pROC)
library(e1071)
library(caret)
library(gridExtra)
library(car)

lab <- read.csv('lalondedata.txt')
# lalonda_data$treat_fac <- factor(lalonda_data$treat)
# lalonda_data$black <- factor(lalonda_data$black)
# lalonda_data$hispan <- factor(lalonda_data$hispan)
# lalonda_data$married <- factor(lalonda_data$married)
# lalonda_data$nodegree <- factor(lalonda_data$nodegree)


lab$treat <- factor(lab$treat)
lab$black <- factor(lab$black)
lab$hispan <- factor(lab$hispan)
lab$married <- factor(lab$married)
lab$nodegree <- factor(lab$nodegree)
lab$educ <- factor(lab$educ)
lab$re74 <- as.numeric(lab$re74)
lab$re75 <- as.numeric(lab$re75)
lab$re78 <- as.numeric(lab$re78)


levels(lab$educ) <- list('elementary'=c('0','1','2','3','4','5'), 'middle'=c('6', '7', '8'), 'high'=c('9', '10', '11', '12'), 'college' = c('13','14', '15', '16', '17', '18'))
# 
# lab$educ <- factor(lab$educ)


lab$employed78[lab$re78 > 0] <- 1
lab$employed78[lab$re78 == 0] <- 0


lab$employed78 <- factor(lab$employed78)


lab$employed78_n <- as.integer(lab$employed78) -1
##################################
#EDA
##################################

table(lab$employed78) #more employed than unemployed

#continuous variable:age
ggplot(lab,aes(x=employed78, y=age, fill=employed78)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="age vs employed78",
       x="employed?",y="age") + 
  theme_classic() + theme(legend.position="none")

ggplot(lab,aes(x=employed78, y=re74, fill=employed78)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="re74 vs employed78",
       x="employed?",y="anual total income 1974") + 
  theme_classic() + theme(legend.position="none")

#discrete variable:treat
apply(table(lab[,c("employed78","treat")])/sum(table(lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) # probability for men who received training is employed is lower than men who did not receive training

#discrete variable:educ
apply(table(lab[,c("employed78","educ")])/sum(table(lab[,c("employed78","educ")])),
      2,function(x) x/sum(x)) # probability for men who received college degree or beyond is employed is the highest among all education level, high school the second and elementary the third, middle school the fourth. 
#discrete variable:black
apply(table(lab[,c("employed78","black")])/sum(table(lab[,c("employed78","black")])),
      2,function(x) x/sum(x)) #  black men has lower probability to be employed than non-black men.

#discrete variable:hispan
apply(table(lab[,c("employed78","hispan")])/sum(table(lab[,c("employed78","hispan")])),
      2,function(x) x/sum(x)) #  hispanic men has lower probability to be employed than non-hispanic men.

#discrete variable:married
apply(table(lab[,c("employed78","married")])/sum(table(lab[,c("employed78","married")])),
      2,function(x) x/sum(x)) #  married men has higher probability to be employed than men who was not married.

#discrete variable:nodegree
apply(table(lab[,c("employed78","nodegree")])/sum(table(lab[,c("employed78","nodegree")])),
      2,function(x) x/sum(x)) #  men who did not drop out of school has higher probability to be employed than men who did drop out of high school.


# interaction age:treat
ggplot(lab,aes(x=employed78, y=age, fill=employed78)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="age vs employed78",
       x="employed?",y="age") + 
  theme_classic() + theme(legend.position="none") + facet_wrap(~treat) #interesting

ggplot(lab,aes(x=employed78, y=re74, fill=employed78)) +
  geom_boxplot() + coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="treat vs employed78",
       x="employed?",y="age") + 
  theme_classic() + theme(legend.position="none") + facet_wrap(~treat) #interesting

# interaction treat:educ
college_lab <- subset(lab, educ == "college and beyond")
highschool_lab <- subset(lab, educ == "high school")
middle_lab <- subset(lab, educ == "middle school")
elementry <- subset(lab, educ == "elementary")


apply(table(college_lab[,c("employed78","treat")])/sum(table(college_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x))
apply(table(highschool_lab[,c("employed78","treat")])/sum(table(highschool_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x))
apply(table(middle_lab[,c("employed78","treat")])/sum(table(middle_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x))
apply(table(elementry[,c("employed78","treat")])/sum(table(elementry[,c("employed78","treat")])),
      2,function(x) x/sum(x))
#From the EDA treat has positive effect on people who at education level elementary or college and beyond 


# interaction treat:black
black_lab <- subset(lab, black == 1)
noblack_lab <- subset(lab, black == 0)

apply(table(black_lab[,c("employed78","treat")])/sum(table(black_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
apply(table(noblack_lab[,c("employed78","treat")])/sum(table(noblack_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
#not significant 

# interaction treat:hisp
hisp_lab <- subset(lab, hispan == 1)
nohisp_lab <- subset(lab, hispan == 0)

apply(table(hisp_lab[,c("employed78","treat")])/sum(table(hisp_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
apply(table(nohisp_lab[,c("employed78","treat")])/sum(table(nohisp_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
#maybe significant 

# interaction treat:married
married_lab <- subset(lab, married == 1)
nomarried_lab <- subset(lab, married == 0)

apply(table(married_lab[,c("employed78","treat")])/sum(table(married_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
apply(table(nomarried_lab[,c("employed78","treat")])/sum(table(nomarried_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
#maybe significant 

# interaction treat:nodegree
degree_lab <- subset(lab, nodegree == 0)
nodegree_lab <- subset(lab, nodegree == 1)

apply(table(degree_lab[,c("employed78","treat")])/sum(table(degree_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
apply(table(nodegree_lab[,c("employed78","treat")])/sum(table(nodegree_lab[,c("employed78","treat")])),
      2,function(x) x/sum(x)) 
#maybe significant 



##################################
# Model fitting
##################################
#let's try a logistic regression that has a main effect for every variable and linear predictors
#first begin by centering the continuous predictor: age

lab$age_c <- lab$age - mean(lab$age)
labreg <- glm(employed78 ~ age_c + re74 + treat + educ + black + hispan + married + nodegree, data = lab, family = binomial)
summary(labreg)



###### Model diagnostics for naive model

#binned residual plots
#save the raw residuals
rawresid1 <- residuals(labreg,"resp")

#binned residual plots
binnedplot(x=fitted(labreg),y=rawresid1,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# a potential quadratic trend

binnedplot(x=lab$age_c,y=rawresid1,xlab="age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

binnedplot(x=lab$re74,y=rawresid1,xlab="re74",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#Model validation

#0.5 threshold 
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(labreg) >= 0.5, "1","0")),
                            as.factor(lab$employed78),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate



#change to mean threshold
mean(lab$employed78_n)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(labreg) >= mean(lab$employed78_n), "1","0")),
                            as.factor(lab$employed78_n),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];#Accuracy: 0.61
Conf_mat$byClass[c("Sensitivity","Specificity")]

#look at ROC curve
roc(lab$employed78_n,fitted(labreg),plot=T,print.thres="best",legacy.axes=T,#AUC:0.66
    print.auc =T,col="red3")


###################################
#full model with all interactions
###################################
labreg1 <- glm(employed78 ~ age_c + treat + educ+ re74 + black + hispan + married + nodegree + age_c:treat + treat:educ + treat:black + treat:hispan + treat:married + treat:nodegree + treat:re74, data = lab, family = binomial)
summary(labreg1) #not very significant
###############################
#model selection (using BIC)
###############################
n <- nrow(lab)
null_model <- glm(employed78~treat,data=lab,family=binomial)
BIC_stepwise_model <- step(null_model,scope=formula(labreg1),direction="both",
     trace=0,k = log(n))
BIC_stepwise_model$call # age_C + re74

BIC_forward_model <- step(null_model,scope=formula(labreg1),direction="forward",
                           trace=0,k = log(n))
BIC_forward_model$call # treat + age_C + re74 + treat:age_c

BIC_backward_model <- step(labreg1,scope=formula(labreg1),direction="backward",
                          trace=0,k = log(n))
BIC_backward_model$call # treat + age_C + re74 + treat:age_c

#test if treat:age_c is significant:
treatagemodel=glm(formula = employed78 ~ treat:age_c + age_c + re74, 
               family = binomial, data = lab)
anova(BIC_stepwise_model, treatagemodel, test= "Chisq")#p:0.0079, very significant

#test if treat is significant:
treatmodel=glm(formula = employed78 ~ treat + age_c + re74, 
               family = binomial, data = lab)
anova(BIC_stepwise_model, treatmodel, test= "Chisq")#p:0.79 not significant, but it is variable of interest. keep it for now.
#since we want to keep both treat:age_c and treat, we use BIC_forward_model.


#test if black is significant:
blackmodel=glm(formula = employed78 ~ treat + age_c + re74 + black + treat:age_c, 
               family = binomial, data = lab)
anova(BIC_forward_model, blackmodel, test= "Chisq") #black is significant, keep for now

#test if hispan is significant:
hispanmodel=glm(formula = employed78 ~ treat + age_c + re74 + hispan + treat:age_c, 
               family = binomial, data = lab)
anova(BIC_forward_model, hispanmodel, test= "Chisq") #hispan is not significant, drop

#test if married is significant:
marriedmodel=glm(formula = employed78 ~ treat + age_c + re74 + married + treat:age_c, 
               family = binomial, data = lab)
anova(BIC_forward_model, marriedmodel, test= "Chisq") #married is not significant, drop

#test if nodegree is significant:
nodegreemodel=glm(formula = employed78 ~ treat + age_c + re74 + nodegree + treat:age_c, 
                 family = binomial, data = lab)
anova(BIC_forward_model, nodegreemodel, test= "Chisq") #nodegree is not significant, drop

# add black to bicforward model

labreg2 = glm(formula = employed78 ~ treat + age_c + re74 + black + treat*age_c, family = binomial, data = lab)
summary(labreg2)


#test if treat*educ is significant:
inter1model = glm(employed78 ~ treat + age_c + black+ re74 + treat*age_c  + treat*educ , family = binomial, 
                  data = lab)
anova(inter1model, labreg2, test= "Chisq")#p:0.066

#test if treat*black is significant:
inter2model = glm(employed78 ~ treat + age_c + black+ re74 + treat*age_c+ treat*black , family = binomial, data = lab)
anova(inter2model, labreg2, test= "Chisq")#p:0.079

#test if treat*hispan is significant:
inter3model = glm(employed78 ~ treat + age_c + black+ re74 +treat*age_c+ treat*hispan , family = binomial, data = lab)
anova(inter3model, labreg2, test= "Chisq")#p:0.12

#test if treat*marrued is significant:
inter4model = glm(employed78 ~ treat + age_c + black+ re74 + treat*age_c+ treat*married , family = binomial, data = lab)
anova(inter4model, labreg2, test= "Chisq")#p:0.47

#test if treat*nodegree is significant:
inter5model = glm(employed78 ~ treat + age_c + black+ re74 + treat*age_c + treat*nodegree , family = binomial, data = lab)
anova(inter5model, labreg2, test= "Chisq")#p:0.90

#test if treat*re74 is significant:
inter6model = glm(employed78 ~ treat + age_c + black+ re74 + treat*age_c + treat*re74 , family = binomial, data = lab)
anova(inter6model, labreg2, test= "Chisq")#p:0.13

# no need to add these interactions

#################################
#Final model
#################################
# labreg2 is our final model

labreg2 = glm(formula = employed78 ~ treat + age_c + re74 + black + treat*age_c, family = binomial, data = lab)
summary(labreg2)
confint(labreg2)



##################################
#Model diagnostics for naive model
##################################
#binned residual plots
rawresid5 <- residuals(labreg2,"resp")
binnedplot(x=fitted(labreg2),y=rawresid5,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
#still looks good

binnedplot(x=lab$age_c,y=rawresid5,xlab="age centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# no concerning pattern, only 1 point outside
binnedplot(x=lab$re74,y=rawresid5,xlab="re74 centered",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")
# no concerning pattern, only 1 point outside
mean(lab$employed78_n)
Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(labreg2) >= mean(mean(lab$employed78_n)), "1","0")),
                            as.factor(lab$employed78_n),positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"];#60%
Conf_mat$byClass[c("Sensitivity","Specificity")]


#look at ROC curve
roc(lab$employed78_n,fitted(labreg2),plot=T,print.thres="best",legacy.axes=T,#AUC:0.60
    print.auc =T,col="red3")# AUC:0.64

vif(labreg2)