library(mice)
library(ggplot2)
library(naniar)
library(VIM)
library(lattice) 
tree <- read.csv("/Users/fengyi111/Desktop/2020-Fall/702/assignment-4/treeage.txt",header = TRUE)

# randomly replace age values in 6 observations with NA
set.seed(123)
missing_index = sample(1:20, 6, replace = TRUE)
tree[missing_index,3] <- NA

# inspect missing patterns
md.pattern(tree)

aggr(tree,col=c("orange","lightblue"),numbers=TRUE,sortVars=TRUE,
                                    labels=names(tree),cex.axis=.7,gap=3, ylab=c("Proportion missing","Missingness pattern"))


# given this small sample size, marginplot may not be very helpful to look at


# imputation
tree_imp <- mice(tree,m=50, defaultMethod=c("norm","logreg","polyreg","polr"),
                    print=F)
stripplot(tree_imp, col=c("grey","darkred"),pch=c(1,20))

# look at diameter vs age 
# the trend is kind of consistent across different  imputations
xyplot(tree_imp, age ~ diameter | .imp,pch=c(1,20),cex = 1.4,col=c("grey","darkred"))# different distribution of age across imputed dataset

d7 <- complete(tree_imp, 7); d7
d17 <- complete(tree_imp, 17); d17

# imputated data have much larger variance than observed data
densityplot(tree_imp)

# fit linear regression model on one of two randomly selected data
treeregd17 <- lm(age~diameter, data = d17) 
summary(treeregd17)

# model diagnostic
# d7
# random pattern, so linearity assumption is satisfied 
# plot(treeregd7$residual,x=d7$diameter,xlab="Diameter",ylab="Residual"); abline(0,0)
plot(treeregd17,which=1:5)

d7$group = "Dataset 7"
d17$group = "Dataset 17"
tree$group = "Original"
df = rbind(d7, d17, tree)

#trend
ggplot(data=df, aes(x=diameter,y=age,color=group)) + 
  geom_point() + 
  geom_smooth(method='lm',level=0) + 
  theme(legend.position = 'bottom')

#overlap
ggplot(data=df, aes(x=age,fill=group)) + 
  geom_density(alpha=0.5) + 
  theme(legend.position = 'bottom')

treereg_imp <- with(data=tree_imp, lm(diameter~age))
treereg_imp[[4]][[7]]
treereg_imp[[4]][[17]]
tree_reg <- pool(treereg_imp)
# the overall estimate of coefficient is close to what we've observed in the two specific dataset
# need interpretation
summary(tree_reg)




## Qeustion 2 
nhanes <- read.csv("/Users/fengyi111/Desktop/2020-Fall/702/assignment-4/nhanes.csv",header = TRUE, na.strings = c('.',NA))
nhanes = nhanes[, !names(nhanes) %in% c('wtmec2yr','sdmvstra','sdmvpsu','ridageyr')]

nhanes$riagendr <- factor(nhanes$riagendr)
nhanes$ridreth2 <- factor(nhanes$ridreth2)
nhanes$dmdeduc <- factor(nhanes$dmdeduc)
nhanes$indfminc<- factor(nhanes$indfminc)
summary(nhanes)
nhanes_imp <- mice(nhanes,m=10, defaultMethod=c("pmm","logreg","polyreg","polr"),
                 print=F)

# select two complete datasets 
n3 <- complete(nhanes_imp, 3);
n10 <- complete(nhanes_imp, 10); 
# bmi by age
xyplot(nhanes_imp, bmxbmi ~ age | .imp,pch=c(1,20),cex = 1.4,col=c("grey","darkred"))# different distribution of age across imputed dataset
# bmi by gender 
stripplot(nhanes_imp, bmxbmi~.imp|riagendr, col=c("grey","darkred"),pch=c(1,20))

stripplot(nhanes_imp, col=c("grey","darkred"),pch=c(1,20))


n3$group = "Dataset 3"
n10$group = "Dataset 10"
nhanes$group = "Original"
df_nhanes = rbind(n3, n10, nhanes)

#trend
ggplot(data=df_nhanes, aes(x=age,y=bmxbmi,color=group)) + 
  #geom_point() + 
  geom_smooth(method='lm',level=0) + 
  theme(legend.position = 'bottom')

ggplot(data=df_nhanes, aes(x=riagendr,y=bmxbmi,color=group)) + 
  geom_boxplot() + 
  #geom_smooth(method='lm',level=0) + 
  theme(legend.position = 'bottom')

#overlap
ggplot(data=df_nhanes, aes(x=age,fill=group)) + 
  geom_density(alpha=0.5) + 
  theme(legend.position = 'bottom')

# fit model 
nhanes_reg <- lm(bmxbmi~age+riagendr+ridreth2+ dmdeduc + indfminc + dmdeduc:riagendr,data = n3)
plot(nhanes_reg,which = 1:5)
# assumptions violated, going to transform data 


nhanes_log_reg <- lm(log(bmxbmi)~age+riagendr+ridreth2 + dmdeduc + indfminc + dmdeduc:riagendr,data = n3)
plot(nhanes_log_reg,which = 1:5)

model_backward <- step(nhanes_log,direciton='backward',trace=0)
model_backward$call


nhanes_logged <- read.csv("/Users/fengyi111/Desktop/2020-Fall/702/assignment-4/nhanes.csv",header = TRUE, na.strings = c('.',NA))
nhanes_logged = nhanes_logged[, !names(nhanes_logged) %in% c('wtmec2yr','sdmvstra','sdmvpsu','ridageyr')]

nhanes_logged$riagendr <- factor(nhanes_logged$riagendr)
nhanes_logged$ridreth2 <- factor(nhanes_logged$ridreth2)
nhanes_logged$dmdeduc <- factor(nhanes_logged$dmdeduc)
nhanes_logged$indfminc<- factor(nhanes_logged$indfminc)
nhanes_logged$bmxbmi <- log(nhanes_logged$bmxbmi)
nhanes_imp_log <- mice(nhanes_logged,m=10, defaultMethod=c("pmm","logreg","polyreg","polr"),
                   print=F)


log3 <- complete(nhanes_imp_log, 3);

nhanesreg_imp <- with(data=nhanes_imp_log, lm(bmxbmi~age+riagendr+ridreth2 + dmdeduc + indfminc + dmdeduc:riagendr))

nhanes_overall <- pool(nhanesreg_imp)

summary(nhanes_overall)