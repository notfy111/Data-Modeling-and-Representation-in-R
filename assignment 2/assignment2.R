## Question 1 ###
geyser <- read.csv('Desktop/2020-Fall/702/702-Assignment2/OldFaithful.csv')
head(geyser)

# Part a
reg_model <- lm(Duration~Interval,data=geyser)
summary(reg_model)
head(geyser)

babies <- read.csv('Desktop/2020-Fall/702/702-Assignment2/babiesdata.csv')
head(babies)
smoking <-read.csv('Desktop/2020-Fall/702/702-Assignment2/smoking.csv')
head(smoking)
bi_smoking <-smoking['smoke']
head(bi_smoking)
smoking$med_asFactor <- factor(smoking$med)
smoking$mrace_asFactor <-factor(smoking$mrace)
smoking$parity_asFactor <- factor(smoking$parity)
#ggplot(beer,aes(x=temp_median_c,y=beer_cons_liters))+ geom_point()+ facet_wrap(~rain,ncol=3)+geom_smooth(method='lm',col='red3')


# EDA
# birth weight follows a normal distributon. 
hist(smoking$bwt.oz)
# Using boxplot, we see that the distribution of birth weight between smoker and non-smoker is different. Thus we can do further investigation. 
ggplot(smoking,aes(x=smoke,y=bwt.oz))+ geom_boxplot()
# mrace and bwt
ggplot(smoking,aes(x=mrace_asFactor,y=bwt.oz))+ geom_boxplot()
# parity and bwt (high parity have significantly less birth weight)
ggplot(smoking,aes(x=parity_asFactor,y=bwt.oz))+ geom_boxplot()
# mage and bwt
ggplot(smoking,aes(x=mage,y=bwt.oz))+ geom_point()
# mht and bwt
ggplot(smoking,aes(x=mht,y=bwt.oz))+ geom_point()
# mpregwt and bwt
ggplot(smoking,aes(x=mpregwt,y=bwt.oz))+ geom_point()
# med and bwt
ggplot(smoking,aes(x=med_asFactor,y=bwt.oz))+ geom_boxplot()



# Check  race as an interaction term
ggplot(smoking,aes(x=smoke,y=bwt.oz,group=smoke))+ geom_boxplot()+ facet_wrap(~mrace_asFactor,ncol=6)
# Check education as an interaction term
ggplot(smoking,aes(x=smoke,y=bwt.oz,group=smoke))+ geom_boxplot()+ facet_wrap(~med_asFactor)
# Check age and birthweight (pretty random), no significnat interaction
ggplot(smoking,aes(x=smoke,y=bwt.oz,group=smoke))+geom_boxplot()+facet_wrap(~parity_asFactor)



# fit a linear model
full_Model <- lm(bwt.oz~smoke*mrace_asFactor+smoke*parity_asFactor+mage+mht+mpregwt+smoke*med_asFactor,data = smoking)
summary(full_Model)

model_backward <- step(full_Model,direciton='backward',trace=0)
model_backward$call

# Final model
final_Model <-lm(bwt.oz~smoke*mrace_asFactor+parity_asFactor+mht+mpregwt,data = smoking)
final_wo_inter <-lm(bwt.oz~smoke+mrace_asFactor+parity_asFactor+mht+mpregwt,data = smoking)
summary(final_Model)
# Final model w/wo interaction
anova(full_Model,final_Model)
anova(final_Model,final_wo_inter)

# check model assumptions
ggplot(smoking, aes(x = smoke, y = final_Model$residuals)) + geom_point() + geom_hline(yintercept = 0, col = 'red3')
ggplot(smoking, aes(x = mrace_asFactor, y = final_Model$residuals)) + geom_point() + geom_hline(yintercept = 0, col = 'red3')
ggplot(smoking, aes(x = parity_asFactor, y = final_Model$residuals)) + geom_point() + geom_hline(yintercept = 0, col = 'red3')
ggplot(smoking, aes(x = mht, y = final_Model$residuals)) + geom_point() + geom_hline(yintercept = 0, col = 'red3')
ggplot(smoking, aes(x = mpregwt, y = final_Model$residuals)) + geom_point() + geom_hline(yintercept = 0, col = 'red3')


plot(final_Model,which=1:5)

#
