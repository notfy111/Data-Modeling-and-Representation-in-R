# Load library
library(dplyr)
library(ggplot2)
library(lme4)
library(GGally)
library(pROC)
library(car)
library(arm)
library(caret)
# Read data
registered <- read.table('/Users/fengyi111/Desktop/2020-Fall/702/TeamProject2/voter_stats_20161108.txt', header = TRUE) # 514848 observations, 11 variables 
voter <- read.delim('/Users/fengyi111/Desktop/2020-Fall/702/TeamProject2/history_stats_20161108.txt', header = TRUE, sep = '\t') # 734126 observations, 15 variables


### Data Wrangling ###

## Step 1 
# check if there are any duplicated rows
any(duplicated(registered)) # false
any(duplicated(voter)) # fase

## Step 2
# Get rid of irrelevant variables
voter = voter[, !names(voter) %in% c('election_date','precinct_abbrv', 'stats_type','update_date','voting_method','voting_method_desc','party_cd')] # 8 variables 
registered = registered[, !names(registered) %in% c('election_date','precinct_abbrv','stats_type')] # 8 variables

## Step 3
# 3.1 check nan in voter, all 0
sum(is.na(voter$county_desc))
sum(is.na(voter$voted_party_cd))
sum(is.na(voter$county_desc))
sum(is.na(voter$age))
sum(is.na(voter$race_code))
sum(is.na(voter$ethnic_code))
sum(is.na(voter$sex_code))
sum(is.na(voter$total_voters))
sum(is.na(voter$vtd_abbrv))
# 3.2 check nan in registered, all 0
sum(is.na(registered$county_desc))
sum(is.na(registered$party_cd))
sum(is.na(registered$age))
sum(is.na(registered$race_code))
sum(is.na(registered$ethnic_code))
sum(is.na(registered$sex_code))
sum(is.na(registered$total_voters))
sum(is.na(registered$vtd_abbrv))

## Step 4
# 4.1 check empty string or space
voter_empty <- voter[apply(voter,1,function(x) any(x == ' '| x == '')),] # 30031 observations
registered_empty <- registered[apply(registered,1,function(x) any(x == ' '| x == '')),] # 1140 observations
# 4.2 drop observations with empty value
voter_clean <- voter[!apply(voter,1,function(x) any(x == ' '| x == '')),]  # 704095 observations
registered_clean <- registered[!apply(registered,1,function(x) any(x == ' '| x == '')),] # 513708 observations 
# 4.3 drop levels where there are no values after cleaning the empty values
voter_clean = droplevels(voter_clean)
registered_clean = droplevels(registered_clean)


## Step 5
# still difference in levels in varialbes: age, sex_code, voted_party_cd, race_code
str(voter_clean)
str(registered_clean)

# 5.1
# drop age < 18 or invalid birth dates in voter_clean because people under 18 cannot legally vote anyways
under_18 = voter_clean[(voter_clean$age == 'Age < 18 Or Invalid Birth Dates'),]
voter_clean = voter_clean[!(voter_clean$age == 'Age < 18 Or Invalid Birth Dates'),] # drop 1 observation
voter_clean = droplevels(voter_clean)
levels(voter_clean$age) # age looks good now


## 5.2 
# drop sex _ code = N in voter_clean
sex_N = voter_clean[(voter_clean$sex_code == 'N'),]
voter_clean = voter_clean[!(voter_clean$sex_code == 'N'),] # drop 1 observation
voter_clean = droplevels(voter_clean)
levels(voter_clean$sex_code) # sex_code looks good now


## Step 6
# reanme the total_voters column to avoid ambiguity
names(voter_clean)[names(voter_clean) == "total_voters"] <- "actual_voters"
names(voter_clean)[names(voter_clean) == "voted_party_cd"] <- "party_cd"
str(voter_clean)

## Step 7
# 7.1 aggregate all votoer numbers by the existing 7 variables (exlucding actual_voter and total voter)
voter_aggregate <- aggregate(voter_clean$actual_voters,
                             list(County=voter_clean$county_desc, Vtd_abbrv=voter_clean$vtd_abbrv, 
                                  Age=voter_clean$age,Party=voter_clean$party_cd, Race=voter_clean$race_code,
                                  Sex=voter_clean$sex_code, Ethnicity=voter_clean$ethnic_code),sum)
register_aggregate <- aggregate(registered_clean$total_voters,
                             list(County=registered_clean$county_desc, Vtd_abbrv=registered_clean$vtd_abbrv, 
                                  Age=registered_clean$age,Party=registered_clean$party_cd, Race=registered_clean$race_code,
                                  Sex=registered_clean$sex_code, Ethnicity=registered_clean$ethnic_code),sum)

                     
df_full <- inner_join(voter_aggregate, register_aggregate, by= c('County'='County','Vtd_abbrv'='Vtd_abbrv','Age'='Age',
                                                      'Party'='Party','Race'='Race','Sex'='Sex','Ethnicity'='Ethnicity')) # 384822 observations

# 7.2 change column names to make them more informative
names(df_full)[names(df_full) == "x.x"] <- "actual_voters"
names(df_full)[names(df_full) == "x.y"] <- "registered_voters"

# 7.3 check if all registered voters value are larger or equal to actual voters
nrow(df_full[(df_full$registered_voters < df_full$actual_voters),]) 
# turns out that there are 3238 observations where there are more actual voters than registered voters.

# 7.4 drop these observations at this moment but should mention in report about the problem of gerrymandering
df_full <- df_full[!(df_full$registered_voters < df_full$actual_voters),] # 381584 observations



## Step 8
# select 20 counties and subset the dataframe
set.seed(123)
county_names = unique(df_full$County) # need to include all these names in the report
county_list = as.character(sample(county_names,20,replace=FALSE))
df_subset <- df_full[df_full$County %in% county_list,] # 96920 observations





########## Zheng Pian Kai Shi ###########



#### EDA ####
#############

### Main Effect ### 
df_subset = droplevels(df_subset) # drop unused county levels and other variable levels

# Look for traces of imbalance data distribution
table(df_subset$County) # significant difference in # of observations across counties
table(df_subset$County,df_subset$Age) # again, some counties have much more observations in age group data than others. But almost every county has a considerable amount of data in each age group (>= 100)
table(df_subset$County,df_subset$Party) # Some counties have very few party representatives -- 8 or 9
table(df_subset$County,df_subset$Sex) # McDowell has 0 unspecificed sex observation
table(df_subset$County,df_subset$Ethnicity) # Some counties have very few hispanic/lantino register population (e.g., 18)
table(df_subset$County,df_subset$Race) # Again, some ethnicities are under-represented in some counties

df_subset['ratio'] = df_subset['actual_voters'] / df_subset['registered_voters']


## County ##
aggregated_county_vote <- aggregate(df_subset$actual_voters,
                                 list(County=df_subset$County),sum)
aggregated_county_total <- aggregate(df_subset$registered_voters,
                                  list(County=df_subset$County),sum)
aggregated_county_ratio <- setNames(aggregated_county_total %>% 
  mutate(ratio = aggregated_county_vote$x/aggregated_county_total$x),c('County','TotalVoter','Ratio'))

# patterns of turnout rates do not vary significantly across counties.
# however there is a county taht has a large number of total voters with a very turnout ratio. Might be worth further investigation
ggplot(aggregated_county_ratio,aes(x=County,y=Ratio)) + geom_point(colour='orange',aes(size=TotalVoter)) 

ggplot(df_subset,aes(x=County, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="County vs Turnout Ratio",
       x="County",y="Ratio") +
  theme_classic() + theme(legend.position = "none")





### Sex ###
aggregated_sex_vote <- aggregate(df_subset$actual_voters,
                             list(Gender=df_subset$Sex),sum)
aggregated_sex_total <- aggregate(df_subset$registered_voters,
                            list(Gender=df_subset$Sex),sum)
aggregated_sex_ratio <- setNames(aggregated_sex_total %>% 
  mutate(ratio = aggregated_sex_vote$x/aggregated_sex_total$x),c('Gender','TotalVoter','Ratio'))

# very clear trend across genders, with female leding in turnout rates and male in the middle, unspecified lowest (might due to insufficient data)
ggplot(aggregated_sex_ratio,aes(x=Gender,y=Ratio)) + geom_point(colour='orange',aes(size=TotalVoter)) + theme_classic()


ggplot(df_subset,aes(x=Sex, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Sex vs Turnout Ratio",
       x="Sex",y="Ratio") +
  theme_classic() + theme(legend.position = "none")



## Age ##
aggregated_age_vote <- aggregate(df_subset$actual_voters,
                                 list(Age=df_subset$Age),sum)
aggregated_age_total <- aggregate(df_subset$registered_voters,
                                  list(Age=df_subset$Age),sum)
aggregated_age_ratio <- setNames(aggregated_age_total %>% 
  mutate(ratio = aggregated_age_vote$x/aggregated_age_total$x),c('Age','TotalVoter','Ratio'))

# very clear positive trend, with older people having higher turnout rate
ggplot(aggregated_age_ratio,aes(x=Age,y=Ratio)) + geom_point(colour='orange',aes(size=TotalVoter)) +  theme_classic()

ggplot(df_subset,aes(x=Age, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Age vs Turnout Ratio",
       x="Age",y="Ratio") +
  theme_classic() + theme(legend.position = "none")



## Race ##
aggregated_race_vote <- aggregate(df_subset$actual_voters,
                                 list(Race=df_subset$Race),sum)
aggregated_race_total <- aggregate(df_subset$registered_voters,
                                  list(Race=df_subset$Race),sum)
aggregated_race_ratio <- setNames(aggregated_race_total %>% 
                                   mutate(Ratio = aggregated_race_vote$x/aggregated_race_total$x),c('Race','TotalVoter','Ratio'))

# Race A and M (two or more races) have significantly higher rates, but also smaller total voter population. 
ggplot(aggregated_race_ratio,aes(x=Race,y=Ratio)) + geom_point(colour='orange',aes(size=TotalVoter)) +  theme_classic()

ggplot(df_subset,aes(x=Race, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Race vs Turnout Ratio",
       x="Race",y="Ratio") +
  theme_classic() + theme(legend.position = "none")

## Ethnicity ##
aggregated_ethn_vote <- aggregate(df_subset$actual_voters,
                                  list(Ethnicity=df_subset$Ethnicity),sum)
aggregated_ethn_total <- aggregate(df_subset$registered_voters,
                                   list(Ethnicity=df_subset$Ethnicity),sum)
aggregated_ethn_ratio <- setNames(aggregated_ethn_total %>% 
                                    mutate(Ratio = aggregated_ethn_vote$x/aggregated_ethn_total$x),c('Ethnicity','TotalVoter','Ratio'))

# HL (Hispanic or Latino) has the highest turnout rates, not Hispanic or Latino second, and undesignated is the lowest)
ggplot(aggregated_ethn_ratio,aes(x=Ethnicity,y=Ratio)) + geom_point(colour='orange',aes(size=TotalVoter)) +  theme_classic()

ggplot(df_subset,aes(x=Ethnicity, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Ethnicity vs Turnout Ratio",
       x="Ethnicity",y="Ratio") +
  theme_classic() + theme(legend.position = "none")


## Party ##
aggregated_party_vote <- aggregate(df_subset$actual_voters,
                                  list(Party=df_subset$Party),sum)
aggregated_party_total <- aggregate(df_subset$registered_voters,
                                   list(Party=df_subset$Party),sum)
aggregated_party_ratio <- setNames(aggregated_party_total %>% 
                                    mutate(Ratio = aggregated_party_vote$x/aggregated_party_total$x),c('Party','TotalVoter','Ratio'))

# Liberal has the smallest total voter population yet highest turnout rates, second is republican, and democrats third.
# Unaffiliated has the lowest turnout rates
ggplot(aggregated_party_ratio,aes(x=Party,y=Ratio)) + geom_point(colour='orange',aes(size=TotalVoter)) +  theme_classic()

ggplot(df_subset,aes(x=Party, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Party vs Turnout Ratio",
       x="Ethnicity",y="Ratio") +
  theme_classic() + theme(legend.position = "none")

### Interaction ###

# County, Age, Gender,Party, Ethnicity, Race

## County by Age ##
# Might not be significant: across states older people have higher turnout ratio than younger people
aggregated_ctByage_vote <- aggregate(df_subset$actual_voters,
                                     list(County=df_subset$County,Age=df_subset$Age),sum)
aggregated_ctByage_total <- aggregate(df_subset$registered_voters,
                                      list(County=df_subset$County,Age=df_subset$Age),sum)
aggregated_ctByage_ratio <- setNames(aggregated_ctByage_total %>% 
                                       mutate(Ratio = aggregated_ctByage_vote$x/aggregated_ctByage_total$x),c('County','Age','TotalVoter','Ratio'))
ggplot(aggregated_ctByage_ratio,aes(x=County,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Age)) + theme_classic() 

# might not be significant
ggplot(df_subset,aes(x=Age, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Age : County",
       x="Age",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~County)


## County by Gender ##
# Might not be significant
aggregated_ctBysex_vote <- aggregate(df_subset$actual_voters,
                                       list(County=df_subset$County,Sex=df_subset$Sex),sum)
aggregated_ctBysex_total <- aggregate(df_subset$registered_voters,
                                        list(County=df_subset$County,Sex=df_subset$Sex),sum)
aggregated_ctBysex_ratio <- setNames(aggregated_ctBysex_total %>% 
                                         mutate(Ratio = aggregated_ctBysex_vote$x/aggregated_ctBysex_total$x),c('County','Sex','TotalVoter','Ratio'))
ggplot(aggregated_ctBysex_ratio,aes(x=County,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Sex)) + theme_classic() 

# Might not be significant
ggplot(df_subset,aes(x=Sex, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Sex : County",
       x="Sex",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~County)




## County by Party ##
# Hard to tell, maybe
aggregated_ctByparty_vote <- aggregate(df_subset$actual_voters,
                                     list(County=df_subset$County,Party=df_subset$Party),sum)
aggregated_ctByparty_total <- aggregate(df_subset$registered_voters,
                                      list(County=df_subset$County,Party=df_subset$Party),sum)
aggregated_ctByparty_ratio <- setNames(aggregated_ctByparty_total %>% 
                                       mutate(Ratio = aggregated_ctByparty_vote$x/aggregated_ctByparty_total$x),c('County','Party','TotalVoter','Ratio'))
ggplot(aggregated_ctByparty_ratio,aes(x=County,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Party)) + theme_classic() 

# might not be significant
ggplot(df_subset,aes(x=Party, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Party : County",
       x="Party",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~County)


## County by Ethnicity ##
# might be significant
# Unidentified has disproportionately lower turnout rates in some countries than others
aggregated_ctByeth_vote <- aggregate(df_subset$actual_voters,
                                       list(County=df_subset$County,Ethnicity=df_subset$Ethnicity),sum)
aggregated_ctByeth_total <- aggregate(df_subset$registered_voters,
                                       list(County=df_subset$County,Ethnicity=df_subset$Ethnicity),sum)
aggregated_ctByeth_ratio <- setNames(aggregated_ctByeth_total %>% 
                                        mutate(Ratio = aggregated_ctByeth_vote$x/aggregated_ctByeth_total$x),c('County','Ethnicity','TotalVoter','Ratio'))
ggplot(aggregated_ctByeth_ratio,aes(x=County,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Ethnicity)) + theme_classic() 
# might not be significant
ggplot(df_subset,aes(x=Ethnicity, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Ethnicity : County",
       x="Ethnicity",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~County)



## County by Race ##
# might be significant
# Asian has disproportionately lower turnout rates in some countries than others
aggregated_ctByrace_vote <- aggregate(df_subset$actual_voters,
                                          list(County=df_subset$County,Race=df_subset$Race),sum)
aggregated_ctByrace_total <- aggregate(df_subset$registered_voters,
                                          list(County=df_subset$County,Race=df_subset$Race),sum)
aggregated_ctByrace_ratio <- setNames(aggregated_ctByrace_total %>% 
                                           mutate(Ratio = aggregated_ctByrace_vote$x/aggregated_ctByrace_total$x),c('County','Race','TotalVoter','Ratio'))
ggplot(aggregated_ctByrace_ratio,aes(x=County,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Race)) + theme_classic() 

ggplot(df_subset,aes(x=Race, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Race : County",
       x="Race",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~County)



## We see that trends between turnout ratio and predictor variables vary across counties. Thus, it is necessary to take advantage of the natual county-level grouping and do a multilevel regression with 
# counties having varying intercepts. 

## Age by Sex ##
# might be significant

aggregated_ageBygender_vote <- aggregate(df_subset$actual_voters,
                                 list(Age=df_subset$Age,Gender=df_subset$Sex),sum)
aggregated_ageBygender_total <- aggregate(df_subset$registered_voters,
                                  list(Age=df_subset$Age,Gender=df_subset$Sex),sum)
aggregated_ageBygender_ratio <- setNames(aggregated_ageBygender_total %>% 
  mutate(Ratio = aggregated_ageBygender_vote$x/aggregated_ageBygender_total$x),c('Age','Sex','TotalVoter','Ratio'))

# different patterns across genders in different age groups: female has the highest turnout rates in the first three levels of age groups but lowest in the over 66 group.
ggplot(aggregated_ageBygender_ratio,aes(x=Sex,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Age,shape=Age)) + theme_classic()

# might be ? use anova later 
ggplot(df_subset,aes(x=Sex, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Sex by Age",
       x="Awz",y="Age") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Age)





## Age by Party ##

aggregated_ageByparty_vote <- aggregate(df_subset$actual_voters,
                                          list(Age=df_subset$Age,Party=df_subset$Party),sum)
aggregated_ageByparty_total <- aggregate(df_subset$registered_voters,
                                          list(Age=df_subset$Age,Party=df_subset$Party),sum)
aggregated_ageByparty_ratio <- setNames(aggregated_ageByparty_total %>% 
                                           mutate(Ratio = aggregated_ageByparty_vote$x/aggregated_ageByparty_total$x),c('Age','Party','TotalVoter','Ratio'))

# trend does not vary much between parties across age groups. 
ggplot(aggregated_ageByparty_ratio,aes(x=Age,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Party,shape=Party)) + theme_classic()


ggplot(df_subset,aes(x=Age, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Age : Party",
       x="Age",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Party)

## Age by Ethnicity ##

aggregated_ageByeth_vote <- aggregate(df_subset$actual_voters,
                                         list(Age=df_subset$Age,Ethnicity=df_subset$Ethnicity),sum)
aggregated_ageByeth_total <- aggregate(df_subset$registered_voters,
                                         list(Age=df_subset$Age,Ethnicity=df_subset$Ethnicity),sum)
aggregated_ageByeth_ratio <- setNames(aggregated_ageByeth_total %>% 
                                          mutate(Ratio = aggregated_ageByeth_vote$x/aggregated_ageByeth_total$x),c('Age','Ethnicity','TotalVoter','Ratio'))

# trend is different: difference across ethnicity groups in age group 41-64 are significantly less than that in other age groups
ggplot(aggregated_ageByeth_ratio,aes(x=Age,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Ethnicity,shape=Ethnicity)) + theme_classic()

# trend not clear
ggplot(df_subset,aes(x=Age, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Age : Ethnicity",
       x="Age",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Ethnicity)


## Age by Race ##

aggregated_ageByrace_vote <- aggregate(df_subset$actual_voters,
                                       list(Age=df_subset$Age,Race=df_subset$Race),sum)
aggregated_ageByrace_total <- aggregate(df_subset$registered_voters,
                                       list(Age=df_subset$Age,Race=df_subset$Race),sum)
aggregated_ageByrace_ratio <- setNames(aggregated_ageByrace_total %>% 
                                        mutate(Ratio = aggregated_ageByrace_vote$x/aggregated_ageByrace_total$x),c('Age','Race','TotalVoter','Ratio'))

# trend is different
ggplot(aggregated_ageByrace_ratio,aes(x=Age,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Race)) + theme_classic()
# might be interaction 
ggplot(df_subset,aes(x=Age, y=ratio, fill = 'Orange')) +
  geom_boxplot() + #coord_flip() +
  labs(title="Age : Race",
       x="Age",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Race)

## Sex by Party ##

aggregated_sexByparty_vote <- aggregate(df_subset$actual_voters,
                                         list(Sex=df_subset$Sex,Party=df_subset$Party),sum)
aggregated_sexByparty_total <- aggregate(df_subset$registered_voters,
                                         list(Sex=df_subset$Sex,Party=df_subset$Party),sum)
aggregated_sexByparty_ratio <- setNames(aggregated_sexByparty_total %>% 
                                          mutate(Ratio = aggregated_sexByparty_vote$x/aggregated_sexByparty_total$x),c('Sex','Party','TotalVoter','Ratio'))

# trend slightly varies across different parties -- female republicans are female democats have similar turnout rates, while for men, republicans have a higher turnout rates.
# turnout rates for unaffiliated people decrease across genders
ggplot(aggregated_sexByparty_ratio,aes(x=Sex,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Party,shape=Party)) + theme_classic()

ggplot(df_subset,aes(x=Sex, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Sex : Party",
       x="Sex",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Party)





## Sex by Ethnicity ##

aggregated_sexByeth_vote <- aggregate(df_subset$actual_voters,
                                        list(Sex=df_subset$Sex,Ethnicity=df_subset$Ethnicity),sum)
aggregated_sexByeth_total <- aggregate(df_subset$registered_voters,
                                         list(Sex=df_subset$Sex,Ethnicity=df_subset$Ethnicity),sum)
aggregated_sexByeth_ratio <- setNames(aggregated_sexByeth_total %>% 
                                          mutate(Ratio = aggregated_sexByeth_vote$x/aggregated_sexByeth_total$x),c('Sex','Ethnicity','TotalVoter','Ratio'))

# very different trend, especially for unspecified sex category. Might be interaction
# however can also due to insufficient data. 
ggplot(aggregated_sexByeth_ratio,aes(x=Sex,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Ethnicity,shape=Ethnicity)) + theme_classic()

# not apparent
ggplot(df_subset,aes(x=Sex, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Sex : Ethnicity",
       x="Sex",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Ethnicity)




## Sex by Race ##

aggregated_sexByrace_vote <- aggregate(df_subset$actual_voters,
                                      list(Sex=df_subset$Sex,Race=df_subset$Race),sum)
aggregated_sexByrace_total <- aggregate(df_subset$registered_voters,
                                       list(Sex=df_subset$Sex,Race=df_subset$Race),sum)
aggregated_sexByrace_ratio <- setNames(aggregated_sexByrace_total %>% 
                                        mutate(Ratio = aggregated_sexByrace_vote$x/aggregated_sexByrace_total$x),c('Sex','Race','TotalVoter','Ratio'))

# slightly different trend, especially for unspecified sex category. 
# however can also due to insufficient data
ggplot(aggregated_sexByrace_ratio,aes(x=Sex,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Race)) + theme_classic()

# Might be significant
ggplot(df_subset,aes(x=Sex, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Sex : Race",
       x="Sex",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Race)




## Party by Ethnicity ##

aggregated_partyByeth_vote <- aggregate(df_subset$actual_voters,
                                         list(Party=df_subset$Party,Ethnicity=df_subset$Ethnicity),sum)
aggregated_partyByeth_total <- aggregate(df_subset$registered_voters,
                                          list(Party=df_subset$Party,Ethnicity=df_subset$Ethnicity),sum)
aggregated_partyByeth_ratio <- setNames(aggregated_partyByeth_total %>% 
                                           mutate(Ratio = aggregated_partyByeth_vote$x/aggregated_partyByeth_total$x),c('Party','Ethnicity','TotalVoter','Ratio'))

# trend between ethnicity groups across party affiliations vary: For democrats, non-hispanic has the highest turnout rates; for liberals, non-hispanic has the lowest turnout rates
ggplot(aggregated_partyByeth_ratio,aes(x=Party,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Ethnicity,shape=Ethnicity)) + theme_classic()

# might be worth exploring
ggplot(df_subset,aes(x=Ethnicity, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Ethnicity : Party",
       x="Ethnicity",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Party)






## Party by Race ##

aggregated_partyByrace_vote <- aggregate(df_subset$actual_voters,
                                        list(Party=df_subset$Party,Race=df_subset$Race),sum)
ç
aggregated_partyByrace_ratio <- setNames(aggregated_partyByrace_total %>% 
                                          mutate(Ratio = aggregated_partyByrace_vote$x/aggregated_partyByrace_total$x),c('Party','Race','TotalVoter','Ratio'))

# trend does not seem to vary a lot
ggplot(aggregated_partyByrace_ratio,aes(x=Party,y=Ratio,size=TotalVoter)) + geom_point(aes(color=Race)) + theme_classic()

# worth exploring
ggplot(df_subset,aes(x=Race, y=ratio)) +
  geom_boxplot() + #coord_flip() +
  labs(title="Race : Pary",
       x="Race",y="Ratio") +
  theme_classic() + theme(legend.position = "none") + facet_wrap(~Party)






## Race by Ethnicity may not be meaningful ##








#### Model Selection ####
#########################

# sex, ethnicity, .... a bunch of interactions that we deemed significant 
# null model includes all demographics predictors
# full model includes null model as well other predictos, 
# run aic or bic
# use anova to test interactions.
# produce final model and interpret



# Model 
# all
# model 1 = (1|sex) + b + c 
# model 2 = a + (1|b) + c 
# model 3 = a + b + (1|c)

# In order to create a binomial distribution to fit the model, we create another variable: non_voter
df_subset['non_voters'] = df_subset['registered_voters'] - df_subset['actual_voters']
# multiple variables, try random intercepts or random slopes based on AIC BIC (based on research question)
# Use anova to test trimmed terms one by one
# other variables of interest 
set.seed(111)
# Fail to converge
model_v <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity + Sex:Party + Party:Race + Party:Ethnicity +  (1|County) , family =  binomial (link='logit'), data = df_subset)

summary(model_v)


model_full <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + Party:Race  +  (1|County) , family =  binomial (link='logit'), data = df_subset)

summary(model_full)

# Converged
model_noInt <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  +  (1|County) , family =  binomial (link='logit'), data = df_subset)

summary(model_noInt)

# Converged
model_1Int <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  +  Sex:Party + (1|County) , family =  binomial (link='logit'), data = df_subset)

summary(model_1Int)



# Try to add another interaction term : Age:Race
# Fatal error
model_anotherInt <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + Age:Race + (1|Party:Race) + (1|County) , family =  binomial (link='logit'), data = df_subset)

# Try to add Age:Race as varying effect
# Fail to converge
model_AR <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + (1|Age:Race) + (1|Party:Race) + (1|County) , family =  binomial (link='logit'), data = df_subset)

# Try to add Age: Sex as interaction
# Runs fine - 10 mins to run
# Prob the fullest possible model 
model_AG <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + Age:Sex + (1|Party:Race) + (1|County) , family =  binomial (link='logit'), data = df_subset)
summary(model_AG)
#mode_try <- glmer(cbind(actual_voters,non_voters) ~ Race + Party+ Party:Race + (1|County) , family =  binomial (link='logit'), data = df_subset,verbose = 1)
# Backward model selection - does not work on multilevel model
#n <- nrow(df_subset)
#model_te <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + Age:Sex +(1|Party:Race) + (1|County), family =  binomial (link='logit'), data = df_subset)
#BIC_backward_model <- step(model_noInt,direction="backward", trace=0,k = log(n))









## null model which includes only variables of primary interests 
null_model <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + (1|County) , family =  binomial (link='logit'), data = df_subset)
# model with no interaction terms at all
model_noInt <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  +  (1|County) , family =  binomial (link='logit'), data = df_subset)

# anova test on sex:party
# p < .05, significant
anova(model_noInt,null_model)

# anova test on party:race
# first failed to converge. Might be that there are too many values for actual = 1 or 0 in certian combinations 
model_2Int <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  +  Sex:Party + Party:Race + (1|County) , family =  binomial (link='logit'), data = df_subset)
# turns out it is the case where Liberal American Indian/Alaskan Native have 100% turnout ratio
aggregated_partyByrace_ratio
# Try to make that interaction as varying effect - runs fine
# p < .05, significant
model_randInt <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  +  Sex:Party + (1|Party:Race) + (1|County) , family =  binomial (link='logit'), data = df_subset)
anova(null_model,model_randInt)



# anova test on sex:race
# p < .05
model_wSexRace <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + Sex:Race + (1|County) , family =  binomial (link='logit'), data = df_subset)
anova(model_wSexRace,null_model)


# anova test on sex: age
# p < .05 
model_wAgeSex <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Ethnicity  + Sex:Party + Sex:Age + (1|County) , family =  binomial (link='logit'), data = df_subset)
anova(model_wAgeSex,null_model)


# age : race
# p < .05 
model_wAgeRace <- glmer(cbind(actual_voters,non_voters) ~ Race + Party + Sex + Age + Ethnicity  + Sex:Party + Age:Race+ (1|County) , family =  binomial (link='logit'), data = df_subset)
anova(model_wAgeRace,null_model)


## Given our research interest, we will choose our model_wAgeRace as our final model

final_model <- model_wAgeRace

summary(final_model)
# visualiation of random effect

 

## Model Asessement ##


resid <- residuals(final_model,"resp")
binnedplot(x=fitted(final_model),y=resid,xlab="Pred. probabilities",
           col.int="red4",ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

# overall mean turnout ratio
overall_voter <- sum(df_subset['actual_voters'])
overall_register <- sum(df_subset['registered_voters'])
overall_ratio <- overall_voter / overall_register  

dotplot(ranef(final_model, condVar=TRUE))$County


Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(final_model) >= overall_ratio, "1","0")),
                            as.factor(df_full$actual_voters),positive = "1")



Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(labreg2) >= mean(mean(lab$employed78_n)), "1","0")),
                            as.factor(lab$employed78_n),positive = "1")

Conf_mat$table
Conf_mat$overall["Accuracy"];#60%
Conf_mat$byClass[c("Sensitivity","Specificity")]



aggregated_party <- aggregate(df_subset$non_voters,
                                         list(Party=df_subset$Party),sum)


## Linearity
## Independence of errors / varying effect effects for each predictor
## Equal variance
## Normality (QQ plot)
## Outliers and multicolinearity
## Accuracy & AUC, etc
## Overall binned residual plot




# 2: binary 

# Use anova for the rest of the interaction terms




## JY's code for plotting
#gg_i <- ggplot(eda_i,
#              aes(y=voter_turnout,x=.data[[var1]],color=.data[[var2]]))+
#  geom_point(aes(size=total_voters), alpha=0.7)+
#  scale_size_continuous(range = c(5, 20))+
#  ylim(0.4, 0.9)+
#  xlab(var1)+
#  labs(color = var2)+
#  guides(size=FALSE)
#gg.name = paste("gg", var1, var2, sep = "_")
#assign(gg.name, gg_i)
