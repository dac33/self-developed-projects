
#### IMPORTING LIBRARIES #####
library(tidyverse)
library (ggthemes)
library(corrplot)
library(VIM)
library(caret)
library(RANN)
library(reshape2)
library(dbplyr)

x = c('corrplot','VIM', 'caret','RANN')
install.packages(x)

# Importing Data
train_data = read.csv('C:/Users/deeban.chandran/Desktop/Titanic Project/train.csv',na.strings ="")
test_data = read.csv('C:/Users/deeban.chandran/Desktop/Titanic Project/test.csv', na.string = "")
full_data = bind_rows(train_data, test_data)
full_data %>% head()

#### EXPLORATORY DATA ANALYSIS (EDA) ####

#Survival On Titanic
ggplot(full_data, aes(x = Survived, fill = factor(Survived))) + 
  geom_bar(position = 'dodge') +scale_x_discrete() + 
  labs(title = 'Survival on Titanic', x = 'Outcome', y = 'Count' ) + 
  theme_dark() + 
  theme(legend.position = 'right') + 
  scale_fill_discrete(labels = c('Died', 'Survived'), name= 'Outcome')

#Survival based on Sex
ggplot(full_data, aes(x = factor(Sex), fill = factor(Survived))) + 
  geom_bar(position = 'dodge') + 
  scale_x_discrete() +labs(title = 'Survival Based On Gender',x = 'Gender', y = 'Rate')+
  theme_dark() + 
  theme(legend.position = 'right') + 
  scale_fill_discrete(labels = c('Died', 'Survived'), name = 'Outcome')

#Survival Based on Pclass 
ggplot(full_data, aes(x = factor(Pclass), fill = factor(Survived))) + 
  geom_bar(position = 'dodge') + scale_x_discrete() + 
  labs (title = 'Survival based on Pclass', x = 'Pclass', y = 'Rate') + 
  theme_dark() + 
  theme(legend.position = 'right') + 
  scale_fill_discrete(labels = c('Died', 'Survived'), name = 'Outcome')

#Survival Based on Embarkment 
ggplot(full_data, aes(x = factor(Embarked), fill = factor(Survived))) + 
  geom_bar(position = 'dodge') + 
  scale_x_discrete() + 
  labs(title = 'Survival based on Embarkment', x = 'Embarkment', y = 'Rate') + 
  theme_dark() + 
  theme(legend.position = 'right') + 
  scale_fill_discrete(labels = c('Died', 'Survived'), name = 'Outcome')

# Survival Based on Age
ggplot(full_data) + 
  labs(title = 'Survival Based on Age', x = 'Age', y = 'count') +
  geom_freqpoly(aes(x = Age, color = factor(Survived)), binwidth = 1) + 
  theme_dark() + 
  scale_x_discrete() +
  scale_color_discrete(labels = c('Died', 'Survived'), name = 'Outcome') 

#Survival Based on Fare
ggplot(full_data[1:891,]) + 
  geom_freqpoly(aes(x = Fare, colour = factor(Survived)), binwidth = 0.05)+ theme(legend.position = 'right') +
  labs(title = 'Survival Based on Fare', x = 'Fare(log10)', y = 'Count') + 
  scale_x_log10()+
  theme_dark()+
  scale_color_discrete(labels = c('Died', 'Survived'), name = 'Outcome') 

#### CORRELATION ####
full_data$Sex = as.numeric(full_data$Sex)
corrplot.mixed(corr = cor(full_data[c('Survived', 'Fare', 'Sex', 'Pclass', 'Age')], use = 'complete.obs'), tl.col = "black", upper = "ellipse") 

#### OBSERVATION ####
# Most of the passengers on the Titanic died.
# Women had better chance at survival than men with the majority of them surviving while the majority of the men died. 
# The higher the passenger class, the better chance of survival.
# Those who embarked from *C* had a slightly higher chance of survival than those who embarked from other places. 
# There seems to be a trend of those younger than 16 having a higher chance of survival than death. Other age groups have a higher risk of death than survival.
# Passengers who paid a higher fare price had, in general, a higher chance of survival than those who paid less for their fare. 


#### FEATURE ENGINEERING ####

## Family Size ##
full_data$family = full_data$SibSp + full_data$Parch + 1
ggplot(full_data[1:891,], aes(x = factor(family), fill=factor(Survived))) +
       geom_bar(position = 'dodge') +
       scale_x_discrete() +
       labs(title = 'Survival by Family Size on Board', 
            x = 'Number of family members on board', y = 'Count') + 
       scale_fill_discrete(name = 'Outcome', labels = c('Died', 'Survived')) + 
       theme(legend.position = 'right') +
       theme_classic()

full_data$family_range = cut(full_data$family, c(0, 1, 4, 15), include.lowest = TRUE) #to convert numeric to factor

levels(full_data$family_range) = c('1', '2-4', '5+') #setting levels for the factors 

ggplot(full_data[1:891,], aes(x=factor(family_range), fill = factor(Survived))) + 
  geom_bar(position = 'dodge') +
  labs(title = 'Survival Based on Family Size', x = 'Family Size', y = ' Count') + 
  scale_x_discrete() + 
  theme(legend.position = 'right')+
  theme_dark() +
  scale_fill_discrete(labels = c('Died','Survived'), name = 'Outcome')

## Title ##
full_data$Title = gsub('(.*, )|(\\..*)', '', full_data$Name)
table(full_data$Title)

rare_title = c('Capt', 'Col', 'Don', 'Jonkheer', 'Lady', 'Major', 'Rev', 'Sir', 
               'the Countess', 'Dr')
full_data$Title[full_data$Title %in% rare_title] = 'Rare title' #to make the minority titles show as rare title.  

full_data$Title[full_data$Title=='Mlle'] = 'Miss'
full_data$Title[full_data$Title=='Ms'] = 'Miss'
full_data$Title[full_data$Title=='Dona'] = 'Miss'
full_data$Title[full_data$Title=='Mme'] = 'Mrs'

ggplot(full_data[1:891,], aes(x = Title, fill = factor(Survived))) + 
  geom_bar(position = 'dodge') + 
  labs(title = 'Survival by Title', x = 'Title', y = 'Count') + 
  scale_x_discrete() + 
  scale_fill_discrete (labels = c('Died', 'Survived'), name = 'Outcome') + 
  theme_dark() + 
  theme(legend.position = 'bottom')

## Cabin ##
full_data$Cabin_letter <- gsub('[0-9].*', '', full_data$Cabin) #to extract the letters of cabin

full_data$Cabin_letter[full_data$Cabin_letter=='E'] <- 'EFGT'#blanking the minority letters 
full_data$Cabin_letter[full_data$Cabin_letter=='F'] <- 'EFGT'
full_data$Cabin_letter[full_data$Cabin_letter=='F E'] <- 'EFGT'
full_data$Cabin_letter[full_data$Cabin_letter=='F G'] <- 'EFGT'
full_data$Cabin_letter[full_data$Cabin_letter=='G'] <- 'EFGT'
full_data$Cabin_letter[full_data$Cabin_letter=='T'] <- 'EFGT'

full_data$Cabin_letter[is.na(full_data$Cabin_letter)] <- 'Blank'

ggplot(full_data[1:891,], aes(x= Cabin_letter, fill = factor(Survived))) +
  geom_bar(position = 'dodge') + 
  labs(title = 'Survival based on Cabin Letter', x = 'cabin letter', y = 'Count') +
  scale_x_discrete() +
  scale_fill_discrete(labels = c('Dead', 'Survived'), name = 'Outcome') + 
  theme_dark() + 
  theme(legend.position = 'right')

#ppl with no cabin died more. 
full_data$cabin_presence[full_data$Cabin_letter=='Blank'] <- 'No cabin'
full_data$cabin_presence[is.na(full_data$cabin_presence)] <- 'Cabin'  

ggplot(full_data[1:891,], aes(x = factor(cabin_presence), fill=factor(Survived))) +
       geom_bar(position = 'dodge') +
       scale_x_discrete() +
       labs(title = 'Survival by Cabin', x = 'Cabin', y = 'Count') + 
       scale_fill_discrete(name = 'Outcome', labels = c('Died', 'Survived')) + 
       theme(legend.position = 'right') +
       theme_dark()

## Ticket Number ##
full_data$Ticket_number <- gsub('[^0-9]', '', full_data$Ticket)
table(full_data$Ticket_number=="")

full_data$Ticket_number[full_data$Ticket_number==""] = 0
full_data$Ticket_number <- as.integer(full_data$Ticket_number)

ggplot(full_data[1:891,]) + 
       geom_freqpoly(aes(x = Ticket_number, color = factor(Survived)), binwidth=0.1) +
       scale_x_log10() +
       scale_color_discrete(name = 'Outcome', labels = c('Died', 'Survived')) +
       theme_classic() +
       labs(title = 'Survival by Ticket number', x = 'Ticket number', y = 'Count')

cor(full_data$Ticket_number, as.numeric(full_data$Survived), use = 'complete.obs') #when you cant see a clear trend, you try and see the correlation
#This shows there no real correlation


#### FIXING MISSING VALUES ####

### Preperation of Data ###

#to eliminate the least correlation (Ticket Number and Embarkment)
full_data_relevant = subset(full_data, select = c(Survived, Pclass, Sex, Age, Fare, Title, cabin_presence, family_range))

full_data_relevant$Survived = as.factor(full_data_relevant$Survived)
full_data_relevant$Pclass = factor(full_data_relevant$Pclass, ordered = TRUE)
full_data_relevant$Survived = as.factor(full_data_relevant$Survived)
full_data_relevant$Title = as.factor(full_data_relevant$Title)
full_data_relevant$cabin_presence = as.factor(full_data_relevant$cabin_presence)                            

aggr(full_data_relevant, sortVars = TRUE, prop = FALSE, cex.axis = .6, numbers = TRUE)

md_prediction = preProcess(full_data_relevant[c(2:8)], method = c('knnImpute', 'center', 'scale'))
print(md_prediction)

full_data_complete = predict(md_prediction, newdata = full_data_relevant[c(2:8)])

full_data_final = data.frame(full_data_complete, full_data$Survived)
full_data_final = cbind(full_data$PassengerId, full_data_final)


full_data_final = rename(full_data_final, Survived = full_data.Survived, PassengerId = `full_data$PassengerId`)

full_data_final$Survived = as.factor(full_data_final$Survived)

#### MACHINE LEARNING ####

# Splitting back to test and train data 
train <- full_data_final[1:891,]
test <- full_data_final[892:1309,]

### RANDOM FOREST ###
