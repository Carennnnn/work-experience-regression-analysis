library(tidyverse)
library(lubridate)

//data is downloaded from "https://www.kaggle.com/aungpyaeap/beauty?select=beauty.csv"
setwd("Downloads")

//plot of ggpairs
beauty <- read.csv("beauty.csv")
beauty%>%ggpairs()

//mutate a new variable gender
library(dplyr)
beauty <- beauty %>% 
  mutate(gender=factor(female, levels = c(0,1), labels=c("male", "female")))

//experience~wage plot
beauty %>% ggplot(aes(y=exper, x=wage, color=gender))+
  geom_point()+
  labs(x="wage", y="year of experience", title="wage effects on people's experience")+
  theme(plot.title = element_text(hjust = 0.5))

//bar chart of people look
beauty %>% ggplot(aes(x=looks, fill=gender))+
  geom_bar()+
  labs(x="appearance", title="People's attractiveness score")+
  theme(plot.title = element_text(hjust = 0.5))

//mutate a new variable health
beauty <- beauty %>% 
  mutate(health=factor(goodhlth, levels = c(0,1), labels=c("not well", "well")))

//education~wage plot
beauty %>% ggplot(aes(y=educ, x=wage, color=health))+
  geom_point()+
  labs(x="wage", y="year of education", title="wage and people's education")+
  theme(plot.title = element_text(hjust = 0.5))

//mutate a new variable married
marry <- beauty %>% 
  mutate(got_married=factor(married, levels = c(0,1), labels=c("No", "Yes")))

//experience~marriage plot
marry %>% ggplot(aes(y=exper, x=got_married, color=gender))+
  geom_point()+
  labs(x="marital status", y="work experience", title="work experience and marital status")+
  theme(plot.title = element_text(hjust = 0.5))

//model selection
beauty <- beauty%>%select(-gender, -health)
N = beauty %>% nrow
beauty = beauty%>%mutate(train=runif(N)<.5)
beautyTrain = beauty%>%filter(train == TRUE)
beautyTest = beauty%>%filter(train == FALSE)

beautyTrain <- beautyTrain%>%select(-train)
m_beauty = lm(exper~., beautyTrain)
summary(m_beauty)
stepMains = step(m_beauty, direction="both")
m_beauty = lm(exper~wage + goodhlth + female + married + service + educ + looks, beautyTrain)

//diagnostic plot
plot(m_beauty)

//predict experience for a person
newPerson <-data.frame(wage = 20.5, goodhlth = 1, female = 0, married  = 1, service = 0, educ = 12, looks = 3)
predict(m_beauty, newPerson, interval = "prediction", level = 0.95)

//confidence interval
beautyTest <- beautyTest%>%select(-train)
m_beautyTest = lm(exper~wage + goodhlth + female + married + service + educ + looks, beautyTest)
confint(m_beautyTest)

