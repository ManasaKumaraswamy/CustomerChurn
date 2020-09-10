library(readxl)
library(psych)

getwd()
setwd("K:/Spring2020/IDS 572 - Data Mining/Assignments/Assignment 4/Assignment 4")

df <- read_excel("data.xlsx", sheet="Case Data")
str(df)
df$`Churn (1 = Yes, 0 = No)` <- as.factor(df$`Churn (1 = Yes, 0 = No)`)


describe(df$`Customer Age (in months)`)
#The mean can be estimated to be about 13.9 months and median being 11 and the median is lesser than the mean

boxplot(df$`Customer Age (in months)`~df$`Churn (1 = Yes, 0 = No)`)
table(df$`Churn (1 = Yes, 0 = No)`)
#There are about 6024 values for no and 323 for churn yes.

#consider the yes and no seperately to calculate the 
y<-df[df$`Churn (1 = Yes, 0 = No)`=="1",]
n<-df[df$`Churn (1 = Yes, 0 = No)`=="0",]
plot(density(y$`Customer Age (in months)`), col="red", lwd=2.5, main="Distribution of Age by Churn yes or no")
lines(density(n$`Customer Age (in months)`), col="blue", lwd=2.5)

library(dplyr)
df %>% group_by(`Churn (1 = Yes, 0 = No)`) %>% summarise(avg = mean(`Customer Age (in months)`), median = median(`Customer Age (in months)`), sd = sd(`Customer Age (in months)`), freq=n()) 

#From the plot, Churn yes peaks at approximately 13 months and churn no at less than 11 months. 
##churn-yes has an avg value of 15.4 and median 13; 
##churn-no has an avg value of 13.8 and median value of 11. 
##according to these stats, data do not strongly support Wall's belief as customer with no-churn has median age of 11 
## However, we can conclude that the comapany has to target customers approaching 12-13 months, as the churn rate is highest at this age


##Consider the model with all variables as the indepenedent variables with Churn as the the dependent variable

model <- glm(`Churn (1 = Yes, 0 = No)` ~.-ID, data = df, family="binomial")
summary(model)
##The AIC of the model is 2464.3
## smaller value of AIC is desired.

#with variable selection
null <- glm(`Churn (1 = Yes, 0 = No)` ~1, data = df, family="binomial")
step(null,scope=list(lower=null,upper=model),direction="forward")

model1 <- glm(`Churn (1 = Yes, 0 = No)` ~ `CHI Score Month 0` + 
                `Days Since Last Login 0-1` + `CHI Score 0-1` + `Customer Age (in months)` + 
                `Views 0-1` + `Support Cases 0-1` + `Support Cases Month 0`, 
              family = "binomial", data = df)
summary(model1)
#AIC:2457


row <- df[df$ID == 672,]
View(row) ##no churn
predict(model1, row  , type="response")

#possibility of churning is about 3.8% which happens to be very low probability of churning and according to the data given the customer did not churn

row1 <- df[df$ID == 354,]
row1  ##No churn
predict(model1, row1  , type="response")

#possibility of churning is about 4.7% which happens to be very low probability of churning and according to the data given the customer did not churn

row2 <- df[df$ID == 5203,]
row2  #no churn
predict(model1, row2  , type="response")

#possibility of churning is about 4.2% which happens to be very low probability of churning and according to the data given the customer did not churn


summary(model1)
#There are 6 factors significant namely Customer Age (in months), CHI Score Month 0, CHI Score 0-1, Support Cases 0-1, Views 0-1 and Days Since Last Login 0-1. 
#If we increase the confidence level to 95%, Support Cases 0-1 is not significant and the remain 5 factors are still significant.
# So the top 3 factors are CHI Score Month 0, CHI Score 0-1 and Days Since Last Login 0-1. 

alldata <- predict(model1, type = 'response', data=df)
alldata<-order(alldata)
alldata[1:100]

##the list of the top 100 IDs with highest probability of churning is as seen here.
