train <- read.csv("C:/Users/Mathew K/Desktop/Mathew's/ACGILD Data Analytics/Acad Assignment 6/train.csv")

#Preprocess the passenger names to come up with a list of titles that represent families and represent using appropriate visualization graph

train$famName <- substr(train$Name,1,regexpr('\\,',train$Name)-1)
train$title <- substr(train$Name,regexpr('\\,',train$Name)+1,regexpr('\\.',train$Name)-1)

famName <- as.data.frame(sort(table(train$famName),decreasing = T))
colnames(famName) <- c("famName","Freq")
View(famName)
famName$IsFam <- 0
famName$IsFam[famName$Freq>1] <- 1

train1 <- merge(train,famName, by = "famName")
View(train1)
famTitle <- as.data.frame(sort(table(train1$title[train1$IsFam ==1]),decreasing = T))
colnames(famTitle) <- c("Title","Freq")
library(plotrix)
pie3D(famTitle$Freq,labels = famTitle$Title,explode = 0.05)

library(ggplot2)
ggplot(data = famTitle,aes(Title,Freq),fill=param)+
  geom_bar(stat = "identity",position = "dodge")+
  geom_text(aes(label = Freq),position=position_dodge(width=0.9),vjust = -0.2,size=5)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),axis.title=element_text(size=24,face="bold"),legend.text=element_text(size=18),legend.title=element_text(size=20),axis.text = element_text(size = 20))

#Represent the proportion of people survived from the family size using a graph
table(train1$Freq)
train1$famType <- "Small"
train1$famType[train1$Freq>=6] <- "Large"
train1$famType[train1$Freq>=3 & train1$Freq <6 ] <- "Medium"
train1$famType[train1$Freq==1] <- "Individuals"
library(dplyr)
train1_Summ <- train1 %>%
  group_by(famType,Survived) %>%
  summarise(count = n())
train1_Summ$Survived <- as.factor(train1_Summ$Survived)
train1_Summ$ypos <- 0
train1_Summ$ypos[train1_Summ$Survived==1] <- train1_Summ$count[train1_Summ$Survived==1]/2
train1_Summ$ypos[train1_Summ$Survived==0] <- train1_Summ$count[train1_Summ$Survived==1]+train1_Summ$count[train1_Summ$Survived==0]/2

ggplot(data=train1_Summ[train1_Summ$famType != "Individuals",],aes(famType,count,fill=Survived))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = count, y=ypos),vjust = -0.2,size=5)+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))

#Impute the missing values in Age variable using Mice Library, create two different graphs showing Age distribution before and after imputation

install.packages("mice")
library(mice)
md.pattern(train)
train2<- as.data.frame(train$Age)
train2$pid <- train$PassengerId
train_imp <- mice(train2, m=1, maxit = 5, method = 'pmm')

train3 <- complete(train_imp,1)

par(mfrow=c(1,2))
h1 <- hist(train2$`train$Age`, main = "Before Imputation", xlab = "Age")
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))
h2 <- hist(train3$`train$Age`, main = "After Imputation", xlab = "Age")
text(h2$mids,h2$counts,labels=h2$counts, adj=c(0.5, -0.5))
