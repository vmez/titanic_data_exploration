#----------------------------remove working directory past labels:
rm(list = ls())


#----------------------------
library(readr)
library(plyr)
library(dplyr) 
library(tidyr)
library(ggplot2)
library(gridExtra)
library(rpart)
library(rattle)
library(lattice)
library(caret)
library(scales)
#---------------------------

titanicD <- read_csv("Ubiqum/Titanic/train.csv")
titanic_test <- read_csv("Ubiqum/Titanic/test.csv")
View(titanicD)
View(titanic_test)
str(titanicD)

titaniC = titanicD
summary(titaniC)

# --------------------------Female vs Male, to convert to dummy variable:
titaniC$Sex <- ifelse(titaniC$Sex == 'female', 1,0)
table(titaniC$Sex)


# --------------------------how many survived or not?
table(titaniC$Survived)
# 549 did not
# 342 survived

table(titaniC$Survived, titaniC$Pclass)


titaniC %>% group_by(Pclass, Survived) %>% tally()


ggplot(titaniC, aes(Pclass)) + 
  geom_bar(aes(weight = Survived, fill = Sex), position = "dodge") + 
  ggtitle("Distribution of Survival per Class") + 
  ylab("Total Survival") + 
  xlab("Class") +
  scale_fill_discrete(name = "Gender",
                      breaks = c("female", "male"),
                      labels = c("Female", "Male"))



# --------------------------Sex in Pclass?
ggplot(titaniC) + geom_bar(aes(Pclass, fill = Sex), position = "dodge")

titaniC %>% group_by(Pclass, Sex) %>% tally() 


# --------------------------Survival per Class, Sex
titaniC %>% group_by(Pclass, Sex, Survived) %>% summarize(spc = n())


# -------------------------- Per Class, count the passangers that Embarked per Town
titaniC %>% group_by(Pclass, Embarked) %>% summarise(TownCount = n())


# ---------------------------Where have the passangers Embarked? 
Town <- titaniC %>% group_by(Pclass) %>% select(Embarked)

ggplot(Town) + geom_bar(aes(Pclass, fill = Embarked)) + 
  ggtitle("Where have the most passengers Embarked?") + 
  xlab("Passengers per Class") +
  scale_fill_discrete(name = "Town", 
                      breaks = c("C", "Q", "S"), 
                      labels = c("Cherbourg", "Queenstown", "Southampton"))


#--- Fill Missing Embarked
which(is.na(titaniC$Embarked))
titaniC[c(62, 830),]
titaniC$Embarked[c(62, 830)] <- 'C'

# ----------------------------- Ticket Fare
Fare <- titaniC %>% 
  group_by(Pclass, Sex) %>% 
  summarize(MinF = min(Fare), MaxF = max(Fare), Mean = mean(Fare))


ggplot(Fare) + geom_density(aes(Mean, fill = Sex), alpha = .4) +
  ggtitle("Density Distribution of Average Cost per Ticket") +
  xlab("") +
  scale_x_continuous(labels=dollar_format()) +
  scale_fill_discrete(name = "Gender")



Fare1 <- titaniC %>% group_by(Pclass, Sex) %>% summarise(M = mean(Fare))
ggplot(Fare1) +
  geom_col(aes(Pclass, M, fill = Sex), position = "dodge") +
  ggtitle("Average Cost of Ticket per Class")+
  ylab("Count") + 
  xlab("") + 
  scale_fill_discrete(name = "Gender",
                      breaks = c("female", "male"),
                      labels = c("Female", "Male"))


#---Fare per Embarkment + Sex + Class
NameTown <- as_labeller(c(`C` = "Cherbourg", `Q` = "Queenstown", `S` = "Southampton"))
ggplot(titaniC) + 
  geom_histogram(aes(Fare, fill = Sex), binwidth = 25, position = "dodge") + 
  facet_grid(Pclass~Embarked, scales = "free", labeller = labeller(Embarked = NameTown)) + 
  scale_x_continuous(labels = dollar_format()) + 
  ggtitle("Distribution Cost of Fare") +
  xlab("") + scale_fill_discrete(name = "Gender")



#---Distribution of Cost per Class
ggplot(titaniC) + geom_histogram(aes(Fare, fill = Sex), bins = 30, position = "dodge" ) + 
  facet_grid(.~Pclass) +
  ggtitle("Fare Distribution per Class") + 
  scale_fill_discrete(name = "Gender") + 
  scale_x_continuous(labels = dollar_format()) +
  xlab("")



#------------------------------ Density Plot for Fare of persons Embarking on Port S
ggplot(titaniC[titaniC$Pclass == '3' & titaniC$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format())


# ---------------------------- Age investigation
ggplot(titaniC) + geom_bar(aes(Age, fill=Sex), binwidth = 10, position = "dodge") +
  ggtitle("Age Distribution to Gender") + 
  ylab("Count") +
  xlab("") +
  scale_fill_discrete(name = "Gender")


par(mfrow = c(1,2))

#----------------------------- Group by Pclass, Sex to find the average Age
titaniC %>% group_by(Pclass, Sex) %>% summarize(mean(Age, na.rm = TRUE))


#----------------------------- Per Class, Males younger than 25
titaniC %>% group_by(Pclass) %>% 
  select(Sex, Age) %>% 
  filter( Age < 25, Sex== 'male') %>% 
  summarise(n())


#----------------------------- StDev of Age per Sex and Class
titaniC %>% group_by(Pclass, Sex) %>% summarize(STDEV = sd(Age, na.rm = TRUE))



# ----------------------------- Making a list with the NA in age
#---VERSION 1
NA_list <- titaniC[c(which(is.na(titaniC$Age))),]

#---VERSION 2
NA_list <- subset(titaniC, is.na(Age))


#------------------------------ Age fixes:
#--- Smaller values than 1 should be 0
which(titaniC$Age < 1)
titaniC$Age[titaniC$Age < 1] <- 0

#--- Function to fill NA age based on mean Age per Class
av_age <- ave(titaniC$Age, titaniC$Pclass, FUN = function(x)mean(x, na.rm = TRUE))
titaniC$Age <- ifelse(is.na(titaniC$Age), av_age , titaniC$Age)
summary(titaniC$Age)
summary(titaniC)


#-----------------------------As.csv fixed Age & Embarked
write.csv(titaniC, file = "C:/Users/Violeta/Documents/Ubiqum/Titanic/TrainingFixed.csv")



#-----------------------------Load Dataset with changed NA for Age & Embarked
data <- read.csv("Ubiqum/Titanic/TrainingFixed.csv")
titaniC <- data
head(titaniC)


#------------------------------Separating Names with title
#titaniC <- separate(titaniC, Name, c("Last", "Title", "Rest"), "(,) | (.) | ()")

#---Because the above command depends on structure within Name, better extract Title with gsub
titaniC$Title <- gsub('( .*)|(.*, )', '', titaniC$Name)
head(titaniC)
table(titaniC$Sex, titaniC$Title)

#Changing to uniform titles
filter(titaniC, Title %in% c("Ms.", "Mlle.", "Mme."))
titaniC$Title[c(444, 642, 711)] <- 'Mme.'

filter(titaniC, Title %in% c("Don.", "Sir."))
titaniC$Title[c(31, 600)] <- 'Mr.'

titaniC$Title[760] <- 'Mrs.' #because Countess only one and split in testD can't classify


#---------------------------- Changing structure to numeric to compute correlation matrix
titaniCN <- sapply(titaniC, as.numeric)
str(titaniCN)
titaniCN <- data.frame(titaniCN)
str(titaniCN)
titaniCN <- titaniCN[, -c("13")]
cor1 <- cor(titaniCN)
corrplot(cor1, order = "hclust", type = "lower")


#-----------------------------Preparing Partitions
set.seed(136)

inTrain <- createDataPartition(titaniC$Survived, p = .75, list = FALSE)

trainD <- titaniC[inTrain,]
testD <- titaniC[-inTrain,]

#----------------------------- Decision Tree
#tControl<- trainControl(method= "repeatedcv", number = 10, repeats = 3)

DTree_fit<- rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                  data = trainD, 
                  method="class",
                  parms = list(split = 'information'))

DTree_fit
fancyRpartPlot(DTree_fit)
DTree_fit$variable.importance
DTree_fit$cptable
printcp(DTree_fit)
summary(DTree_fit)

#---Based on variable Importance, the following were chosen:
Other_fit<- rpart(Survived~ Pclass + Age + Fare + Title,
                  data = trainD, 
                  method="class",
                  parms = list(split = 'gini'))

fancyRpartPlot(Other_fit)
Other_fit$variable.importance

Other_pred <- predict(Other_fit, testD, method = "class")

#-------------------------------------Random Forest
RF_fit <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                data = trainD, 
                method="rf",
                metric = "Accuracy")



