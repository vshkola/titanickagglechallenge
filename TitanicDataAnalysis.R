# Load csv data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

rawfordata <- test.Survived[,c(2,1,3:ncol(test.Survived))]

# Combine data sets
data.combined <- rbind(train, rawfordata)

str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

table(data.combined$Survived)

table(data.combined$Pclass)

library(ggplot2)

#Hypothesis rich - survived
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_histogram(width = 0.5) + 
  xlab("Pclass")
  ylab("Total Count") +
  labs(fill = "Survived ")
  
head(as.character(train$Name))

length(unique(as.character(data.combined$Name)))

# Get publicate names
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

#Any correlation with other variables?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

males  <- data.combined[which(train$Sex == "male"),]
males[1:5,]

#Explore 3-dimensional relationship

excractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0){
    return ("Master.")
  }  else if (length(grep("Mrs.", name)) > 0){
    return ("Mrs")
  } else if (length(grep("Mr.", name)) > 0){
    return ("Mr")
  } else {
    return ("Other")
    }
}

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, excractTitle(data.combined[i, "Name"]))
}
data.combined$title <- as.factor(titles)

#Let's use only first 891 rows that corresponds to train data set
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) + 
  stat_count(width = 0.5) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("PClass") + 
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

table(data.combined$Sex)

#Visual of sex, pclass and survival relationship.

ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  ggtitle("Pclass by Sex") +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master" good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

boys <- data.combined[which(data.combined$title == "Miss."),]
summary(boys$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  ggtitle("Age for Miss by PClass") +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  xlab("Age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone)
length(which(misses.alone$Age <= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# Visal survival rates by sibsp, pclass andtitle

ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  stat_count(width = 1) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") + 
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

data.combined$Parch <- as.factor(data.combined$Parch)

# Visal survival rates by sibsp, pclass andtitle

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  stat_count(width = 1) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") + 
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Create family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)

data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  stat_count(width = 1) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") + 
  xlab("family size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

str(data.combined$Ticket)

# Convert ticket price into characters
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

# ticket.first.char could be factor
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# Visualize first char data
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") + 
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,360) +
  labs(fill = "Survived")

# Visualize first char data per pclass
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by ticket.first.char") + 
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, title") + 
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

# Check Titanic fares passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#Visualize fare - numeric variable with hist. 
ggplot(data.combined, aes(x = Fare)) + 
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) + 
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + title) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) + 
  labs(fill = "Survived")

str(data.combined$Cabin)

# Make cabins - strings
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

#Take a look at first letter

Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)

#Add to combined data set and plot
data.combined$Cabin.first.char <- Cabin.first.char

#Drill into pclass/cabin dataZ
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("PClass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

#Cabin / pclass / title
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("PClass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Folks in multiply cabins
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

#Check place where volks on board to Titanic
str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#data.combined <- subset(data.combined, select = -cabin)

#Let's start with Random Forest
library(randomForest)

#Train a Random Forest with the default parameters using Pclass & Title
rf.train.1 <- data.combined[1:891, c("Pclass","title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891, c("Pclass","title", "SibSp")]
#Train a Random Forest with the default parameters using Pclass & Title & SibSp
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

rf.train.3 <- data.combined[1:891, c("Pclass","title", "Parch")]

#Train a Random Forest with the default parameters using Pclass & Title & Parch
set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#Family size RF

rf.train.5 <- data.combined[1:891, c("Pclass", "title","family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


#Try both. With adding of SibSp -> error increasing
rf.train.6 <- data.combined[1:891, c("Pclass", "title","SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

#Cross validation 

test.submit.df <- data.combined[892:1309, c("Pclass","title","family.size")]

#Make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

#Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20160828_1.csv", row.names = FALSE)

#Let's look into cross-validation to see if we could do better

library(caret)
library(doSNOW)

# 10-fold CV repeated 10 times is the best place to start

# Leverage caret to crearte 100 total folds, but check that ration of survived/perished matches the overall training set
# This is known as stratified cross validation and generally provides better results.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
342 / 549

table(rf.label[cv.10.folds[(33)]])
308 / 494

# Setup caret's trainControl object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

# Setup doSNOW package for multi-core trainings. Helpful to train a lot of trees
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Train

set.seed(32324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1)
library(e1071)

stopCluster(cl)

# Check results
rf.5.cv.1

# Let's try 5-fold CV repeated 10 times
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.Z <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.Z)

stopCluster(cl)

rf.5.cv.2

# 5-fold CV isn't better. Move to 3-fold 10 times

set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

stopCluster(cl)

rf.5.cv.3