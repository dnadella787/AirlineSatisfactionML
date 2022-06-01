
library(readr)
train <- suppressMessages(suppressWarnings(read_csv("./train.csv"))) #training data
train_no_id <- train[,-c(1,2)] #rid of index and id
test <- suppressMessages(suppressWarnings(read_csv("./test.csv"))) #testing data
test_no_id <- test[,-c(1,2)]

# Deal with categorical variables
train_no_id$Gender <- factor(train_no_id$Gender)
train_no_id$`Customer Type` <- factor(train_no_id$`Customer Type`)
train_no_id$`Type of Travel` <- factor(train_no_id$`Type of Travel`)
train_no_id$Class <- factor(train_no_id$Class)
train_no_id$`Inflight wifi service` <- factor(train_no_id$`Inflight wifi service`)
train_no_id$`Departure/Arrival time convenient` <- factor(train_no_id$`Departure/Arrival time convenient`)
train_no_id$`Ease of Online booking` <- factor(train_no_id$`Ease of Online booking`)
train_no_id$`Gate location` <- factor(train_no_id$`Gate location`)
train_no_id$`Food and drink` <- factor(train_no_id$`Food and drink`)
train_no_id$`Online boarding` <- factor(train_no_id$`Online boarding`)
train_no_id$`Seat comfort` <- factor(train_no_id$`Seat comfort`)
train_no_id$`Inflight entertainment` <- factor(train_no_id$`Inflight entertainment`)
train_no_id$`On-board service` <- factor(train_no_id$`On-board service`)
train_no_id$`Leg room service` <- factor(train_no_id$`Leg room service`)
train_no_id$`Baggage handling` <- factor(train_no_id$`Baggage handling`)
train_no_id$`Checkin service` <- factor(train_no_id$`Checkin service`)
train_no_id$`Inflight service` <- factor(train_no_id$`Inflight service`)
train_no_id$Cleanliness <- factor(train_no_id$Cleanliness)
test_no_id$Gender <- factor(test_no_id$Gender)
test_no_id$`Customer Type` <- factor(test_no_id$`Customer Type`)
test_no_id$`Type of Travel` <- factor(test_no_id$`Type of Travel`)
test_no_id$Class <- factor(test_no_id$Class)
test_no_id$`Inflight wifi service` <- factor(test_no_id$`Inflight wifi service`)
test_no_id$`Departure/Arrival time convenient` <- factor(test_no_id$`Departure/Arrival time convenient`)
test_no_id$`Ease of Online booking` <- factor(test_no_id$`Ease of Online booking`)
test_no_id$`Gate location` <- factor(test_no_id$`Gate location`)
test_no_id$`Food and drink` <- factor(test_no_id$`Food and drink`)
test_no_id$`Online boarding` <- factor(test_no_id$`Online boarding`)
test_no_id$`Seat comfort` <- factor(test_no_id$`Seat comfort`)
test_no_id$`Inflight entertainment` <- factor(test_no_id$`Inflight entertainment`)
test_no_id$`On-board service` <- factor(test_no_id$`On-board service`)
test_no_id$`Leg room service` <- factor(test_no_id$`Leg room service`)
test_no_id$`Baggage handling` <- factor(test_no_id$`Baggage handling`)
test_no_id$`Checkin service` <- factor(test_no_id$`Checkin service`)
test_no_id$`Inflight service` <- factor(test_no_id$`Inflight service`)
test_no_id$Cleanliness <- factor(test_no_id$Cleanliness)






#Train and test data without delay

train_removeDelay <- train_no_id[-c(21,22)]
train_removeDelay <- train_removeDelay[rowSums(is.na(train_removeDelay)) == 0, ]
#Remove Delays since not important + No more NAs
test_removeDelay <- test_no_id[-c(21,22)]





# Reduced train and test data after feature selection 
reduced_features_ind <- c(2:5, 8:9, 12, 16:17, 21:23)
reduced_train <- train_no_id[, reduced_features_ind]

reduced_test <- test_no_id[, reduced_features_ind]

#################################################################
#Reduced KNN Model
library(class)

#separate out the satisfaction from the predictor variables
#for both the test and train data
red_train<-na.omit(reduced_train)
train_satisfaction<-red_train$satisfaction
red_train<-red_train[c(2,5,6,7,8,9,10,11)]

red_test<-na.omit(reduced_test)
test_satisfaction<-red_test$satisfaction
red_test<-red_test[c(2,5,6,7,8,9,10,11)]


#scale all of the numeric data (columns 2,5:11)
for (i in 1:8) {
	red_train[i] = scale(as.numeric(unlist(red_train[i])))
	red_test[i] = scale(as.numeric(unlist(red_test[i])))
}

#build actual model (87.06%)
k_1 <- knn(red_train, red_test, train_satisfaction, k=9)

#check 
mean(k_1 == test_satisfaction)
#################################################################



