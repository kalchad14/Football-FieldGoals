## Football Field Goal Success Model
## Kalyan Chadalavada
## Based on an MIT Sloan presentation:
## http://www.sloansportsconference.com/content/going-for-three-predicting-the-likelihood-of-field-goal-success-with-logistic-regression/
library(randomForest)
# Functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Start by setting working directory
setwd(".")

# Call in weather data
weather <- read.csv("weather.csv", stringsAsFactors = FALSE)

# Keep if 2002-2012
# Games outside this range, we don't have data for
weather.clean <- weather[8216:10936,]
rm(weather)

# Extract date in character form from id
for(i in 1:nrow(weather.clean)) {
  # Remove team id from id
  weather.clean$datechar[i] <- gsub("[^0-9]", "", weather.clean$id[i])
}
# Divide by 10 to remove trailing 0
weather.clean$datechar <- as.integer(weather.clean$datechar)/10

# Extract home team from id
for(i in 1:nrow(weather.clean)) {
  weather.clean$hometeam[i] <- substrRight(weather.clean$id[i],3)
}

# Call in 2002-2012 data and merge
data02 <- read.csv("2002_nfl_pbp_data.csv")
data03 <- read.csv("2003_nfl_pbp_data.csv")
data_comb_03 <- rbind(data02,data03)
rm(data02)
rm(data03)
data04 <- read.csv("2004_nfl_pbp_data.csv")
data_comb_04 <- rbind(data04,data_comb_03)
rm(data_comb_03)
rm(data04)
data05 <- read.csv("2005_nfl_pbp_data.csv")
data_comb_05 <- rbind(data05,data_comb_04)
rm(data_comb_04)
rm(data05)
data06 <- read.csv("2006_nfl_pbp_data.csv")
data_comb_06 <- rbind(data06,data_comb_05)
rm(data_comb_05)
rm(data06)
data07 <- read.csv("2007_nfl_pbp_data.csv")
data_comb_07 <- rbind(data07,data_comb_06)
rm(data_comb_06)
rm(data07)
data08 <- read.csv("2008_nfl_pbp_data.csv")
data_comb_08 <- rbind(data08,data_comb_07)
rm(data_comb_07)
rm(data08)
data09 <- read.csv("2009_nfl_pbp_data.csv")
data_comb_09 <- rbind(data09,data_comb_08)
rm(data_comb_08)
rm(data09)
data10 <- read.csv("2010_nfl_pbp_data.csv")
data_comb_10 <- rbind(data10,data_comb_09)
rm(data_comb_09)
rm(data10)
data11 <- read.csv("2011_nfl_pbp_data.csv")
data_comb_11 <- rbind(data11,data_comb_10)
rm(data_comb_10)
rm(data11)
data12 <- read.csv("2012_nfl_pbp_data.csv")
data.unclean <- rbind(data12,data_comb_11)
rm(data_comb_11)
rm(data12)

##### OR MERGE LIKE THIS########
data02 <- read.csv("2002_nfl_pbp_data.csv")
data03 <- read.csv("2003_nfl_pbp_data.csv")
data04 <- read.csv("2004_nfl_pbp_data.csv")
data05 <- read.csv("2005_nfl_pbp_data.csv")
data06 <- read.csv("2006_nfl_pbp_data.csv")
data07 <- read.csv("2007_nfl_pbp_data.csv")
data08 <- read.csv("2008_nfl_pbp_data.csv")
data09 <- read.csv("2009_nfl_pbp_data.csv")
data10 <- read.csv("2010_nfl_pbp_data.csv")
data11 <- read.csv("2011_nfl_pbp_data.csv")
data12 <- read.csv("2012_nfl_pbp_data.csv")
data.unclean <-rbind(data02,data03,data04,data05,data06,data07,data08,data09,data10,data11,data12)

# End merge
# Final, unclean df is called data.unclean

# Set data.unclean$description to a character (to not have many factors)
data.unclean$description <- as.character(data.unclean$description)

# Remove extra points and kick offs
data.unclean2 <- data.unclean[which(data.unclean$down != ""),]

# Extract data with field goal, rm garbage
data.fieldgoals <- data.unclean2[which(grepl("field goal",data.unclean2$description) == TRUE),]
rm(data.unclean2)


# Extract date in character form from gameid
for(i in 1:nrow(data.fieldgoals)) {
  data.fieldgoals$datechar[i] <- gsub("[^0-9]", "", data.fieldgoals$gameid[i])
}
data.fieldgoals$datechar <- as.integer(data.fieldgoals$datechar)

# Extract home team
data.fieldgoals$gameid <- as.character(data.fieldgoals$gameid)
for(i in 1:nrow(data.fieldgoals)) {
  data.fieldgoals$hometeam[i] <- tolower(substrRight(data.fieldgoals$gameid[i],3))
}
data.fieldgoals$hometeam <- tolower(substrRight(data.fieldgoals$gameid,3))

# Point distance
data.fieldgoals$scorediff <- abs(data.fieldgoals$offscore - data.fieldgoals$defscore)



# Time
data.fieldgoals$sec <- as.integer(data.fieldgoals$sec)
data.fieldgoals <- data.fieldgoals[which(data.fieldgoals$min >= 0),]
data.fieldgoals <- data.fieldgoals[which(data.fieldgoals$sec < 60),]
data.fieldgoals$min <- abs(data.fieldgoals$min-60)

data.fieldgoals$time <- data.fieldgoals$min+data.fieldgoals$sec/60



# Distance
# Add 18 yards to ydline, because of the actual distance the kicker kicks
data.fieldgoals$distance <- data.fieldgoals$ydline+18



# Result, 1 = Good, 0 = No Good or Blocked
for(i in 1:nrow(data.fieldgoals)) {
  if(grepl("No Good",data.fieldgoals$description[i]) | 
     grepl("BLOCKED",data.fieldgoals$description[i])) {
    data.fieldgoals$result[i] = FALSE
  }
  else {
    data.fieldgoals$result[i] = TRUE
  }
}

# Merge to get weather data
for(i in 1:nrow(data.fieldgoals)) {
  if(length(weather.clean$temperature[which(weather.clean$datechar == data.fieldgoals$datechar[i] & weather.clean$hometeam == data.fieldgoals$hometeam[i])]) > 0) {
    data.fieldgoals$temperature[i] <- weather.clean$temperature[which(weather.clean$datechar == data.fieldgoals$datechar[i] & weather.clean$hometeam == data.fieldgoals$hometeam[i])]
  }
  else {
    data.fieldgoals$temperature[i] <- NA
  }
}
for(i in 1:nrow(data.fieldgoals)) {
  if(length(weather.clean$wind_mph[which(weather.clean$datechar == data.fieldgoals$datechar[i] & weather.clean$hometeam == data.fieldgoals$hometeam[i])]) > 0) {
    data.fieldgoals$wind_mph[i] <- weather.clean$wind_mph[which(weather.clean$datechar == data.fieldgoals$datechar[i] & weather.clean$hometeam == data.fieldgoals$hometeam[i])]
  }
  else {
    data.fieldgoals$wind_mph[i] <- NA
  }
}

weather.clean$humidity <- as.character(weather.clean$humidity)
for(i in 1:nrow(data.fieldgoals)) {  
  if(length(weather.clean$humidity[which(weather.clean$datechar == data.fieldgoals$datechar[i] & weather.clean$hometeam == data.fieldgoals$hometeam[i])]) > 0) {
    data.fieldgoals$humidity[i] <- as.integer(strsplit(weather.clean$humidity[which(weather.clean$datechar == data.fieldgoals$datechar[i] & weather.clean$hometeam == data.fieldgoals$hometeam[i])],"%"))
  }
  else {
    data.fieldgoals$humidity[i] <- NA
  }
}

# Write final data
data <- data.fieldgoals[complete.cases(data.fieldgoals),]
#write.csv(data,"football_data_noBlock.csv")
# Change result to class of factor
data$result <- as.factor(data$result)

# Logistic regression
# Don't include leverage...it's really just a function of time and scorediff.
logit <- glm(result ~ scorediff + time + distance + temperature + wind_mph + humidity, data, family="binomial")
summary(logit)
# temperature has a positive coefficient, increases log odds
# all others have essentially a negative coefficient, decreasing log odds
# intuitive results: more distance, tougher to make a field goal
# higher wind mph, tougher to make a field goal
# higher humidity, air is thicker, tougher to make a field goal
# interesting that time has a negative coefficient, meaning as we are closer
# to the start of a game, the log odds, ceteris paribus, decreases

##### Now let's do some real work and split it into a test and train set
n <- nrow(data)
train <- sample(1:n, size = round(0.8*n), replace=FALSE)
data.train <- data[train,]
data.test <- data[-train,]
# train model
train.mdl <- randomForest(result ~ scorediff + time + distance + temperature + wind_mph + humidity, data.train)
data.test$pred <- predict(train.mdl,data.test)
# not very good...
#variable importance
varImpPlot(train.mdl)
#distance obviously the most important, followed by time, temperature, etc.
# Let's try some k-fold validation with k = 10
# shuffle the data
data <- data[sample(nrow(data)),]
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
falsepositiverate <- c()
falsenegrate <- c()
truenegrate <- c()
truepositiverate <- c()
# let's go through with the 10-fold validation alg
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testInd <- which(folds==i,arr.ind=TRUE)
  testData <- data[testInd, ]
  trainData <- data[-testInd, ]
  mdl <- randomForest(result ~ scorediff + time + distance + temperature + wind_mph + humidity, trainData)
  testData$pred <- predict(mdl,testData)
  # false positive
  for(j in 1:nrow(testData)) {
    if((testData$result[j] == FALSE) & (testData$pred[j] == TRUE)) {
      testData$falsepos[j] <- TRUE
    }
    else {
      testData$falsepos[j] <- FALSE
    }
  }
  # true negative
  for(j in 1:nrow(testData)) {
    if((testData$result[j] == FALSE) & (testData$pred[j] == FALSE)) {
      testData$trueneg[j] <- TRUE
    }
    else {
      testData$trueneg[j] <- FALSE
    }
  }
  # false negative
  for(j in 1:nrow(testData)) {
    if((testData$result[j] == TRUE) & (testData$pred[j] == FALSE)) {
      testData$falseneg[j] <- TRUE
    }
    else {
      testData$falseneg[j] <- FALSE
    }
  }
  # true positive
  for(j in 1:nrow(testData)) {
    if((testData$result[j] == TRUE) & (testData$pred[j] == TRUE)) {
      testData$truepos[j] <- TRUE
    }
    else {
      testData$truepos[j] <- FALSE
    }
  }
  falsepositiverate[i] <- table(testData$falsepos)[2]/nrow(testData)
  falsenegrate[i] <- table(testData$falseneg)[2]/nrow(testData)
  truepositiverate[i] <- table(testData$truepos)[2]/nrow(testData)
  truenegrate[i] <- table(testData$trueneg)[2]/nrow(testData)
}
# false positive when model says yes, but reality says no
# mean false positive rate is
mean(falsepositiverate)
barplot(falsepositiverate)
# mean false negative rate is
mean(falsenegrate)
barplot(falsenegrate)
# mean true positive rate is
mean(truepositiverate)
barplot(truepositiverate)
# mean true negative rate is
mean(truenegrate)
barplot(truenegrate)
# false positive rate is quite high. predicts that teams should "go for it" more 
# model is correct this percent of the time:
mean(truenegrate) + mean(truepositiverate)
# preferable to have more features to explore
