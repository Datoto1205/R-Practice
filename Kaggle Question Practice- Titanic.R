rawDataOfTitanic<- read.csv("/Users/Lizhengen/Documents/GitHub/R-Practice/Data of Titanic/train.csv")



# Delete the Null Data
complete.cases(rawDataOfTitanic)
rawDataOfTitanicWithoutNull <- na.omit(rawDataOfTitanic)
summary(rawDataOfTitanicWithoutNull)


# Survive Rate
tableOfSurviveData <- table(rawDataOfTitanicWithoutNull$Survived)
names(tableOfSurviveData) <- c("Dead", "Survive")

TotalNumberOfPeople <- tableOfSurviveData[1]+tableOfSurviveData[2]
percentageLabel <- c(round(tableOfSurviveData[1]/TotalNumberOfPeople, 4), round(tableOfSurviveData[2]/TotalNumberOfPeople, 4))

colorOfPieChart <- c("white", "gray")
pie(tableOfSurviveData, labels = percentageLabel, col = colorOfPieChart, main = "Total Survived Rate", radius = 0.8)

legend("topright", names(tableOfSurviveData), fill = colorOfPieChart)



# Pclass
tableOfNumberOfSurvivedAndPclass <- table(rawDataOfTitanicWithoutNull$Survived, rawDataOfTitanicWithoutNull$Pclass)
barplot(tableOfNumberOfSurvivedAndPclass, beside = T, legend.text = c("Dead", "Survive"), xlab = "Pclass", ylab = "Number of people")



# Pclass
tableOfNumberOfSurvivedAndSex <- table(rawDataOfTitanicWithoutNull$Survived, rawDataOfTitanicWithoutNull$Sex)
barplot(tableOfNumberOfSurvivedAndSex, beside = T, legend.text = c("Dead", "Survive"), xlab = "Sex", ylab = "Number of people")



# Age
tableOfNumberOfSurvivedAndAge <- table(rawDataOfTitanicWithoutNull$Age, rawDataOfTitanicWithoutNull$Survived)

par(mfrow = c(1, 2))
barplot(tableOfNumberOfSurvivedAndAge[,1], main = "Distribution of Age of Dier", xlab = "Age", ylab = "Number of People", ylim = c(0, 20))
abline(v = 21)
abline(v = 73)

barplot(tableOfNumberOfSurvivedAndAge[,2], main = "Distribution of Age of Survivor", xlab = "Age", ylab = "Number of People", ylim = c(0, 20))
abline(v = 21)
abline(v = 73)



# Distribution of Number of Sibling and Spouse
tableOfNumberOfSurvivedAndSibSp <- table(rawDataOfTitanicWithoutNull$SibSp, rawDataOfTitanicWithoutNull$Survived)
barplot(tableOfNumberOfSurvivedAndSibSp[,1], beside = T, main = "Distribution of SibSp of Diers", ylim = c(0, 310), xlab = "Number of Sibling and Spouse", ylab = "Number of People")
barplot(tableOfNumberOfSurvivedAndSibSp[,2], beside = T, main = "Distribution of SibSp of Survivors", ylim = c(0, 310), xlab = "Number of sibling and spouse", ylab = "Number of People")



# Distribution of Number of Parents and Descendant
tableOfNumberOfSurvivedAndParch <- table(rawDataOfTitanicWithoutNull$Parch, rawDataOfTitanicWithoutNull$Survived)
tableOfNumberOfSurvivedAndParch
barplot(tableOfNumberOfSurvivedAndParch[, 1], beside = T, main = "Distribution of Parch of Diers", ylim = c(0, 350), xlab = "Number of Parents and Descendant", ylab = "Number of People")
barplot(tableOfNumberOfSurvivedAndParch[, 2], beside = T, main = "Distribution of Parch of Survivors", ylim = c(0, 350), xlab = "Number of Parents and Descendant", ylab = "Number of People")



# Fares
vectorOfTimesOfDistributionOfDier <- c(rep(0, 30))
vectorOfTimesOfDistributionOfSurvivor <- c(rep(0, 30))

for (i in 1:30) {
  for (j in 1:nrow(rawDataOfTitanicWithoutNull)) {
    if ((rawDataOfTitanicWithoutNull$Survived[j] == 0) &&
        (10 * (i - 1) <= rawDataOfTitanicWithoutNull$Fare[j]) &&
        (rawDataOfTitanicWithoutNull$Fare[j] < (10 * i))) {
      vectorOfTimesOfDistributionOfDier[i] = vectorOfTimesOfDistributionOfDier[i] + 1
    } else if ((rawDataOfTitanicWithoutNull$Survived[j] == 1) &&
               (10 * (i - 1) <= rawDataOfTitanicWithoutNull$Fare[j]) &&
               (rawDataOfTitanicWithoutNull$Fare[j] < (10 * i))) {
      vectorOfTimesOfDistributionOfSurvivor[i] = vectorOfTimesOfDistributionOfSurvivor[i] + 1
    } else {
      
    }
  }
}

barplot(vectorOfTimesOfDistributionOfDier, xlim = c(0, 30), ylim = c(0, 200), main = "Distribution of Fares Dier Spent", xlab = "Amount of Fares", ylab = "Number of People")
barplot(vectorOfTimesOfDistributionOfSurvivor, xlim = c(0, 30), ylim = c(0, 200), main = "Distribution of Fares Survivors Spent", xlab = "Amount of Fares", ylab = "Number of People")



# Cabin
tableOfNumberOfSurvivedAndCabin<- table(rawDataOfTitanicWithoutNull$Cabin, rawDataOfTitanicWithoutNull$Survived)
PercentageOfSurvivedRateWithoutCabinNumber <- round(tableOfNumberOfSurvivedAndCabin[1, 1]/(tableOfNumberOfSurvivedAndCabin[1, 1] + tableOfNumberOfSurvivedAndCabin[1, 2]), 4)
PercentageOfDyingRateWithoutCabinNumber <- 1 - PercentageOfSurvivedRateWithoutCabinNumber

pie(c(tableOfNumberOfSurvivedAndCabin[1, 1], tableOfNumberOfSurvivedAndCabin[1, 2]), col = c("white", "gray"), labels = c(PercentageOfSurvivedRateWithoutCabinNumber, PercentageOfDyingRateWithoutCabinNumber), main = "Distribution of Passengers Without Cabin Number")
legend("bottom", c("Dead", "Survived"), fill = colorOfPieChart)

NumberOfDyingRateWithCabinNumber = 0
NumberOfSurvivedRateWithCabinNumber = 0
for (i in 2:nrow(tableOfNumberOfSurvivedAndCabin)) {
  NumberOfDyingRateWithCabinNumber = tableOfNumberOfSurvivedAndCabin[i, 1] + NumberOfDyingRateWithCabinNumber
  NumberOfSurvivedRateWithCabinNumber = tableOfNumberOfSurvivedAndCabin[i, 2] + NumberOfSurvivedRateWithCabinNumber
}

PercentageOfSurvivedRateWithCabinNumber <- round(NumberOfSurvivedRateWithCabinNumber / (NumberOfSurvivedRateWithCabinNumber + NumberOfDyingRateWithCabinNumber), 4)
PercentageOfDyingRateWithCabinNumber <- 1 - PercentageOfSurvivedRateWithCabinNumber


PercentageOfSurvivedRateWithCabinNumber

pie(c(NumberOfSurvivedRateWithCabinNumber, NumberOfDyingRateWithCabinNumber), col = c("white", "gray"), labels = c(PercentageOfSurvivedRateWithCabinNumber, PercentageOfDyingRateWithCabinNumber), main = "Distribution of Passengers With Cabin Number")
legend("bottom", c("Dead", "Survived"), fill = colorOfPieChart)



#Embark
par(mfrow = c(1, 1))
tableOfNumberOfSurvivedAndEmbark <- table(rawDataOfTitanicWithoutNull$Survived, rawDataOfTitanicWithoutNull$Embarked)
tableOfNumberOfSurvivedAndEmbark
barplot(tableOfNumberOfSurvivedAndEmbark[, 2:4], beside = T, main = "Distribution of Harbors Passengers Embarked", legend.text = c("Dead", "Survived"), xlab = "Name of Harbor", ylab = "Number of People")



# Linear-Regression
LinearRegressionModel <- lm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = rawDataOfTitanicWithoutNull)
summary(LinearRegressionModel)

modifiedLinearRegressionModel <- lm(Survived~Pclass+Sex+Age+SibSp, data = rawDataOfTitanicWithoutNull)
summary(modifiedLinearRegressionModel)



# Prediction
rawTestDataOfTitanic<- read.csv("/Users/Lizhengen/Documents/GitHub/R-Practice/Data of Titanic/test.csv")
ResultOfTestDataOfTitanic<- read.csv("/Users/Lizhengen/Documents/GitHub/R-Practice/Data of Titanic/gender_submission.csv")
complete.cases(rawTestDataOfTitanic)

#badge <- c(0)
#numberOfHiHi <- 1
#
#for (row in 1:nrow(rawTestDataOfTitanic)) {
#  for (column in 1:ncol(rawTestDataOfTitanic)) {
#    if (is.na(rawTestDataOfTitanic[row, column]) == TRUE) {
#      badge[numberOfHiHi] = row
#      numberOfHiHi = numberOfHiHi + 1
#    } else {
#     
#    }
#  }
#}

rawTestDataOfTitanic[, 12] <- ResultOfTestDataOfTitanic[, 2]
rawTestDataOfTitanicWithoutNull <- na.omit(rawTestDataOfTitanic)
predict(modifiedLinearRegressionModel, rawTestDataOfTitanicWithoutNull)



# Generalized Linear Regression Model & Prediction 
GLMModel <- glm(Survived~Pclass+Sex+Age+SibSp, family = binomial(link = "probit"), data = rawDataOfTitanicWithoutNull)
summary(GLMModel)

predictionOfGLM <- predict(GLMModel, rawTestDataOfTitanicWithoutNull, type = "response")

for (i in 1:nrow(rawTestDataOfTitanicWithoutNull)) {
  if (predictionOfGLM[i] < 0.5) {
    predictionOfGLM[i] = 0
  } else {
    predictionOfGLM[i] = 1
  }
}



# Check the Accuracy
correct = 0
incorrect = 0
accuracy = 0

for (m in 1:nrow(rawTestDataOfTitanicWithoutNull)) {
  if (predictionOfGLM[m] == rawTestDataOfTitanicWithoutNull[m, 12]) {
    correct = correct + 1
  } else {
    incorrect = incorrect + 1
  }
}
accuracy = (correct / (correct + incorrect)) * 100
print(round(accuracy, 2))

