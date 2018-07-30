# Single Variable Linier Regression
library(readr)
dataset <- read_csv("/Users/lizhengen/Documents/GitHub/R-Practice/TestData.csv")
model1 <- lm(height~weight, data = dataset)
summary(model1)
attributes(model1)
model1$coefficients
anova(model1)

plot(dataset$weight, dataset$height, main = "Scatterplot")
abline(model1, col = 3, lwd = 3 )
hist(dataset$height, breaks = 60, freq = FALSE, col = colors()[c(112, 10)], main = "Histogram")
hist(dataset$weight, ylab = "number of people", xlab = "weight", main = "Histogram2")

# Remember the order: lm(Y~X)
# The number in the row of estimate indicates the coefficient of variables in the model.
# If α equal to 0.05, we would reject the null hypothesis if the p-value is lower than 0.05, but it did not indicate that the opposite hypothesis (the things we want to prove) is correct.
# If in the situation of 0.05 of α, then we would reject the hypothesis if the f value, in this case, is lower than f0.05.

# R-square was used to measure how observed outcomes are replicated by the model, its number is between 0 and 1, and larger value of R-square means that the model could replicate (擬合) the outcome well.
# When we add more variables into our model, no matter whether the new variables are related to the dependent variable, the R-square would still increase. Thus, we could use adjusted R-square to substitute the R-square so that the new R-square would increase only when the new variables were related to the dependent variable.
# We could use attribute() function to get the attributes of the object of the model.
# ANOVA() function is used to do the analysis of ANOVA.

# We could use plot(X, Y) to plot the scatter plot, and we could use abline(the name of the regression model, color code, width of the line) function to add the regression line on the graph.
# We could use hist(data, number of the group, color) to draw a histogram.

# More information about R-square could be found at https://en.wikipedia.org/wiki/Coefficient_of_determination
# More information about adjusted R-square could be found at https://tw.answers.yahoo.com/question/index?qid=20051106000014KK07522
# More information about ANOVA could be found at http://belleaya.pixnet.net/blog/post/30754486-%5B教學%5D-%5B統計%5D-anova變異數分析-小筆記
# More information about the code of color in R could be found at http://research.stowers.org/mcm/efg/R/Color/Chart/

# Change the elements of texts into dummy
sexVector <- c()
for(i in 1:length(dataset$Sex)) {
  if(dataset$Sex[i] == "女"){
    sexVector[i] <- 0
  } else {
    sexVector[i] <- 1
  }
}
dataset[5] <- sexVector

# Multiple Regression & Logistic Regression
model2 <- lm(height~weight + sexVector + Grade, data = dataset)
summary(model2)

plot(dataset$height, sexVector)
logis1 <- glm(sexVector~height, data = dataset, family = binomial())
summary(logis1)
confint(logis1, level = 0.90)
predict(model1, newdata = data.frame(weight = 60))

# When the dependent variable is binomial (dummy), it is incorrect for us to use the normal linear regression model to replicate it. Instead, we need to use the logistic one to substitute it, and we could use glm() function to do that.
# Remember to set the property of binomial when you use the logistic regression, or the result of glm() would be as the same as that of normal regression.
# confint() function is used to get the confident interval, we could get the confident interval of coefficients of variables here. (For example, if the confidence level is 0.90, and the result is -0.5 and 0.1, it means that the probability that true coefficient is located in the interval between -0.5 and 0.1 is 95%.)
# predict() function could be used to predict the value of the dependent variable if we give a value of independent variables. Remember to transform the data you want to give to the type of data frame beforehand.
# To get more information about glm() function, please visit the website at https://www.youtube.com/watch?v=Yv05RjKpEKY

# Decision Tree - Sort the data beforehand
library(tree)
dataOfOccupation <- read_csv("/Users/lizhengen/Documents/GitHub/R-Practice/TestData2.csv")
dataWeWantToUseToBuildADecisionTree <- data.frame(Grade = dataset$Grade[1:70], Sex = sexVector[1:70], Height = dataset$height[1:70], Weight = dataset$weight[1:70], Occupation = dataOfOccupation$Occupation[1:70])
dataWeWantToUseToBuildADecisionTreeWithoutNull <- dataWeWantToUseToBuildADecisionTree[Reduce(`&`, lapply(dataWeWantToUseToBuildADecisionTree, function(x) !(is.na(x)|x==""))),]

# Decision Tree - Create and prune the tree
result <- tree(Occupation ~ Height + Weight + Grade, data = dataWeWantToUseToBuildADecisionTreeWithoutNull)
plot(result)
text(result)

set.seed(1000000000)
costOfComplexity <- cv.tree(result, FUN = prune.misclass)
plot(costOfComplexity)

treeAfterBePrunned <- prune.tree(result, best = 8)
plot(treeAfterBePrunned)
text(treeAfterBePrunned)

# Decision Tree - Test the accuracy of decision tree
dataWeWantToUseToTestADecisionTree <- data.frame(Grade = dataset$Grade[1:30], Sex = sexVector[1:30], Height = dataset$height[1:30], Weight = dataset$weight[1:30], Occupation = dataOfOccupation$Occupation[1:30])
dataWeWantToUseToTestADecisionTreeWithoutNull <- dataWeWantToUseToBuildADecisionTree[Reduce(`&`, lapply(dataWeWantToUseToBuildADecisionTree, function(x) !(is.na(x)|x==""))),]

testResult <- predict(result, dataWeWantToUseToTestADecisionTreeWithoutNull, type = "class")
testResultMatrix <- table(dataWeWantToUseToTestADecisionTreeWithoutNull$Occupation, testResult, dnn = c("Real", "Predicted"))
accuracyOfDecisionTree <- sum(diag(testResultMatrix)) / sum(testResultMatrix)
accuracyOfDecisionTree

# There are several different packages concerning of decision tree, and here we use the package "tree".
# We need to create a data frame as the training group of our decision tree, or we could not change the column name.
# Before we build the real decision tree, we need to remove the row with null values, so we refer to the following website to do that. (https://stackoverflow.com/questions/42721788/filter-empty-rows-from-a-dataframe-with-r)
# When you use the tree() function to create the decision tree, remember that you need to change the type of the data you want to train -- here is Occupation -- into "factor".
# We could use plot() and text() function to draw the decision tree.
# We could use cv.tree() function to calculate the deviance of k-value so that we could know the cost of complexity of this decision tree, and we could decide the minimum of the k value.
# After we know the cost of complexity, we could use prune.tree() function to lower the number of node in the decision tree.

# I could refer to the following website to get more information about decision tree:
# 1. https://ithelp.ithome.com.tw/articles/10187561
# 2. https://www.youtube.com/watch?v=GOJN9SKl_OE
# 3. https://blog.gtwang.org/r/r-strings-and-factors/4/
# 4. https://blog.stranity.com.tw/2016/10/17/r語言的決策樹應用/

# Install the package we need. (It only need to be done for one time.)
#install.packages("tree")
