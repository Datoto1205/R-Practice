# Function
specialFunctionWithSingleParameter = function(originalNumber = 10) {
  return(originalNumber + 20)
}

specialFunctionWithSingleParameter(30)
specialFunctionWithSingleParameter()
# 1. I did not need to type the type of data the function returned in R.
# 2. I could set a default value of parameter when I declare the function, it would be used if I did not give any value of parameter to this function.



# Function And Vector
specialFunctionWithVectorParameter = function(originalVector) {
  processedVector = rev(sort(originalVector))
  return(var(processedVector[1:5]))
}
testVector <- c(1, 10, 35, 199, 26, 83, 964, 5, 69, 287)
specialFunctionWithArrayParameter(testVector)
# If I want to import several value as the parameter of an function, I could take into account vector.



# Optimize Function with Single Variable
normalSingleVariableFunction = function(x) {
  x^2+1
}
optimize(normalSingleVariableFunction, c(-100, 100))
# I could use optimize() function to get the solution of a function with single variable. But I need to list out the function first.



# Generate Random Variable
runif(1, min = 0, max = 100)
plot(runif(10000, min = 0, max = 100))
hist(runif(10000, min = 0, max = 100))

set.seed(1)
runif(1, min = 0, max = 100)

rnorm(1, mean = 50, sd = 10)
plot(rnorm(10000, mean = 50, sd = 10))
hist(rnorm(10000, mean = 50, sd = 10))

rexp(1, 1/10)
hist(rexp(10000, 1/10))
# 1. I could use runif(NumberOfRandomVariable, min, max) function to obtain several random variables which were follow uniform distribution.
# 2. I could use "set.seed()" before the codes of generating random variables to fixed the random variable.
# 3. I could use rnorm(NumberOfRandomVariable, mean, std) function to obtain several random variables which were follow normal distribution.
# 4. I could use rexp(NumberOfRandomVariable, lamda) function to obtain several random variables which were follow exponential distribution.
# 5. I could use plot() function and hist() function to visualize the distribution of random variable.



# Random Selection
originalSample = c("Tina", "Cindy", "Michelle", "Hsuan")
sample(originalSample, 1, replace = FALSE)
sample(originalSample, 10, rep = T)
# I could use sample(data, times, ...) function to generate random selection.



# Change the Data We Imported
originalData = read.csv("/Users/Lizhengen/Documents/GitHub/R-Practice/TestData.csv")
transform(originalData, Grade = Grade - 10)
originalData$sexReplace = NULL
originalData$SexReplace[originalData$Sex == '男'] <- "B"
originalData$SexReplace[originalData$Sex == '女'] <- "G"
originalData
# 1. We could use transform() function to change the value of data with particular rule.
# 2. If we want to change the value with particular rule of interval, we could create a null vector first. Afterward, we could use the way of vector to transform the value.



# Disposition of Null
particularData <- originalData[ 90:95,]
complete.cases(particularData)
print(na.omit(particularData))
# 1. I could use complete.cases(fata) function to check a form whether there's null data in it.
# 2. I not only could use no.omit(data) dunction to delete the row with null data, but I also could use other ways, such as mwan imputation or multiple imprtation, to replace to null data.



# Graphs
ExploratoryDataAnalysis = function(x) {
  hist(x);
  boxplot(x, horizontal = T);
  qqnorm(x);
  qqline(x);
}
ExploratoryDataAnalysis(originalData$Grade)

originalData$Sex
SexReplaceAfterClassification <- table(originalData$SexReplace)
names(SexReplaceAfterClassification) <- c("Girls", "Boys")
pie(SexReplaceAfterClassification, col = c("white", "gray"))
# 1. I could use boxplot() function to draw box plot, I could use hist() function to draw histogram, and I could use qqnorm() to draw a Q-Q plot.
# 2. I could use table() function to classify the data.
# 3. I could use pie() function to draw a pie chart. But the prerequisite is that we need to classify the data first.
# 4. We could use names() function to change the title of each part in the pie chart.


# Calculation of correlation
originalDataWithoutNull <- na.omit(particularData)
cor(originalDataWithoutNull$height, originalDataWithoutNull$weight)
# Before I do any calculation, make sure I need to clear all of the Null data.



# Attach & Detach
height
originalData$height
attach(originalData)
height
detach(originalData)
height
# I could use attach() function and detach() function to extract and put back specific data in a form/list.








# install.packages("mice")

