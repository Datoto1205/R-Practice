## Function
specialFunctionWithSingleParameter = function(originalNumber = 10) {
  return(originalNumber + 20)
}

specialFunctionWithSingleParameter(30)
specialFunctionWithSingleParameter()
## 1. I did not need to type the type of data the function returned in R.
## 2. I could set a default value of parameter when I declare the function, it would be used if I did not give any value of parameter to this function.



# Function And Vector
specialFunctionWithVectorParameter = function(originalVector) {
  processedVector = rev(sort(originalVector))
  return(var(processedVector[1:5]))
}
testVector <- c(1, 10, 35, 199, 26, 83, 964, 5, 69, 287)
specialFunctionWithArrayParameter(testVector)
## If I want to import several value as the parameter of an function, I could take into account vector.



## Optimize Function with Single Variable
normalSingleVariableFunction = function(x) {
  x^2+1
}
optimize(normalSingleVariableFunction, c(-100, 100))
## I could use optimize() function to get the solution of a function with single variable. But I need to list out the function first.



## Generate Random Variable
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
## 1. I could use runif(NumberOfRandomVariable, min, max) function to obtain several random variables which were follow uniform distribution.
## 2. I could use "set.seed()" before the codes of generating random variables to fixed the random variable.
## 3. I could use rnorm(NumberOfRandomVariable, mean, std) function to obtain several random variables which were follow normal distribution.
## 4. I could use rexp(NumberOfRandomVariable, lamda) function to obtain several random variables which were follow exponential distribution.
## 5. I could use plot() function and hist() function to visualize the distribution of random variable.
