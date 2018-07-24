# Load the library we need.
library(readr)
library(httr)
library(XML)
library(lubridate)

# cat()
cat("Hello world")
cat(matrix(c(1, 2, 3, 4), 2, 2))

# print()
print("Hello world")
print(matrix(c(1, 2, 3, 4, 5, 6), 3, 2))

# cat() could not print out some type of data completely (no direction), such as the matrix. In this case, we could use print() function to finish this work.
# Remember that print() function is a good tool to examine the bug in the codes.
# c() function could create a vector, which could be seen as a data set, I could add some components into it.
# matrix() function could create a matrix, the second parameter means the number of rows, and the third parameter means the number of columns.
# The tutorials on wrapping the codes according to the size of the screen: https://d.cosx.org/d/110673-110673

# Assign the value
firstInteger <- 1
secondDouble <- 0.5
firstVector <- c(1,2,3,4,5)
secondVector <- c(10,20,30,40,50)
thirdVector <- c(2, 4, -1, 3, 10)
forthVector <- c(-1, -2, -3, -4, -5)

# Fundamental statistics
mean(firstVector)
median(firstVector)
var(firstVector)
sd(firstVector)
cor(firstVector, secondVector)
cor(firstVector, thirdVector)
cov(firstVector, secondVector)
cov(firstVector, thirdVector)

# We could use "<-" to assign a value to the parameters, we did not need to declare the "int" or "double", the system would help us to do that automatically.
# We could use mean() function, median() function, var() function and sd() function to get the mean, median, variance and standard variation of several numbers.
# Correlation is used to evaluate the relationship of the trend of the change between two different data sets if the trend of the moving of the second data set is as the same as that of the first data set, then the value of correlation is 1, vice versa.
# Covariance is used to evaluate the extent of the relationship of the trend of the change between two different datasets, too. The function of co-variance is similar to that of correlation. However, the value of correlation is between -1 and 1, you could not know more magnitude of it. With the application of covariance, the value would be magnified so that we could know the extent of it.
# To get more information about co-variance, please visit here. (https://zh.wikipedia.org/wiki/协方差) （http://www.stat.nuk.edu.tw/cbme/math/statistic/sta2/s3_5/bud.html）

# Sequence and vector
1:5
seq(from = 0, to = 10, by = 2)
rep(1, times = 5)
firstVector[2]
firstVector[2:4]
firstVector[ firstVector < 4 ]
secondVector - firstVector
cbind(firstVector, secondVector)
rbind(firstVector, secondVector)

# ":" could be used to show the sequence of numbers between two number. However, the tolerance of it is only 1. If you want to change it, you need to use another function, such as seq() function.
# rep() function could be used to get a series of repetitive numbers.
# [] behind a vector could be used to gain a component in that factor.
# We could do the calculation of two vectors directly, each component in the vectors would be changed independently.
# cbind() and rbind() functions could be used to combine several different vectors and create a matrix.

# Define our own individual function
resultOfFunction <- function(a, b) {
  if (a < 2 && b > 6) {
    return("Success")
  } else {
    return("Failure")
  }
}

resultOfFunction(1, 7)
resultOfFunction(2, 6)

# We could set our own individual function by the way above.
# In loop, "&&" means "and", and "||" means "or". 

# For loop & if
theResultOfFactorial <- 1
theSumOfConsecutiveSeriesOfNumber <- 0
for( i in ( 0 : 10 )) {
  if( i < 1 ) {
  theSumOfConsecutiveSeriesOfNumber = theSumOfConsecutiveSeriesOfNumber + ( i + 1 )
  } else if( i > 9 ) {
  theSumOfConsecutiveSeriesOfNumber = theSumOfConsecutiveSeriesOfNumber
  } else {
  theResultOfFactorial = theResultOfFactorial * ( i + 1 )
  theSumOfConsecutiveSeriesOfNumber = theSumOfConsecutiveSeriesOfNumber + ( i + 1 )
  }
}

print(theResultOfFactorial)
print(theSumOfConsecutiveSeriesOfNumber)

theSumOfConsecutiveSeriesOfNumber <- 0

# For loop & if & break & next
for( i in (0:5)) {
  if( i == 3) {
    next
  } else if( i == 5) {
    break
  } else {
    theSumOfConsecutiveSeriesOfNumber = theSumOfConsecutiveSeriesOfNumber + ( i + 1 )
  }
  
  print(theSumOfConsecutiveSeriesOfNumber)
}

# The codes above is the application of for-loop. Notice that we did not need to declare "i" beforehand, and i would increase each time after it finishes all the codes insides the loop.
# If we put "next" in the for-loop, the computer would skip that time; if we put "break" in the for-loop instead, this loop would suspend.

# Data frame
dataOfGame <- data.frame(nameOfCharacter = c("A", "B", "C", "D", "E"), levelOfCharacter = c(10, 20, 30, 40, 50), occupationOfCharacter = c("Magician", "Warrior", "Thief", "Bowmen", "Pirate"), positionOfJob = c(1, 1, 2, 2, 2))
colnames(dataOfGame)
row.names(dataOfGame) <- c("甲", "乙", "丙", "丁", "戊")
str(dataOfGame)
length(dataOfGame$nameOfCharacter)
nrow(dataOfGame)
dim(dataOfGame)
plot(dataOfGame$levelOfCharacter, dataOfGame$positionOfJob)

# data.frame() function could be used to create a particular type of data which called data frame.
# We could use colnames() function and row.names() function to get the name of column and row of the data frame, and we also could assign another vector to modify it later.
# str() function is used to get the type of data in the data frame.
# length() function could be used to obtain the number of columns, and dim() function could be used to get the number of column and row of data frame simultaneously.
# plot() function could be used to plot a primitive dot graph.

# Multiply the matrix
matrix(c(1, 2, 3, 4, 5, 6), 3, 2) %*% matrix(c(1, 2, 3, 4), 2, 2)

# List
dataset <- read_csv("/Users/lizhengen/Documents/GitHub/R-Practice/TestData.csv")
names(dataset)
dataset[1, ]
dataset[ ,1]
dataset$Sex

dataset$color <- c("black", "yellow", "yellow", "white", "brown")
dataset
dataset$color <- NULL
dataset
write.csv(dataset, file = "Renew", row.names = TRUE)

# %*% is used to multiply different matrices.
# read_csv() function is used to import the file of CSV, remember that I need to use "/" instead of "\" (because of my operating system), and I also need to plus ".csv" at the end of my file name. I could search the path of the file with the terminal. (Remember to run the package of readr first.)
# I could use other functions to import other types of data, go to the following website to get more information: https://yijutseng.github.io/DataScienceRBook/io.html
# I could use "$" to extract particular columns from the list.
# I could use "c()" & "$" to add more column into the list, and I also could use "NULL" to delete a column of a list.
# Function write.csv() could be used to export the file we edited to CVS file.

# Check the type of variables
is.numeric(firstInteger)
is.character("Hi")
is.logical(TRUE)
class(firstInteger)
ymd('2018/7/1')
mdy('7/1/2018')
tolower("I LOVE YOU")
toupper("i hate you")
# We could use is.x() functions to check the type of variable, and we could use class() function to obtain the type of variable.
# ymd() and mdy() functions could be used to change the type of text into "date".
# tolower() and toupper() functions could be used to change the capital of characters.


# Install the package we need. (It only need to be done for one time.)
#install.packages("readr")
#install.packages("httr")
#install.packages("XML")
#install.packages("lubridate")
