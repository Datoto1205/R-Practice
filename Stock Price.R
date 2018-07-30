library(zoo)
library(xts)
library(quantmod)

# Draw The Trend Plot of Stock Price
GuoJiu <- get(getSymbols("2327.TW", from = "2015-01-01", to = Sys.Date(),src = "yahoo"))
GuoJiuWithoutNull <- GuoJiu[Reduce(`&`, lapply(GuoJiu, function(x) !(is.na(x)|x==""))),] # Preclusion
chartSeries(GuoJiuWithoutNull, up.col = 'red', dn.col = 'green')

GuoJiuMonthLine <- runMean(GuoJiuWithoutNull[,4], n = 20)
GuoJiuSeasonLine <- runMean(GuoJiuWithoutNull[,4], n = 60)
addTA(GuoJiuMonthLine, on = 1, col = "orange")
addTA(GuoJiuSeasonLine, on = 1, col = "purple")

GuoJiuWeekly <- to.weekly(GuoJiuWithoutNull)
chartSeries(GuoJiuWeekly)
addBBands()
addRSI()
# In order to use these special functions, we need to use the quantmod package first.
# I could use getSymbols() function to gain the data of stock from several places, and here I fetch the data from yahoo.
# Because I could not use the data with null to draw the plot, so I need to remove the null data, which was shown in the part of "Preclusion" above.
# I could use chartSeries() function to draw the trend plot of stock price.
# In the default setting, the red bar means slump of stock price, while the green means augmentation of stock price instead, which is normal in foreign countries.
# We could use addTA() function to add the moving average lines into our extant plot.
# We could use to.weekly() function to change the daily data into weekly data.
# We could use addBBands() and addRSI() function to add several indicators, which included bollinger bands and RSI, into our plot.

# Backtesting- Moving Average
positionOfStockPriceWithMA <- Lag(ifelse(GuoJiuSeasonLine < GuoJiuMonthLine, 1, 0))
rateOfReturnOfMA <- ROC(Cl(GuoJiuWithoutNull)) * positionOfStockPriceWithMA

rateOfReturnOfMAInLimitedTime <- rateOfReturnOfMA['2017-01-01/2018-06-01']
rateOfReturnOfMAInLimitedTimeWithoutNull <- rateOfReturnOfMAInLimitedTime[Reduce(`&`, lapply(rateOfReturnOfMAInLimitedTime, function(x) !(is.na(x)|x==""))),] # Preclusion

accumulatedRateOfReturnOfMA <- exp(cumsum(rateOfReturnOfMAInLimitedTimeWithoutNull))
plot(accumulatedRateOfReturnOfMA)

# Backtesting- RSI
positionOfStockPriceWithRSI <- Lag(ifelse(RSI(GuoJiuWithoutNull[,4]) > 60, 1, 0))
rateOfReturnOfRSI <- ROC(Cl(GuoJiuWithoutNull)) * positionOfStockPriceWithRSI

rateOfReturnOfRSIInLimitedTime <- rateOfReturnOfRSI['2017-01-01/2018-06-01']
rateOfReturnOfRSIInLimitedTimeWithoutNull <- rateOfReturnOfRSIInLimitedTime[Reduce(`&`, lapply(rateOfReturnOfRSIInLimitedTime, function(x) !(is.na(x)|x==""))),] # Preclusion

accumulatedRateOfReturnOfRSI <- exp(cumsum(rateOfReturnOfRSIInLimitedTimeWithoutNull))
plot(accumulatedRateOfReturnOfRSI)
# I could use Cl() function to gain the time series data, and we could use the ROC() function to calculate the rate of change.
# I could use exp() function to gain the exponential value, and we could use the cumsum() function to get the accumulate sum which is a vector.
# I could change different strategies and edit the codes above.


# To get more information about quantmod package, please visit the website below at: 
# 1. "http://tn00343140a.pixnet.net/blog/post/4752792-r語言-quantmod包的使用"
# 2. "http://www.bituzi.com/2016/05/TSMCtrading.html"
# 3. "https://blog.stranity.com.tw/2017/11/13/r-量化分析套件-－-quantmod-ttr/"

# Install the package we need. (It only need to be done for one time.)
#install.packages("quantmod")
