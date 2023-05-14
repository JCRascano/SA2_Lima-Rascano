#Paul Carlos Lima and Jan Celine Rascano
#Summative Assessment #2

#For Item 1 
library(anytime)
#library(dplyr)
library(plyr)
library(dgof)
library(fitdistrplus)
library(VGAM)
library(DataCombine)
library(data.table)
library(EnvStats)
library(ggplot2)
library(tsallisqexp)
library(poweRlaw)
library(fitur)

# install.packages("data.table"), if wala pa
#Gives a summary of the data
summary(BTC.Data.with.Datetime)

#This will check the field names
str(BTC.Data.with.Datetime)
names(BTC.Data.with.Datetime)

BTC.Data.with.Datetime <- BTC.Data.with.Datetime[complete.cases(BTC.Data.with.Datetime$Close), ]

BTC.Data.with.Datetime$Data <- anydate(BTC.Data.with.Datetime[1, "UNIX.Timestamp..ms."])

BTC.Data.with.Datetime <- replace(BTC.Data.with.Datetime, is.na(BTC.Data.with.Datetime), 0)

# This will give a summary of the data
summary(BTC.Data.with.Datetime)

# This will check the field names
str(BTC.Data.with.Datetime)
names(BTC.Data.with.Datetime)

# This will filter the Data to remove rows with missing values in the "Close" column
BTC.Data.with.Datetime <- na.omit(BTC.Data.with.Datetime)

# This will converts the first row's UNIX timestamp to a Date format and assigns it to the "Data" column
BTC.Data.with.Datetime$Data <- anytime::anydate(BTC.Data.with.Datetime[1, "UNIX.Timestamp..ms."])

# This will replace NA values with 0 in the entire data frame
BTC.Data.with.Datetime[is.na(BTC.Data.with.Datetime)] <- 0

# This will perform the remaining calculations using data.table syntax
library(data.table)

dfud <- data.table(BTC.Data.with.Datetime)
x <- dfud[, .(Low = min(Low)), by = "Data"]
setnames(x, "Group.1", "Data")
setnames(x, "Low", "Low")
y <- dfud[, .(High = max(High)), by = "Data"]
setnames(y, "Group.1", "Data")
setnames(y, "High", "High")
dfud <- merge(x, y, by = "Data", all = TRUE)

dfud$return <- NaN
dfud$middle <- NaN
dfud[1, middle := (High - Low) / 2 + Low]
for (i in 2:nrow(dfud)) {
  dfud[i, middle := (High - Low) / 2 + Low]
  dfud[i, return := (middle - shift(middle, 1, fill = 0)) / shift(middle, 1, fill = 0)]
}

#hist(DropNA(dfud$return), breaks = 10)
qplot(DropNA(dfud$return),
      geom = "histogram",
      binwidth = 0.005,
      main = "Histogram of Return - Bitcoin x USD",
      xlab = "Return",
      fill = I("blue"),
      col = I("red"),
      alpha = I(.2),
      xlim = c(-0.3, 0.3))

# For the Normal:
dfud_test <- rnorm(length(DropNA(dfud$return)), mean = mean(DropNA(dfud$return)), sd = sd(DropNA(dfud$return)))
ks.test(DropNA(dfud$return), dfud_test)

# For the Student:
dfud_test <- rt(length(DropNA(dfud$return)), length(DropNA(dfud$return)) - 1)
ks.test(DropNA(dfud$return), dfud_test)

# For the Laplace:
dfud_test <- rlaplace(length(DropNA(dfud$return)), mean(DropNA(dfud$return)), sd(DropNA(dfud$return)))
ks.test(DropNA(dfud$return), dfud_test)

# For the Tsallis:
dfud_test <- rtsal(length(DropNA(dfud$return)), mean(DropNA(dfud$return)), sd(DropNA(dfud$return)))
ks.test(DropNA(dfud$return), dfud_test)

# For the PowerLaw:
dfud_test <- rplcon(length(DropNA(dfud$return)), -0.3, sd(DropNA(dfud$return)))
ks.test(DropNA(dfud$return), dfud_test)
#End of Item 1 




#For Item 2
library(data.table)
library(zoo)

# Icoconvert BTC.Data.with.Datetime to a data.table
BTC.Data.with.Datetime <- as.data.table(BTC.Data.with.Datetime)

# Ichecheck ang structure of the data table
str(BTC.Data.with.Datetime)

# Ichecheck ang first few rows of the data table
head(BTC.Data.with.Datetime)

# Ifill ang missing values with the last observation
BTC.Data.with.Datetime[, Close := na.locf(Close)]

# Icacalculate ang returns from the Ethereum trading data
returns <- c(NA, diff(Close) / shift(Close))
BTC.Data.with.Datetime[, retorno := returns[1:.N]]

# Ireremove NA values
ETH.Data <- na.omit(BTC.Data.with.Datetime)

# Ipeperform Shapiro-Wilk normality test
shapiro.test(ETH.Data$retorno)
#End of #2

  