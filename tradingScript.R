library(gdata)

filename <- readline(prompt = "Enter a filename for an Excel spreadsheet: ")

data1 <- read.xls(filename, sheet = 1, stringsAsFactors = FALSE)
data2 <- read.xls(filename, sheet = 2, stringsAsFactors = FALSE)

# Stores a value for the starting date of the study
startDate = which(data2[,1]=='6/1/1990')

conditionA <- function(column1, column2)
{
	return(column1 > 90 && column2 < 50)
}

# VERY VERY IMPORTANT!!!!
# THERE IS AN OFFSET BETWEEN THE TWO WORKSHEETS. MAKE SURE THIS IS TAKEN CARE OF!!!!

lastBuyDate = 50

runTradingStrategy1 <- function()
{
trades <- c()
for(i in startDate:(length(data1[,1])-32))
{
	if(as.numeric(strsplit(data1[i+1, length(data1)],"%")) < 50 && as.numeric(strsplit(data2[i, length(data2)],"%")) > 90 && (lastBuyDate + 10) < i)
	{
		trades <- rbind(trades, c(data1[i + 2, 1], data1[i + 32, 1], data2[i, length(data2)], data1[i+1, length(data1)], data2[i+1, 5], data2[i+31, 5],(data2[i+31, 5] - data2[i+1, 5]) / data2[i + 31, 5]))
		lastBuyDate = i + 1
	}
}
write.table(trades, file = "data1.csv", sep = ",", row.names=FALSE)
}

runTradingStrategy2 <- function()
{
trades <- c()
for(i in startDate:(length(data1[,1])-32))
{
	if(as.numeric(strsplit(data1[i+1, length(data1)],"%")) > 50 && as.numeric(strsplit(data2[i, length(data2)],"%")) > 90 && (lastBuyDate + 10) < i)
        {
		trades <- rbind(trades, c(data1[i + 2, 1], data1[i + 32, 1], data2[i, length(data2)], data1[i+1, length(data1)], data2[i+1, 5], data2[i+31, 5],(data2[i+31, 5] - data2[i+1, 5]) / data2[i + 31, 5]))
                lastBuyDate = i + 1
        }
}
write.table(trades, file = "data2.csv", sep = ",", row.names=FALSE)
}

runTradingStrategy3 <- function()
{
trades <- c()
for(i in startDate:(length(data1[,1])-32))
{
	if(as.numeric(strsplit(data1[i+1, length(data1)],"%")) > 75 && as.numeric(strsplit(data2[i, length(data2)],"%")) > 90 && (lastBuyDate + 10) < i)
        {
		trades <- rbind(trades, c(data1[i + 2, 1], data1[i + 32, 1], data2[i, length(data2)], data1[i+1, length(data1)], data2[i+1, 5], data2[i+31, 5],(data2[i+31, 5] - data2[i+1, 5]) / data2[i + 31, 5]))
                lastBuyDate = i + 1
        }
}
write.table(trades, file = "data3.csv", sep = ",", row.names=FALSE)
}

runTradingStrategy4 <- function()
{
trades <- c()
for(i in startDate:(length(data1[,1])-32))
{
	if(as.numeric(strsplit(data1[i+1, length(data1)],"%")) < 25 && as.numeric(strsplit(data2[i, length(data2)],"%")) > 90 && (lastBuyDate + 10) < i)
        {
		trades <- rbind(trades, c(data1[i + 2, 1], data1[i + 32, 1], data2[i, length(data2)], data1[i+1, length(data1)], data2[i+1, 5], data2[i+31, 5],(data2[i+31, 5] - data2[i+1, 5]) / data2[i + 31, 5]))
                lastBuyDate = i + 1
        }
}
write.table(trades, file = "data4.csv", sep = ",", row.names=FALSE)
}

runTradingStrategy1()
runTradingStrategy2()
runTradingStrategy3()
runTradingStrategy4()


test <- function(step)
{
	return(as.numeric(strsplit(data1[step + 1, length(data1)],"%")))
}
