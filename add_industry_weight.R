# Return Calculation
# Zhongnan Fang
# June 30, 2016
### I may forgot one thing like rebalancing?? using last month average

#################################### Data cleaning ####################################
library(zoo)
library(tidyr)
library(dplyr)
options(scipen=50)
setwd("C:/Users/Chandler/Desktop/PPC/Data")

### Generate the dataframe of rawdata and clean the format
stock.df <- read.csv("RawData_new.csv", header = TRUE)
stock.df <-subset(stock.df, select = c("Date","SICCD","TICKER","PRC","RET","SHROUT"))
stock.df$date_Q <- as.yearqtr(as.character(stock.df$Date),format = "%m/%d/%Y")
stock.df$date_M <- as.yearmon(as.character(stock.df$Date),format = "%m/%d/%Y")
stock.df$MKT <- stock.df$PRC*stock.df$SHROUT
stock.df$R <- 1 + as.numeric(as.character(stock.df$RET))

# Import the stock industry data
industry.df <- read.csv("industry.csv",header = TRUE)
stock.df <- merge(stock.df, industry.df, by = "TICKER")

# Filter all stock data from the watchlist
stock_list <- list(read.csv("stock_list.csv",header=TRUE))[[1]]
stock.df <- stock.df[(stock.df$TICKER %in% stock_list[,]),]

# drop all space and na data
stock.df <- stock.df[!(is.na(stock.df$R)|is.na(stock.df$TICKER)),]

# Generate quarterly calculation dataframe
stock_Q <- aggregate(stock.df$R,by=list(TICKER = stock.df$TICKER,date_Q = stock.df$date_Q, Industry = stock.df$Industry.Sector),FUN = prod)
MKT_Q <- aggregate(stock.df$MKT,by=list(TICKER = stock.df$TICKER,date_Q = stock.df$date_Q, Industry = stock.df$Industry.Sector),FUN = mean)

# Save the clean data into Rdata file
save(stock.df,file="./stock_new.Rdata")
load("stock_new.Rdata")

#################################### Return Calculation ####################################
### Generate return dataframe 
return_Q <- spread(stock_Q,date_Q,x)
colnames(return_Q) <- make.names(colnames(return_Q), unique=TRUE)
Weight_Q <- spread(MKT_Q,date_Q,x)
colnames(Weight_Q) <- make.names(colnames(Weight_Q), unique=TRUE)

### get wired data
# wired.stock.df <- stock.df[stock.df$R>1.1 | stock.df$R<0.9,]
# write.csv(wired.stock.df,file='wired_stock_df.csv')

### Consider rebalance
# Get the rebalance table
rebalance_Q <- read.csv("rebalance_Q.csv",header = TRUE,check.names=FALSE)
rebalance_Q <- rebalance_Q[(rebalance_Q$Ticker %in% return_Q$TICKER),] # get the common of rebalance_Q and return_Q
return_Q <- return_Q[(return_Q$TICKER %in% rebalance_Q$Ticker),] # get the common of rebalance_Q and return_Q
rebalance_Q <- rebalance_Q[order(rebalance_Q[,1]),] # sort the ticker and make sure it is the same as return_Q

# Filter out all stocks that are in the watchlist
watchList_return_Q <- return_Q[0:nrow(return_Q),]
for (i in (0:(length(return_Q)-3))){
  watchList_return_Q[,i+3] <- rebalance_Q[,i+2]*return_Q[,i+3]
}
watchList_return_Q[is.na(watchList_return_Q)] <- 0

### Calcualate equal weight return
# Borrow the table structure from the return_Q
equal_weight_return_Q <- return_Q[0:2,-1:-2]
for (j in (1:(length(equal_weight_return_Q)-1))){
  equal_weight_return_Q[2,j+1] <- sum(watchList_return_Q[,j+2]>0)
  equal_weight_return_Q[1,j+1] <- sum(watchList_return_Q[,j+2])/equal_weight_return_Q[2,j+1]
}
equal_weight_return_Q <- data.frame(t(equal_weight_return_Q))
equal_weight_return_Q$cumReturn <- cumprod(equal_weight_return_Q[,1])

### Calculate the market weight return
# Format the weight Q
Weight_Q <- Weight_Q[(Weight_Q$TICKER %in% rebalance_Q$Ticker),] # get the common of rebalance_Q and Weight_Q
Weight_Q[is.na(Weight_Q)] <- 0 # replace NA with 0

# Get the market cap weighting dataframe
watchList_market_weight_Q <- return_Q[0:nrow(Weight_Q),]
for (i in (0:(length(Weight_Q)-2))){
  watchList_market_weight_Q[,i+3] <- rebalance_Q[,i+2]*Weight_Q[,i+2]
}
watchList_market_weight_Q[is.na(watchList_market_weight_Q)] <- 0

# Calculate the market weight return considering the watchlist market weight 
market_weight_return_Q <- return_Q[0:2,-1:-2]
for (j in (1:length(market_weight_return_Q))){
  market_weight_return_Q[2,j] <- sum(watchList_market_weight_Q[,j+2])
  market_weight_return_Q[1,j] <- sum(watchList_return_Q[,j+2]*(watchList_market_weight_Q[,j+2]/market_weight_return_Q[2,j]))
}
market_weight_return_Q <- data.frame(t(market_weight_return_Q))
market_weight_return_Q$cumReturn <- cumprod(market_weight_return_Q[,1])

#################################### Table Output ####################################
# write.csv(stock_Q,file='stock_Q.csv')
# write.csv(stock_M,file='stock_M.csv')
# write.csv(stock_list,file='stock_list.csv')
# write.csv(return_Q$TICKER,file="TICKER.csv")
# write.csv(rebalance_Q,file='rebalance.csv')
# write.csv(return_Q,file='return_Q.csv')
# write.csv(equal_weight_return_Q,file='equal_weight_return_Q.csv')
# write.csv(market_weight_return_Q,file='market_weight_return_Q.csv')
# write.csv(watchList_return_Q,file='watchList_return_Q.csv')

#################################### Russell Index ####################################
### Read Russell 2000 and clean the format
russell.df <- read.csv("Russell_index.csv", header = TRUE)
russell.df$date_Q <- as.yearqtr(as.character(russell.df$Date),format = "%m/%d/%Y")
russell.df[is.na(russell.df)] <- 0 # Set first return to be zero
russell.df$R <- russell.df$Return + 1

### Read Russell 2000 by industry and clean the format
russell_industry.df <- read.csv("Russell_industry.csv", header = TRUE)
russell_industry.df$date_Q <- as.yearqtr(as.character(russell_industry.df$Date),format = "%m/%d/%Y")

### Get Russell 2000 return_Q
russell_return_Q <- aggregate(russell.df$R,by=list(date_Q = russell.df$date_Q),FUN = prod)
russell_return_Q$cumReturn <- cumprod(russell_return_Q$x)
russell_return_Q <- russell_return_Q[1:44,]
write.csv(russell_return_Q, file = "russell_return_Q.csv")

#################################### Industry Return Calculation ####################################
### Calculate return by industry
industry_return <- stock_Q %>%
  group_by(date_Q,Industry) %>%
  summarise(Return = mean(x, na.rm=TRUE)) %>%
  spread(date_Q, Return)
industry_return[is.na(industry_return)] <- 0

### Calculate the weight of each industry in the watchlist
# Calculate the market value table
industry_mkt <- MKT_Q %>%
  group_by(date_Q,Industry) %>%
  summarise(MKT = sum(x, na.rm=TRUE)) %>%
  spread(date_Q, MKT)
industry_mkt[is.na(industry_mkt)] <- 0

# Add Sum column
for (i in (1:(length(industry_mkt)-1))){
  industry_mkt[10,i+1] <- sum(industry_mkt[1:9,i+1])
}

# Change the type of Industry Column
industry_mkt$Industry <- as.character(industry_mkt$Industry)

# Replace the NA value
industry_mkt$Industry[is.na(industry_mkt$Industry)] = 'Sum'

# Get the market value weight
industry_mkt_weight <- industry_mkt
for (i in (1:(length(industry_mkt)-1))){
  for (j in (1:10)){
    industry_mkt_weight[j,i+1] <- industry_mkt[j,i+1]/industry_mkt[10,i+1]
  }
}

