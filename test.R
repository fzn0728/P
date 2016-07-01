# Return Calculation
# Zhongnan Fang
# June 14, 2016
library(zoo)
library(tidyr)
setwd("C:/Users/Chandler/Desktop/PPC/Data")

stock.df <- read.csv("RawData.csv", header = TRUE,skip = 1)
stock.df <-subset(stock.df, select = c("date","SICCD","TICKER","PRC","RET","SHROUT"))
stock.list <- sort(unique(stock.df$TICKER))
stock.df$date_Q <- as.yearqtr(with(stock.df,date),format = "%m/%d/%Y")
stock.df$date_M <- as.yearmon(with(stock.df,date),format = "%m/%d/%Y")
stock.df$MKT <- stock.df$PRC*stock.df$SHROUT
stock.df$R <- 1 + as.numeric(as.character(stock.df$RET))
save(stock.df,file="./stock.Rdata")
load("stock.Rdata")

stock.df <- read.csv("RawData_new.csv", header = TRUE)
stock.df <-subset(stock.df, select = c("date","SICCD","TICKER","PRC","RET","SHROUT"))
stock.list <- sort(unique(stock.df$TICKER))
stock.df$date_Q <- as.yearqtr(with(stock.df,date),format = "%m/%d/%y")
stock.df$date_M <- as.yearmon(with(stock.df,date),format = "%m/%d/%y")
stock.df$MKT <- stock.df$PRC*stock.df$SHROUT
stock.df$R <- 1 + as.numeric(as.character(stock.df$RET))
save(stock.df,file="./stock_new.Rdata")
load("stock_new.Rdata")





# Filter all stock data from the watchlist
stock_list <- list(read.csv("stock_list.csv",header=TRUE))[[1]]
stock.df <- stock.df[(stock.df$TICKER %in% stock_list[,]),]

# drop all space and na data
stock.df <- stock.df[!(is.na(stock.df$R)|is.na(stock.df$TICKER)),]

# Generate quarterly calculation dataframe
stock_Q <- aggregate(stock.df$R,by=list(TICKER = stock.df$TICKER,date_Q = stock.df$date_Q),FUN = prod)
MKT_Q <- aggregate(stock.df$MKT,by=list(TICKER = stock.df$TICKER,date_Q = stock.df$date_Q),FUN = mean)


stock_M <- aggregate(stock.df$R,by=list(TICKER = stock.df$TICKER,date_M = stock.df$date_M),FUN = prod)

# write.csv(stock_Q,file='stock_Q.csv')
# write.csv(stock_M,file='stock_M.csv')
# write.csv(stock_list,file='stock_list.csv')

### Generate return dataframe 
return_Q <- spread(stock_Q,date_Q,x)
Weight_Q <- spread(MKT_Q,date_Q,x)
# write.csv(return_Q$TICKER,file="TICKER.csv")

### get wired data
# wired.stock.df <- stock.df[stock.df$R>1.1 | stock.df$R<0.9,]
# write.csv(wired.stock.df,file='wired_stock_df.csv')

### Consider rebalance
# Get the rebalance table
rebalance_Q <- read.csv("rebalance_Q.csv",header = TRUE,check.names=FALSE)
rebalance_Q <- rebalance_Q[(rebalance_Q$Ticker %in% return_Q$TICKER),] # get the common of rebalance_Q and return_Q
return_Q <- return_Q[(return_Q$TICKER %in% rebalance_Q$Ticker),] # get the common of rebalance_Q and return_Q
rebalance_Q <- rebalance_Q[order(rebalance_Q[,1]),] # sort the ticker and make sure it is the same as return_Q



# write.csv(rebalance_Q,file='rebalance.csv')
# write.csv(return_Q,file='return_Q.csv')




# Filter out all stocks that are in the watchlist
watchList_return_Q <- return_Q[0:nrow(return_Q),]
for (i in (0:(length(return_Q)-2))){
  watchList_return_Q[,i+2] <- rebalance_Q[,i+2]*return_Q[,i+2]
}
watchList_return_Q[is.na(watchList_return_Q)] <- 0

### Calcualate equal weight return
# Borrow the table structure from the return_Q
equal_weight_return_Q <- return_Q[0:2,-1]
for (j in (1:length(equal_weight_return_Q))){
  equal_weight_return_Q[2,j] <- sum(watchList_return_Q[,j+1]>0)
  equal_weight_return_Q[1,j] <- sum(watchList_return_Q[,j+1])/equal_weight_return_Q[2,j]
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
  watchList_market_weight_Q[,i+2] <- rebalance_Q[,i+2]*Weight_Q[,i+2]
}
watchList_market_weight_Q[is.na(watchList_return_Q)] <- 0

# Calculate the market weight return considering the watchlist market weight 
market_weight_return_Q <- return_Q[0:2,-1]
for (j in (1:length(market_weight_return_Q))){
  market_weight_return_Q[2,j] <- sum(watchList_market_weight_Q[,j+1])
  market_weight_return_Q[1,j] <- sum(watchList_return_Q[,j+1]*(watchList_market_weight_Q[,j+1]/market_weight_return_Q[2,j]))
}
market_weight_return_Q <- data.frame(t(market_weight_return_Q))
market_weight_return_Q$cumReturn <- cumprod(market_weight_return_Q[,1])

########
write.csv(equal_weight_return_Q,file='equal_weight_return_Q.csv')
write.csv(market_weight_return_Q,file='market_weight_return_Q.csv')
