# Monthly Return Calculation
# Zhongnan Fang
# July 11, 2016

#################################### Data cleaning ####################################
library(zoo)
library(tidyr)
library(dplyr)
options(scipen=50)
setwd("C:/Users/Chandler/Desktop/PPC/Data")

### Generate the dataframe of rawdata and clean the format
stock.df <- read.csv("RawData_all.csv", header = TRUE)
stock.df <- subset(stock.df, select = c("Date","SICCD","TICKER","PRC","RET","SHROUT"))
stock.df$date_M <- as.yearmon(as.character(stock.df$Date),format = "%m/%d/%Y")
stock.df$MKT <- stock.df$PRC*stock.df$SHROUT
stock.df$R <- 1 + as.numeric(as.character(stock.df$RET))

# Drop duplicate data
permno <- c(90842,89917,92468,90217,83559,81651,46068)
stock.df <- stock.df[!(stock.df$PERMNO %in% permno),]

# Import the stock industry data
industry.df <- read.csv("industry.csv",header = TRUE)
stock.df <- merge(stock.df, industry.df, by = "TICKER")

# Filter all stock data from the watchlist
stock_list <- list(read.csv("stock_list.csv",header=TRUE))[[1]]
stock.df <- stock.df[(stock.df$TICKER %in% stock_list[,]),]

# drop all space and na data
stock.df <- stock.df[!(is.na(stock.df$R)|is.na(stock.df$TICKER)),]

# Generate quarterly calculation dataframe
stock_M <- aggregate(stock.df$R,by=list(TICKER = stock.df$TICKER,date_M = stock.df$date_M, Industry = stock.df$Industry.Sector),FUN = prod)
MKT_M <- aggregate(stock.df$MKT,by=list(TICKER = stock.df$TICKER,date_M = stock.df$date_M, Industry = stock.df$Industry.Sector),FUN = mean)

# Save the clean data into Rdata file
save(stock.df,industry.df,stock_M, MKT_M, stock_list, file="./stock_new_M.Rdata")
load("stock_new_M.Rdata")

#################################### Return Calculation ####################################
### Generate return dataframe 
return_M <- spread(stock_M,date_M,x)
n <- names(return_M)[-c(3:26)]
n_for_weight <- names(return_M)[-c(3:25,158)]
return_M <- return_M[,n]
colnames(return_M) <- make.names(colnames(return_M), unique=TRUE)

### Generate weighting matrix -> from 2004-12
Weight_M <- spread(MKT_M,date_M,x)
Weight_M <- Weight_M[,n_for_weight]
colnames(Weight_M) <- make.names(colnames(Weight_M), unique=TRUE)

### Consider rebalance
# Get the rebalance table
rebalance_M <- read.csv("rebalance_M.csv",header = TRUE,check.names=FALSE)
rebalance_M <- rebalance_M[(rebalance_M$Ticker %in% return_M$TICKER),] # get the common of rebalance_M and return_M
return_M <- return_M[(return_M$TICKER %in% rebalance_M$Ticker),] # get the common of rebalance_M and return_M
rebalance_M <- rebalance_M[order(rebalance_M[,1]),] # sort the ticker and make sure it is the same as return_M
rebalance_M <- rebalance_M[,c(1:133)] # Delete columns after 2015 since we don't have data temporately

# Filter out all stocks that are in the watchlist
watchList_return_M <- return_M[0:nrow(return_M),]
for (i in (0:(length(return_M)-3))){
  watchList_return_M[,i+3] <- rebalance_M[,i+2]*return_M[,i+3]
}
watchList_return_M[is.na(watchList_return_M)] <- 0

### Calcualate equal weight return
# Borrow the table structure from the return_Q
equal_weight_return_M <- return_M[0:2,-1:-2]
for (j in (0:(length(equal_weight_return_M)-1))){
  equal_weight_return_M[2,j+1] <- sum(watchList_return_M[,j+3]>0)
  equal_weight_return_M[1,j+1] <- sum(watchList_return_M[,j+3])/equal_weight_return_M[2,j+1]
}
equal_weight_return_M <- data.frame(t(equal_weight_return_M))
equal_weight_return_M$cumReturn <- cumprod(equal_weight_return_M[,1])

### Calculate the market weight return
# Format the weight Q
Weight_M <- Weight_M[(Weight_M$TICKER %in% rebalance_M$Ticker),] # get the common of rebalance_Q and Weight_Q
Weight_M[is.na(Weight_M)] <- 0 # replace NA with 0

# Get the market cap weighting dataframe : This is the weight which is used to calculate the next perioed market weighted return
# I adjust the prior bias since we use the lag rebalance table * weight_Q
watchList_market_weight_M <- Weight_M[0:nrow(Weight_M),]
for (i in (0:(length(Weight_M)-3))){
  watchList_market_weight_M[,i+3] <- rebalance_M[,i+2]*Weight_M[,i+3]
}
watchList_market_weight_M[is.na(watchList_market_weight_M)] <- 0

# Calculate the market weight return considering the watchlist market weight 
market_weight_return_M <- return_M[0:1,-1:-2]
for (j in (0:(length(market_weight_return_M)-1))){
  market_weight_return_M[1,j+1] <- sum(watchList_return_M[,j+3]*(watchList_market_weight_M[,j+3]/sum(watchList_market_weight_M[,j+3])))
}
market_weight_return_M <- data.frame(t(market_weight_return_M))
#market_weight_return_Q[1,1] <- 1 # Let 2005Q1 return equal to one since we skip the Q1 calculation, since we don't know the rebalance table of 2004 Q4
market_weight_return_M$cumReturn <- cumprod(market_weight_return_M[,1])

#################################### Russell Index ####################################
### Read Russell 2000 and clean the format
russell.df <- read.csv("Russell_index.csv", header = TRUE)
russell.df$date_M <- as.yearqtr(as.character(russell.df$Date),format = "%m/%d/%Y")
russell.df[is.na(russell.df)] <- 0 # Set first return to be zero
russell.df$R <- russell.df$Return + 1

### Get Russell 2000 return_Q
russell_return_M <- aggregate(russell.df$R,by=list(date_M = russell.df$date_M),FUN = prod)
russell_return_M$cumReturn <- cumprod(russell_return_M$x)
russell_return_M <- russell_return_M[1:44,]

### Read Russell 2000 by industry and clean the format
russell_industry.df <- read.csv("Russell_industry.csv", header = TRUE)
russell_industry.df$date_M <- as.yearqtr(as.character(russell_industry.df$Date),format = "%m/%d/%Y")

# russell_industry_return <- russell_industry.df %>%
#   group_by(date_Q, Sector) %>%
#   sum





#################################### Industry Return Calculation ####################################
### Calculate return by industry
industry_return_M <- watchList_return_M %>%
  gather(date_M,Return,Jun.2005:Dec.2015) %>%
  group_by(date_M,Industry) %>%
  filter(Return>0) %>%
  summarise(Return = mean(Return, na.rm=TRUE)) %>%
  spread(date_M, Return)
industry_return_M[is.na(industry_return_M)] <- 0

### Calculate the weight of each industry in the watchlist
# Calculate the market value table
industry_mkt_weight_M <- watchList_market_weight_M %>%
  gather(date_M,MKT,May.2005:Nov.2015) %>% # 2015 Dec is not available, since we don't know the next period rebalance table
  group_by(date_M,Industry) %>%
  filter(MKT>0) %>%
  summarise(MKT = sum(MKT, na.rm=TRUE)) %>%
  spread(date_M, MKT)
industry_mkt_weight_M[is.na(industry_mkt_weight_M)] <- 0

# Add Sum column
n <- nrow(industry_mkt_weight_M)
for (i in (1:(length(industry_mkt_weight_M)-1))){
  industry_mkt_weight_M[n+1,i+1] <- sum(industry_mkt_weight_M[1:n,i+1])
}

# Change the type of Industry Column
industry_mkt_weight_M$Industry <- as.character(industry_mkt_weight_M$Industry)

# Replace the NA value
industry_mkt_weight_M$Industry[is.na(industry_mkt_weight_M$Industry)] = 'Sum'

# Get the market value weight
industry_mkt_weight_percent_M <- industry_mkt_weight_M
for (i in (1:(length(industry_mkt_weight_M)-1))){
  for (j in (1:(n+1))){
    industry_mkt_weight_percent_M[j,i+1] <- industry_mkt_weight_M[j,i+1]/industry_mkt_weight_M[n+1,i+1]
  }
}

#################################### Table Output ####################################
# write.csv(stock.df,file='stockdf.csv')
# write.csv(stock_Q,file='stock_Q.csv')
# write.csv(stock_M,file='stock_M.csv')
# write.csv(stock_list,file='stock_list.csv')
# write.csv(return_Q$TICKER,file="TICKER.csv")
# write.csv(rebalance_Q,file='rebalance.csv')
write.csv(return_M,file='return_M.csv')
write.csv(watchList_return_M,file='watchList_return_M.csv')
write.csv(equal_weight_return_M,file='equal_weight_return_M.csv')
write.csv(market_weight_return_M,file='market_weight_return_M.csv')


### Russell
write.csv(russell_return_M, file = "russell_return_M.csv")

### Industry of Watchlist
write.csv(industry_return_M,file='industry_return_M.csv')
# write.csv(industry_mkt_weight,file='industry_mkt_weight.csv')
write.csv(industry_mkt_weight_percent_M,file='industry_mkt_weight_percent_M.csv')
