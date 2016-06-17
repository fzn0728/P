# Return Calculation
# Zhongnan Fang
# June 14, 2016
library(zoo)
library(tidyr)
setwd("C:/Users/Chandler/Desktop/PPC/Data")

# stock.df <- read.csv("RawData.csv", header = TRUE, skip = 1)
# stock.df <-subset(stock.df, select = c("date","SICCD","TICKER","PRC","RET","SHROUT"))
# stock.list <- sort(unique(stock.df$TICKER))
# stock.df$date_Q <- as.yearqtr(with(stock.df,date),format = "%m/%d/%Y")
# stock.df$date_M <- as.yearmon(with(stock.df,date),format = "%m/%d/%Y")
# stock.df$MKT <- stock.df$PRC*stock.df$SHROUT
# stock.df$R <- 1 + as.numeric(as.character(stock.df$RET))
# save(stock.df,file="./stock.Rdata")
load("stock.Rdata")

# Filter all stock data from the watchlist
stock_list <- list(read.csv("stock_list.csv",header=TRUE))[[1]]

stock.df <- stock.df[(stock.df$TICKER %in% stock_list[,]),]

# drop all space and na data
stock.df <- stock.df[!(is.na(stock.df$R)|is.na(stock.df$TICKER)),]

# Generate quarterly calculation dataframe
stock_Q <- aggregate(stock.df$R,by=list(TICKER = stock.df$TICKER,date_Q = stock.df$date_Q),FUN = prod)
stock_M <- aggregate(stock.df$R,by=list(TICKER = stock.df$TICKER,date_M = stock.df$date_M),FUN = prod)

# write.csv(stock_Q,file='stock_Q.csv')
# write.csv(stock_M,file='stock_M.csv')
# write.csv(stock_list,file='stock_list.csv')

### Generate return dataframe 
return_Q <- spread(stock_Q,date_Q,x)
write.csv()

### get wired data
wired.stock.df <- stock.df[stock.df$R>1.1 | stock.df$R<0.9,]
write.csv(wired.stock.df,file='wired_stock_df.csv')



