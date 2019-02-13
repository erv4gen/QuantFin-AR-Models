library(Quandl)
library(dplyr)
setwd("C:/pjt/QuantFin-Volatility")
quandlapi <- readr::read_file('nogit/quandlapi.txt')
Quandl.api_key(quandlapi)
data <- Quandl("XNAS/AAPL")
path_to_save <- 'c:/data/Datasets/Quandl/AAPL.csv'
write.csv(data,path_to_save)

old_dat_file <- "c:/data/Datasets/Finance/Stocks_Prices/AAPL.csv"

exit_file <- "c:/data/Datasets/stockprices/AAPL.csv"

old_df <- read.csv(old_dat_file,header=TRUE,stringsAsFactors = FALSE)

old_df <- old_df[,c('timestamp','open','close','high','low')]
data <- data[,c('Date','Open','Close','High','Low')]
names(data) <- c('timestamp','open','close','high','low')


dat_to_conc <- filter(data,timestamp>max(old_df$timestamp))
full_df <-rbind(old_df,dat_to_conc)
write.csv(full_df,exit_file)
