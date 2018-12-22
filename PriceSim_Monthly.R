library(ggplot2)
setwd('C:/pjt/QuantFin-Volatility')
vola <- read.csv("c:/data/Datasets/Options/aapl_vola_sims.csv",header = TRUE,stringsAsFactors = FALSE)
fact_prices  <- "c:/data/Datasets/stockprices/AAPL.csv"

#Reading initial Data
dat_file_path <- "c:/data/Datasets/stockprices/AAPL.csv"
dat <- read.csv(dat_file_path, header=TRUE,stringsAsFactors = FALSE)
last_price <- dat[1:1,c('close')]






# parameters
simulations <- 200 # number of MC simulations
n <- 12*30 # trading days

stock_mu <- .1 # drift 10%
stock_sigma <- .2 # volatility 20%

# Monte Carlo simulations
set.seed(42) # for reproducibility
mod_sims <- list()

f_stock_return <- function(stock_price, n, vola=vola){
  delta_t <- 1/n # one period
  stock_mu <- mean(vola$x)
  stock_prices <-rep(1:n+1)
  stock_prices[1] <- stock_price
  for (i in seq(n)){
    epsilon <- runif(n=1, min=0, max=1) # random generated number
    stock_sigma <-sample(vola$x,1)
    # calculate stock price (using quantile function of normal distribution)
    stock_prices[i+1] <- stock_prices[i] * (1 + qnorm(epsilon, 
                                                        stock_mu * delta_t, 
                                                        stock_sigma* sqrt(delta_t)))
  }
  return(stock_prices)
}



for (i in seq(simulations)){
  mod_sims[[i]] <- f_stock_return(stock_price=last_price, 
                                   n=n, 
                                   vola=vola)
}



sim_df <- lapply(mod_sims, function(x) cbind(x = seq_along(x), y = x))
names(sim_df) <- 1:simulations
list.names <- names(sim_df)
lns <- sapply(sim_df, nrow)
sim_df <- as.data.frame(do.call("rbind", sim_df))
sim_df$group <- rep(list.names, lns)




fname= "png/SimPlot%03d.png"

png(filename = fname)

ggplot(sim_df, aes(x = x, y = y, colour = group)) +
  theme_bw() +
  geom_line(show.legend = FALSE)

ggsave(fname)

dev.off()


mean(sim_df$y<200)



















