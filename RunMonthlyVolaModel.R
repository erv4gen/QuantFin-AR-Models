setwd('C:/pjt/QuantFin-Volatility')
library(dplyr)
source('DistributionFitting.R')
source('Model_Vola_Monthly.R')


dat_file <- "c:/data/Datasets/stockprices/AAPL.csv"

#Reading initial Data
dat = read.csv(dat_file,
               header=TRUE,stringsAsFactors = FALSE)

fname= "png/EmpiricalDataPlot%03d.png"

png(filename = fname)

head(dat)
str(dat)


#Transform data. Grouping by month, calculating sd for monthly volatility
dat$day <-format(as.Date(dat$timestamp),'%d')
dat$month <- as.numeric(format(as.Date(dat$timestamp),'%m'))
dat$year <- as.numeric(format(as.Date(dat$timestamp),'%y')) *100
dat$monthyear <-dat$month + dat$year
dat$vola <- abs((dat$close - dat$open) /dat$open)

mean_dist <- density(dat[which(dat$monthyear==1),c('vola')])
plot(mean_dist)

summa <-  dat %>% 
  group_by(monthyear) %>%
  summarise(mean=mean(vola),sd=sd(vola))



#Plot a histogram of empirical volatility distribution
x <- summa$sd * 100
h<-hist(x, breaks=80, col="red", xlab="Volatility, %", 
        main="Apple Monthy Volatility"
        ,xlim = c(0.0,6.5)
        #        ,ylim = c(0.0,200.0)
        ,freq = FALSE) 
xfit<-seq(min(x),max(x),length=100) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 

#yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

md <- density(summa$md)
d <- density(summa$sd)
plot(d)
empirical_distribution <- summa$sd
cat('Mean Empirical probability: ',mean(empirical_distribution))
cat('Probability of high volatility on empirical data: ',mean(empirical_distribution>0.10))

dev.off()



#Fit diferent probability functions and find the best aproximations
fit_stat <-FitDistribution(X=empirical_distribution)
(aics<- data.frame(list('weibull' =fit_stat[[1]]$weibull$aic,
                       'gamma' =fit_stat[[1]]$gamma$aic,
                       'norm' =fit_stat[[1]]$norm$aic,
                       'lnorm' =fit_stat[[1]]$lnorm$aic)))

cat("KS-test Confidence - ",fit_stat[[3]])
Sys.sleep(3)

#Creating the model

SearchSigma <- function(frm,steps) {
  sim_to_test <- rep(frm,steps) / 2^(0:steps)

  res <- data.frame('Sigma'=0
                  ,'Mean Volatility'=0
                  ,'Max Volatility'=0
                  ,'High Volatility Probability'=0)


  for(i in 1:length(sim_to_test)) {
  
  #Sample MCMC model with defined paramethers
  y_hat <-MVolaModel(X=summa$sd
                     ,sig_sq = sim_to_test[i]
                     ,n_samples=1e3
                     ,alpha_prior = as.numeric(fit_stat[[1]]$gamma$estimate[c("shape")])
                     ,beta_prior = as.numeric(fit_stat[[1]]$gamma$estimate[c("rate")])
                    )
  
  #Collect statistics
  res[i,] <-list(sim_to_test[i]
                 ,mean(y_hat)
                 ,max(y_hat)
                 ,mean(y_hat>mean(empirical_distribution*4)))
  

                                }
    return(res)
  }


frm <- 160
steps <- 15
res <- SearchSigma(frm,steps)

summary(empirical_distribution)
mean(empirical_distribution> 2*mean(empirical_distribution))
