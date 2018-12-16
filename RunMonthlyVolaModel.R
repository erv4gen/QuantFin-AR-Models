setwd('C:/pjt/QuantFin-Volatility')
library(dplyr)
source('DistributionFitting.R')
source('Model_Vola_Monthly.R')

dat = read.csv("c:/data/Datasets/stockprices/AAPL.csv",
               header=TRUE)

fname= "png/EmpiricalDataPlot%03d.png"

png(filename = fname)

head(dat)
str(dat)

dat$day <-format(as.Date(dat$timestamp),'%d')
dat$month <- as.numeric(format(as.Date(dat$timestamp),'%m'))
dat$year <-format(as.Date(dat$timestamp),'%y')
dat$year <- as.numeric(dat$year)*100
dat$monthyear <-dat$month + dat$year
dat$vola <- (dat$close - dat$open) /dat$open



summa <-  dat %>% 
  group_by(monthyear) %>%
  summarise(mean=mean(vola),sd=sd(vola))



#Plot a histogram of volatility distribution
x <- summa$sd * 100
h<-hist(x, breaks=80, col="red", xlab="Volatility, %", 
        main="Apple Monthy Volatility"
        ,xlim = c(0.0,9.5)
        #        ,ylim = c(0.0,200.0)
        ,freq = FALSE) 
xfit<-seq(min(x),max(x),length=100) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 

#yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


d <- density(summa$sd)
plot(d)
empirical_distribution <- summa$sd
mean(empirical_distribution)

dev.off()

fit_statistics <-FitDistribution(x=empirical_distribution)


y_hat <-MVolaModel(sig_sq = 1.0
                  ,n_samples=1e3
                  ,alpha_prior = as.numeric(fit_statistics$fit$gamma$estimate[c("shape")])
                  ,beta_prior = as.numeric(fit_statistics$fit$gamma$estimate[c("rate")])
                  )


fname= "png/Posterior%03d.png"
png(filename = fname)                  
mean(y_hat)
#mean(y_hat_dummy)

boxplot(y_hat*100)
#boxplot(y_hat_dummy)
max(y_hat)

plot(density(y_hat))
mean(y_hat>0.10)

dev.off()

