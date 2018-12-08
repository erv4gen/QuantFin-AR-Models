
dat = read.csv("c:/data/Datasets/stockprices/AAPL.csv",
               header=TRUE)
head(dat)
str(dat)


dat$day <-format(as.Date(dat$timestamp),'%d')??
dat$month <- as.numeric(format(as.Date(dat$timestamp),'%m'))
dat$year <-format(as.Date(dat$timestamp),'%y')
dat$year <- as.numeric(dat$year)*100
dat$stp <-dat$month + dat$year


vola <-aggregate(close ~ stp,
                 dat,
                 FUN = function(i) 
                   (max(i) - min(i)) / min(i))
vola['lvol'] <-NA
vola['lvol'] <- log(vola['close'])
head(vola)


#Plot a histogram of volatility distribution
x <- vola$close * 100
h<-hist(x, breaks=500, col="red", xlab="Volatility, %", 
        main="Apple Monthy Volatility"
        ,xlim = c(-50.50,150.5)
#        ,ylim = c(0.0,200.0)
       ,freq = FALSE) 
xfit<-seq(min(x),max(x),length=100) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

d <- density(vola$close)
plot(d)

#distribution fitting
library(fitdistrplus)
library(logspline)
descdist(x, discrete = FALSE)

fit.weibull <- fitdist(x, "weibull")
fit.norm <- fitdist(x, "norm")
fit.gamma <- fitdist(x, "gamma")
plot(fit.norm)
plot(fit.weibull)
plot(fit.gamma)

fit.weibull$aic
fit.norm$aic
fit.gamma$aic

str(fit.gamma)

fit.gamma$estimate

library("rjags")
library("coda")


mod_string <- "model {
  #Likelihood
  for(i in 1:n) {
    y[i] ~ dnorm(mu,1.0/sig2)
  }
  #Prior
  mu ~ dt(0.0,1.0/1.0,1)
  sig2 = 1.0
}"
#Set up the model
set.seed(50)


n = nrow(vola)
data_jags <- list(y=vola$lvol,n=n)
params <- c("mu")

inits <- function() {
  mu_init = 1.0
  inits <- list("mu" = mu_init)
}

mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 inits = inits,
                 n.chains = 3)

#Run the MCMC sample 
update(mod,500)

mod_sim <-coda.samples(model = mod,
                       variable.names = params,
                       n.iter =5e3)

#Post processing 

plot(mod_sim)
summary(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
mod_csim  <- as.mcmc(do.call(rbind,mod_sim))
100*mean(exp(mod_csim)) / mean(dat$close)
