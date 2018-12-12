
dat = read.csv("c:/data/Datasets/stockprices/AAPL.csv",
               header=TRUE)
head(dat)
str(dat)


dat$day <-format(as.Date(dat$timestamp),'%d')
dat$month <- as.numeric(format(as.Date(dat$timestamp),'%m'))
dat$year <-format(as.Date(dat$timestamp),'%y')
dat$year <- as.numeric(dat$year)*100
dat$monthyear <-dat$month + dat$year
dat$vola <- (dat$close - dat$open) /dat$open

library(dplyr)

summa <-  dat %>% 
         group_by(monthyear) %>%
         summarise(mean=mean(vola),sd=sd(vola))


summa$vola <- summa$sd
summa$vola[summa$mean<0] <- summa$sd[summa$mean<0] * -1


#Plot a histogram of volatility distribution
x <- summa$vola * 100
h<-hist(x, breaks=80, col="red", xlab="Volatility, %", 
        main="Apple Monthy Volatility"
        ,xlim = c(-9.50,9.5)
        #        ,ylim = c(0.0,200.0)
        ,freq = FALSE) 
xfit<-seq(min(x),max(x),length=100) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

d <- density(summa$vola)
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

#str(fit.gamma)

fit.gamma$estimate


n_sim <-2000
sims <- rgamma(n = n_sim ,shape = as.numeric(fit.gamma$estimate[c("shape")]), rate= as.numeric(fit.gamma$estimate["rate"]) )
hist(sims)
#Kolmogorov-Smirnov test simulation

n.sims <- 5e4

stats <- replicate(n.sims, 
                   {      
                     r <- rgamma(n = length(x)
                                 , shape = as.numeric(fit.gamma$estimate[c("shape")])
                                 , rate= as.numeric(fit.gamma$estimate["rate"])
                     )
                     
                     as.numeric(ks.test(r
                                        , "pgamma"
                                        , shape = as.numeric(fit.gamma$estimate[c("shape")])
                                        , rate= as.numeric(fit.gamma$estimate["rate"])
                     )$statistic
                     )      
                   }
)

plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
grid()

#p-value for KS test
fit <- logspline(stats)

pvalue <- 1.0 - plogspline(ks.test(unique(x)
                                 , "pgamma"
                                 , shape = as.numeric(fit.gamma$estimate[c("shape")])
                                 , rate= as.numeric(fit.gamma$estimate["rate"])
)$statistic
, fit
)

library("rjags")
library("coda")


mod_string <- "model {
  #Likelihood
  for(i in 1:n) {
    y[i] ~ dgamma(alpha,beta)
  }
  #Prior
  alpha ~ dnorm(1.80567063,1.0/1.0*250)
  beta ~ dnorm(0.09418998, 1.0/1.0*1050)
  
                    }"
#Set up the model
set.seed(50)


n = nrow(vola)
data_jags <- list(y=vola$lvol,n=n)
params <- c("alpha","beta")

inits <- function() {
  alpha_init = 1.80567063
  beta_inits = 0.09418998
  inits <- list("alpha" = alpha_init,
                "beta" =beta_inits )
}

mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 inits = inits,n.adapt = 5e2,
                 n.chains = 3)



mod_sim <-coda.samples(model = mod,
                       variable.names = params,
                       n.iter =5e5)

#Post processing 

plot(mod_sim)
summary(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)
mod_csim  <- as.mcmc(do.call(rbind,mod_sim))
params_ <- do.call(rbind,mod_sim)
y_hat <- rgamma(length(params_),
                shape = params_[,1],
                rate = params_[,2])
y_hat <- na.omit(y_hat)
mean(y_hat)
boxplot(y_hat)
max(y_hat)
