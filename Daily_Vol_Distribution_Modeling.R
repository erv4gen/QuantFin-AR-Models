
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

#distribution fitting
library(fitdistrplus)
library(logspline)
descdist(x, discrete = FALSE)

fit.weibull <- fitdist(x, "weibull")
fit.norm <- fitdist(x, "norm")
fit.gamma <- fitdist(x, "gamma")
fit.lnorm <- fitdist(x, "lnorm")
plot(fit.norm)
plot(fit.weibull)
plot(fit.gamma)
plot(fit.lnorm)

aic <-c(fit.weibull$aic,
  fit.norm$aic,
  fit.gamma$aic,
  fit.lnorm$aic)
names(aic) <- c('weibull','norm','gamma','lnorm')
#str(fit.gamma)

fit.gamma$estimate


#Gamma Model testing
n_sim <-2000
sims <- rgamma(n = n_sim,
               shape = as.numeric(fit.gamma$estimate[c("shape")]),
               rate= as.numeric(fit.gamma$estimate["rate"]) )
hist(sims)
#Kolmogorov-Smirnov test simulation

n.sims <- 5e3

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

ks_stata <- ks.test(unique(x)
                   , "pgamma"
                   , shape = as.numeric(fit.gamma$estimate[c("shape")])
                   , rate= as.numeric(fit.gamma$estimate["rate"])
                   )$statistic

ks_results <-  1.0 - plogspline(ks_stata
                                ,fit
                                )

n_samples_level_05 <- 1.36/ (sqrt(length(x)))
n_samples_level_001 <- 1.22/ (sqrt(length(x)))


library("rjags") 
library("coda")

VolaModel <- function(sig_sq=1.0) {
  

  mod_string <- "model {
    #Likelihood
    for(i in 1:n) {
      y[i] ~ dgamma(alpha,beta)
    }
    #Prior
    alpha ~ dnorm(a_mu,1.0/sig_sq)
    beta ~ dnorm(b_mu, 1.0/sig_sq)
    
                      }"
  #Set up the model
  set.seed(50)
  
  
  n = nrow(summa)
  data_jags <- list(y=summa$sd,
                    n=n,
                    a_mu=as.numeric(fit.gamma$estimate[c("shape")]),
                    b_mu = as.numeric(fit.gamma$estimate[c("rate")]),
                    sig_sq = 0.5
                    )
  
  params <- c("alpha","beta")
  
  inits <- function() {
    alpha_init = as.numeric(fit.gamma$estimate[c("shape")])
    beta_inits = as.numeric(fit.gamma$estimate[c("rate")])
    inits <- list("alpha" = alpha_init,
                  "beta" =beta_inits )
  }
  
  mod = jags.model(textConnection(mod_string),
                   data = data_jags,
                   inits = inits,
                   n.adapt = 1e2,
                   n.chains = 3)
  
  
  
  mod_sim <-coda.samples(model = mod,
                         variable.names = params,
                         n.iter =1e4)
  
  #Post processing 
  
  #plot(mod_sim)
  print(summary(mod_sim))
  
  print(gelman.diag(mod_sim))
  #autocorr.plot(mod_sim)
  print(autocorr.diag(mod_sim))
  effectiveSize(mod_sim)
  mod_csim  <- as.mcmc(do.call(rbind,mod_sim))
  params_ <- do.call(rbind,mod_sim)
  
  y_hat <- rgamma(length(params_),
                  shape = params_[,1],
                  rate = params_[,2])
  y_hat <- na.omit(y_hat)
  
  y_hat_dummy <-rgamma(length(params_),
                       shape = as.numeric(fit.gamma$estimate["shape"])
                       , rate= as.numeric(fit.gamma$estimate["rate"]))
 
  return(y_hat) 
}

y_hat <-VolaModel(sig_sq = 1.0)
mean(y_hat)
mean(y_hat_dummy)

boxplot(y_hat*100)
boxplot(y_hat_dummy)
max(y_hat)

plot(density(y_hat))
mean(y_hat>0.10)

