#distribution fitting

library(fitdistrplus)
library(logspline)




FitDistribution <-function(x
                           ,n.sims = 5e4
                           , compare_to = 'gamma') {
  fname= "png/FitDistributionPlot%03d.png"
  
  cat('Fitting distribution to the data.\nPrints will be saved to:' , fname)
  png(filename = fname)
  descdist(x, discrete = FALSE)
  
  fit <- list()
  
  fit$weibull <- fitdist(x, "weibull")
  fit$norm <- fitdist(x, "norm")
  fit$gamma <- fitdist(x, "gamma")

  
  #par(mar=c(1,1,1,1))
  plot(fit$norm)
  plot(fit$weibull)
  plot(fit$gamma)
  
  dev.off()
  

  
  #Kolmogorov-Smirnov test simulation
  
  stats <- replicate(n.sims, 
                     {      
                       r <- rgamma(n = length(x)
                                   , shape = as.numeric(fit$gamma$estimate[c("shape")])
                                   , rate= as.numeric(fit$gamma$estimate["rate"])
                       )
                       
                       as.numeric(ks.test(r
                                          , "pgamma"
                                          , shape = as.numeric(fit$gamma$estimate[c("shape")])
                                          , rate= as.numeric(fit$gamma$estimate["rate"])
                       )$statistic
                       )      
                     }
  )
  
  p.ks <- plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
  #grid()
  
  #p-value for KS test
  pfit <- logspline(stats)
  ks_test <- ks.test(unique(x)
                     , "pgamma"
                     , shape = as.numeric(fit$gamma$estimate[c("shape")])
                     , rate= as.numeric(fit$gamma$estimate["rate"])
  )$statistic
  pvalue_confidence <- 1 - plogspline(ks_test
  , pfit
  )
  
  return(list(fit_object=fit
              ,ks_test = ks_test
              ,pvalue_confidence = pvalue_confidence)
         )
}