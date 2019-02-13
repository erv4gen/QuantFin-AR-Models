library("rjags") 
library("coda")


MVolaModel <- function(X=c(1,1,1)
                       ,sig_sq=1.0
                       ,n_samples=1e5
                       ,alpha_prior = 1.0
                       ,beta_prior = 1.0) {
  
  fname= paste("png/MonthHierarchicalModel",sig_sq,"%03d.png")
  cat('Creating a hierarchical model.\nPrints will be saved to:' , fname)
  png(filename = fname)
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
  data_jags <- list(y=X,
                    n=n,
                    a_mu= alpha_prior,
                    b_mu = beta_prior,
                    sig_sq = sig_sq
  )
  
  params <- c("alpha","beta")
  
  inits <- function() {
    alpha_init =alpha_prior
    beta_inits = beta_prior
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
                         n.iter =n_samples)
  
  #Post processing 
  
  plot(mod_sim)
  print(summary(mod_sim))
  
  print(gelman.diag(mod_sim))
  autocorr.plot(mod_sim)
  
  print(autocorr.diag(mod_sim))
  effectiveSize(mod_sim)
  #mod_csim  <- as.mcmc(do.call(rbind,mod_sim))
  mod_csim <- do.call(rbind,mod_sim)
  
  y_hat <- rgamma(length(mod_csim),
                  shape = mod_csim[,1],
                  rate = mod_csim[,2])
  y_hat <- na.omit(y_hat)
  
  y_hat_dummy <-rgamma(length(mod_csim),
                       shape = alpha_prior
                       ,rate= beta_prior
                       )
  boxplot(y_hat*100)
  plot(density(y_hat))
  dev.off()
  return(y_hat) 
}