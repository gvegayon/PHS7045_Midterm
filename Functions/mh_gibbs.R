############################################################'
# gibbs_mh_run.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Wrapper for full algorithm
#
# Advanced R Goals: 
#       1) Use data table
#
#
# Notes: - Use set_hyperparameters function to define hypers
#############################################################'    

mh_gibbs<- function(Y = dat,          #Data
                         hypers = hyperparams,  #Hyperparameters
                         sim = 100,
                         burn = 10
){
  
  # Set up  
  #number of studies
  k <- nrow(Y)
  
  
  #Preallocate memory for final result
  mu <- numeric(sim)
  tau2 <- numeric(sim)
  thetas <- matrix(NA_real_, nrow = sim, ncol = k)
  gammas <- matrix(NA_real_, nrow = sim, ncol = k)
  

  
  #initial values based on data and hyperparameters
  #thetas initialized by odds ratio in data
  thetas_init <- Y[,"theta_i"]
  
  #gammas initialized by average logit in data
  gammas_init <- Y[,"gamma_i"]
  
  #mu initialized by average theta
  mu_init <- mean(thetas_init)
  
  #tau initialized by variance of thetas
  tau2_init <- sd(thetas_init)^2
  

  #update these initial values
  mu[1]<- mu_init
  tau2[1]<-tau2_init
  thetas[1,]<- thetas_init
  gammas[1,] <- gammas_init


  
  for(i in 2:sim){
    temp <- gibbs(Y=Y,
                  mu = mu[i-1],
                  tau2 = tau2[i-1],
                  gammas = as.vector(gammas[(i-1),]),
                  thetas = as.vector(thetas[(i-1),]),
                  k = k,
                  hypers = hypers
    )

    mu[i]<- temp$mu
    tau2[i]<- temp$tau2
    gammas[i,]<- temp$gammas
    thetas[i,]<- temp$thetas

    rm(temp)


  }

  #Burn
  ret <-list("mu" = mu[(burn+1):length(mu)],
             "tau2" = tau2[(burn+1):length(tau2)],
             "thetas" = thetas[((burn+1):nrow(thetas)),], 
             "gammas" = gammas[((burn+1):nrow(gammas)),])
  ret
}





