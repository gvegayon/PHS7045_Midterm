############################################################'
# mh.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Metropolis-Hastings procedure for
#           componentwise updating of theta, gamma
#
# Advanced R Goals: 
#       1) Remove lapply statements replace with single loop
#           (maybe switch to cpp?)
#       2) Modularization
#       3) Pre allocating storage
#
#
# Notes: - Use set_hyperparameters function to define hypers
#        - Candidate density: Normal centered on 
#         current value and variance low (suggested
#         by Dan in class for scaling 0.23, but can 
#         set using the set_parameters function)
#############################################################'    

##Metropolis-Hastings procedure for
#componentwise updating of theta, gamma
mh <- function(Y,      #Data
               mu,     #Current mu estimate
               tau2,   #Current tau2 estimate
               gammas, #Vector size k
               thetas, #Vector size k
               k,      #Number of observations in meta-analysis
               hypers  #Hyperparameters
               ){
  
  #Pre allocate space for vectors from for loop
  ##Current posteriors
  current_post_thetas <- numeric(k)
  current_post_gammas <- numeric(k)
  
  ##Candidate posteriors
  candidate_post_thetas <- numeric(k)
  candidate_post_gammas <- numeric(k)
  
  
  
  #Define candidates
  candidates_thetas <- rnorm(k, 
                             mean(as.numeric(thetas)), 
                             hypers$mh_scale)
  
  candidates_gammas <- rnorm(k, 
                             mean(as.numeric(gammas)), 
                             hypers$mh_scale)
  
  #Run posterior function on current and proposed data
  # for thetas and gammas in a single loop
  
  for(i in 1:k){
    
    #Thetas 
    current_post_thetas[i] <- post_theta(tau2 = tau2,
                                         mu = mu, 
                                         gamma = gammas[i], 
                                         theta = thetas[i],
                                         rt = Y[i, "rit"],
                                         rc = Y[i, "ric"]) |>
      log()
    
    candidate_post_thetas[i] <- post_theta(tau2 = tau2,
                                         mu = mu, 
                                         gamma = candidates_gammas[i], 
                                         theta = candidates_thetas[i],
                                         rt = Y[i, "rit"],
                                         rc = Y[i, "ric"]) |>
      log()
    
    #Gammas
    current_post_gammas[i] <- post_gamma(tau2 = tau2,
                                         mu = mu, 
                                         gamma = gammas[i], 
                                         theta = thetas[i],
                                         rt = Y[i, "rit"],
                                         rc = Y[i, "ric"]) |>
      log()
    
    candidate_post_gammas[i] <- post_gamma(tau2 = tau2,
                                           mu = mu, 
                                           gamma = candidates_gammas[i], 
                                           theta = candidates_thetas[i],
                                           rt = Y[i, "rit"],
                                           rc = Y[i, "ric"]) |>
      log()
    
  }
  
  #Divide posteriors = subtract log posteriors
  logr_theta = proposed_post_theta - current_post_theta
  logr_gamma = proposed_post_gamma - current_post_gamma
  
  #Update based R function
  #Using uniformdensity
  crit <- log(runif(1))
  criteria_theta <- crit < logr_theta
  criteria_gamma <- crit < logr_gamma
  
  #Vectorization
  
  #Inner loop (fix later for efficiency)
  #Update estimates if ratio of posterior of current
  #and proposed estimates is less than the log function
  for(i in 1:k){
    if(criteria_theta[i]){
      thetas[j,i]<-candidates_thetas[i]
    }else{
      thetas[j,i] <- thetas[(j-1),i]
    }
    if(criteria_gamma[i]){
      gammas[j,i]<-candidates_gammas[i]
    }else{
      gammas[j,i] <- gammas[(j-1),i]
    }
  }
}