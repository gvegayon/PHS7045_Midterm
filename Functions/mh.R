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
#           (maybe switch to cpp?) (maybe parallelize?)
#       2) Modularization
#       
#
#
# Notes: - Use set_hyperparameters function to define hypers
#        - Candidate density: Normal centered on 
#         current value and variance low (suggested
#         by Dan in class for scaling 0.23, but can 
#         set using the set_parameters function)
#         - Depending on size of k, check for efficiency of parallelizing
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
  
  #Pre allocate memory
  thetas_new <- thetas
  gammas_new <- gammas
  

  #Define candidates
  candidates_thetas <- rnorm(k, 
                             mean(as.numeric(thetas)), 
                             hypers$mh_scale)
  
  candidates_gammas <- rnorm(k, 
                             mean(as.numeric(gammas)), 
                             hypers$mh_scale)
  
  #Define critical value
  crit <- log(runif(1))
  
  #Run posterior function on current and proposed data
  # for thetas and gammas and compare to crit value in a single loop
  
  for(i in 1:k){
    
    #Thetas 
    temp_theta_current <- post_theta(tau2 = tau2,
                                         mu = mu, 
                                         gamma = gammas[i], 
                                         theta = thetas[i],
                                         rt = Y[i, "rit"],
                                         rc = Y[i, "ric"]) |>
      log()
    
    temp_theta_candidate <- post_theta(tau2 = tau2,
                                         mu = mu, 
                                         gamma = candidates_gammas[i], 
                                         theta = candidates_thetas[i],
                                         rt = Y[i, "rit"],
                                         rc = Y[i, "ric"]) |>
      log()
    
    if(crit < (temp_theta_candidate - temp_theta_current)){
      thetas_new[i]<- candidates_thetas[i]
    }
    
    #Gammas
    temp_gamma_current <- post_gamma(b_gamma = hypers$b_gamma,
                                     tau2 = tau2,
                                         mu = mu, 
                                         gamma = gammas[i], 
                                         theta = thetas[i],
                                         rt = Y[i, "rit"],
                                         rc = Y[i, "ric"]) |>
      log()
    
    temp_gamma_candidate <- post_gamma(b_gamma = hypers$b_gamma,
                                       tau2 = tau2,
                                           mu = mu, 
                                           gamma = candidates_gammas[i], 
                                           theta = candidates_thetas[i],
                                           rt = Y[i, "rit"],
                                           rc = Y[i, "ric"]) |>
      log()
    
    if(crit < (temp_gamma_candidate - temp_gamma_current)){
      gammas_new[i]<- candidates_gammas[i]
    }
    
    rm(temp_gamma_candidate,
       temp_gamma_current,
       temp_theta_candidate,
       temp_theta_current)
  } #End for loop
  
 ret <- list("thetas" = thetas_new,
             "gammas" = gammas_new)
 
 ret
}
