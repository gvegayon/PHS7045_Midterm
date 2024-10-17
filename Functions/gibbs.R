############################################################'
# gibbs.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Single iteration of a gibbs sampling
# Advanced R Goals: 
#       1) Remove for loop for the simulations 
#       2) Modularization
# Notes: 
#     - Use set_hyperparameters function to define hypers
#     - Source mh function
#############################################################'    

#Gibbs implementation - update mu, tau2, gammas, thetas
gibbs <- function(Y,      #Data
                  mu,     #Current mu estimate
                  tau2,   #Current tau2 estimate
                  gammas, #Vector size k
                  thetas, #Vector size k
                  k,      #Number of observations in meta-analysis
                  hypers  #Hyperparameters
){
  
  #Update mu first
  mu_new <- post_mu(k=k, 
                    sigma = 1/tau2, 
                    theta = thetas,
                    b_mu = hypers$b_mu)
  
  
  #Update tau2 using the mu_new
  tau2_new <- post_tau(k = k,
                       a_tau = hypers$a_tau2,
                       b_tau = hypers$b_tau2,
                       mu = mu_new,
                       theta = thetas)
  
  mh_iter <- mh(Y,     
                mu_new,     
                tau2_new,   
                gammas, 
                thetas, 
                k,      
                hypers  
                
  )
  
  ret <- list("mu" = mu_new,
              "tau2" = tau2_new,
              "thetas" = mh_iter$thetas,
              "gammas" = mh_iter$gammas)
  
  ret
  
}


gibbs(dat,  
      mu_test,     
      tau2_test,   
      gammas_test, 
      thetas_test, 
      7,           
      hypers 
)
