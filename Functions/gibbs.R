############################################################'
# gibbs.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Single iteration of a gibbs sampling
# Advanced R Goals: 
#       1) Remove for loop for the simulations (parallelized later)
#       2) Modularization
# Notes: 
#     - Use set_hyperparameters function to define hypers
#     - Source mh function
#############################################################'    

#Gibbs implementation - update mu, tau2, gammas, thetas
gibbs <- function(mu, 
                  tau2,
                  gammas,
                  thetas,
                  k,
                  hypers){
  
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





}