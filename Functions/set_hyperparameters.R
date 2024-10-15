############################################################'
# set_hyperparameters.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Set up hyperparameters 
#
# Notes:
#############################################################'


#hyperparams
set_hyperparameters <- function(
#mean and var for mu
a_mu = 0,
b_mu = 100,
#mean and var for gamma
a_gamma = 0,
b_gamma = 100,
#shape and scale for 1/tau2
a_tau2 = 0.001,
b_tau2 = 0.001,
#scale for mh
mh_scale = 0.23
){
  
  return(list("a_mu" = a_mu,
              "b_mu" = b_mu,
              "a_gamma" = a_gamma,
              "b_gamma" = b_gamma,
              "a_tau2" = a_tau2,
              "b_tau2" = b_tau2,
              "mh_scale" = mh_scale))
}
