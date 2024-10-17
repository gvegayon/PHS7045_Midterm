############################################################'
# post_tau2.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Closed form full conditional for tau
#
# Notes:
#############################################################' 


post_tau <- function(k, a_tau, b_tau, mu, theta){
  1/rgamma(1,
           shape = k/2 + a_tau,
           rate = 0.5*sum((theta-mu)^2) + b_tau)
}
