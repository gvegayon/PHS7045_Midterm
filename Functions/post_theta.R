############################################################'
# post_theta.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Open form full conditional for theta
#
# Notes:
#############################################################' 


post_theta <- function(tau2, mu, gamma, theta, rt, rc){
  exp(theta/2*(rt-rc))/
    ((1+exp(gamma - theta/2))^(rc)*(1+exp(gamma + theta/2))^(rt))*
    1/sqrt(2*pi*tau2)*
    exp(-1/(2*tau2)*(theta-mu)^2)
}