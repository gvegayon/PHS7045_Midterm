############################################################'
# post_gamma.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Open form full conditional for gamma
#
# Notes:
#############################################################' 

post_gamma <- function(b_gamma, mu, tau2, theta, gamma, rt, rc){
  exp(gamma*(rt-rc))/
    ((1+exp(gamma - theta/2))^(rc)*(1+exp(gamma + theta/2))^(rt))*
    1/sqrt(2*pi*b_gamma)*
    exp(-1/(2*tau2)*(gamma)^2)
}
