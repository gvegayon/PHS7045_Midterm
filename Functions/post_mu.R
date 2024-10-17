############################################################'
# post_mu.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Closed form full conditional for mu
#
# Notes:
#############################################################' 

post_mu <- function(k, sigma, theta, b_mu){
  rnorm(1, 
        mean = sigma*sum(theta)/(k*sigma + 100),
        sd = 1/sqrt(k*sigma + 100))
}
