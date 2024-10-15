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
#       1) Remove lapply statements replace with vectorized funcs for
#           the calculation of posteriors
#       2) Modularization
#
#
# Notes: - Use set_hyperparameters function to define hypers
#############################################################'    

##Metropolis-Hastings procedure for
#componentwise updating of theta, gamma
mh <- function(mu, 
               tau2,
               gammas,
               thetas,
               k,
               hypers){

#Candidate density: Normal centered on 
#current value and variance low (suggested
#by Dan in class for scaling 0.23, but can 
#set using the set_parameters function)

#Define candidates
candidates_thetas <- rnorm(k, 
                           mean(as.numeric(thetas)), 
                           hypers$mh_scale)

candidates_gammas <- rnorm(k, 
                           mean(as.numeric(gammas)), 
                           hypers$mh_scale)

##Run posterior function on current and proposed data

#Theta
#Find the log posterior for the currrent estimates
current_post_theta <- lapply(1:k, function(i)
  post_theta(
    tau2 = tau2,
    mu = mu,
    gamma = gammas[(j-1),i],
    theta = thetas[(j-1), i],
    rt = Y[i, "rit"],
    rc = Y[i, "ric"])
) %>%
  unlist() %>%
  log()

#Find the log posterior for the proposed estimates
proposed_post_theta <- lapply(1:k, function(i)
  post_theta(
    tau2 = tau2[j],
    mu = mu[j],
    gamma = candidates_gammas,
    theta = candidates_thetas,
    rt = Y[i, "rit"],
    rc = Y[i, "ric"])) %>%
  unlist() %>%
  log()

#Gamma 
#Find the log posterior for the current estimates
current_post_gamma<- lapply(1:k, function(i)
  post_gamma(
    b_gamma = b_gamma,
    mu = mu[j],
    tau2 = tau2[j],
    gamma = gammas[(j-1),i],
    theta = thetas[(j-1), i],
    rt = Y[i, "rit"],
    rc = Y[i, "ric"])
) %>%
  unlist() %>%
  log()

#Find the log posterior for the proposed estimates
proposed_post_gamma <- lapply(1:k, function(i)
  post_gamma(
    b_gamma = b_gamma,
    mu = mu[j],
    tau2 = tau2[j],
    gamma = candidates_gammas,
    theta = candidates_thetas,
    rt = Y[i, "rit"],
    rc = Y[i, "ric"])) %>%
  unlist() %>%
  log()


#Divide posteriors = subtract log posteriors
logr_theta = proposed_post_theta - current_post_theta
logr_gamma = proposed_post_gamma - current_post_gamma

#Update based R function
#Using uniformdensity
criteria_theta <- log(runif(1)) < logr_theta
criteria_gamma <- log(runif(1)) < logr_gamma

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