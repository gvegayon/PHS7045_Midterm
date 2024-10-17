############################################################'
# gibbs_mh_run.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Wrapper for full algorithm
#
# Advanced R Goals: 
#       1) Use data table
#
#
# Notes: - Use set_hyperparameters function to define hypers
#############################################################'    

gibbs_mh_run <- function(Y,      #Data
                         mu,     #Current mu estimate
                         tau2,   #Current tau2 estimate
                         gammas, #Vector size k
                         thetas, #Vector size k
                         k,      #Number of observations in meta-analysis
                         hypers  #Hyperparameters
){
  print("Hello World")
}




# gibbs(dat,  
#       mu_test,     
#       tau2_test,   
#       gammas_test, 
#       thetas_test, 
#       7,           
#       hypers 
# )