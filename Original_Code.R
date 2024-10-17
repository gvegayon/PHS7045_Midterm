#functions based on conditional posteriors 

#closed form for mu and tau
# post_mu <- function(k, sigma, theta, b_mu){
#   rnorm(1, 
#         mean = sigma*sum(theta)/(k*sigma + 100),
#         sd = 1/sqrt(k*sigma + 100))
# }
# 
# post_tau <- function(k, a_tau, b_tau, mu, theta){
#   1/rgamma(1,
#            shape = k/2 + a_tau,
#            rate = 0.5*sum((theta-mu)^2) + b_tau)
# }
# 
# #no closed form for gammas and thetas
# post_theta <- function(tau2, mu, gamma, theta, rt, rc){
#   exp(theta/2*(rt-rc))/
#     ((1+exp(gamma - theta/2))^(rc)*(1+exp(gamma + theta/2))^(rt))*
#     1/sqrt(2*pi*tau2)*
#     exp(-1/(2*tau2)*(theta-mu)^2)
# }
# 
# post_gamma <- function(b_gamma, mu, tau2, theta, gamma, rt, rc){
#   exp(gamma*(rt-rc))/
#     ((1+exp(gamma - theta/2))^(rc)*(1+exp(gamma + theta/2))^(rt))*
#     1/sqrt(2*pi*b_gamma)*
#     exp(-1/(2*tau2)*(gamma)^2)
# }



mh_gibbs_original <- function(#Y a data frame with rit, nit, ric, nic, theta_i, gamma_i
  Y,
  
  #hyperparams
  #mean and var for mu
  a_mu = 0,
  b_mu = 100,
  #mean and var for gamma
  a_gamma = 0,
  b_gamma = 100,
  #shape and scale for 1/tau2
  a_tau2 = 0.001,
  b_tau2 = 0.001,
  
  #Number of simulations
  sim = 1e5,
  
  #Burn
  burn = 2000){
  
  # Set up  
  #number of studies
  k <- nrow(Y)
  
  #initial values based on data and hyperparameters
  #thetas initialized by odds ratio in data
  thetas <- data.frame(matrix(as.vector(Y[,"theta_i"]),
                              nrow = 1))%>%
    mutate_all(as.numeric)
  
  #gammas initialized by average logit in data
  gammas <- data.frame(matrix(as.vector(Y[,"gamma_i"]),
                              nrow = 1))%>%
    mutate_all(as.numeric)
  
  #mu initialized by average theta
  mu <- mean(thetas[1,]%>%as.numeric())
  
  #tau initialized by variance of thetas
  tau2 <- sd(thetas)^2
  
  
  #Outer loop - number of simulations
  for(j in 2:sim){
    #Gibbs implementation - start with mu,tau 
    
    mu_new <- post_mu(k=k, 
                      sigma = 1/tau2[(j-1)], 
                      theta = thetas[(j-1),],
                      b_mu = b_mu)
    mu <- c(mu, mu_new)
    
    tau2_new <- post_tau(k = k,
                         a_tau = a_tau2,
                         b_tau = b_tau2,
                         mu = mu[j],
                         theta = thetas[(j-1),])
    
    tau2 <- c(tau2, tau2_new)
    
    
    #Metropolis-Hastings procedure for
    #componentwise updating of theta, gamma
    
    #Candidate density: Normal centered on 
    #current value and variance low (suggested
    #by Dan in class for scaling 0.23)
    
    candidates_thetas <- rnorm(k, mean(thetas[(j-1),]%>%
                                         as.numeric()), 0.23)
    candidates_gammas <- rnorm(k, mean(gammas[(j-1),]%>%
                                         as.numeric()), 0.23)
    
    ##Run posterior function on current and proposed data
    
    #Theta
    #Find the log posterior for the currrent estimates
    current_post_theta <- lapply(1:k, function(i)
      post_theta(
        tau2 = tau2[j],
        mu = mu[j],
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
    }#end inner loop
    
  }#end outer loop
  
  #print(thetas[(burn+1:nrow(thetas)),])
  
  #Burn
  ret <-list("mu" = mu[(burn+1):length(mu)],
             "tau2" = tau2[(burn+1):length(tau2)],
             "thetas" = thetas[(burn+1:nrow(thetas)),]%>%
               filter(complete.cases(.)), 
             "gammas" = gammas[(burn+1:nrow(gammas)),]%>%
               filter(complete.cases(.)))
  ret
}
