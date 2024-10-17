############################################################'
# read_data.R
# 
# Author: Sophie Huebler
# Creation Date: 15Oct2024
#
# Purpose: Read example data for a meta analysis where
#         there were 7 trials with the number of successes
#         in the treatment group (rit) and control group
#         (ric), and the sample sizes in both groups (nit, nic),
#         and the log dds ratio and variance of the log odds ratio.
#
# Notes:
#       - Rewrite the second step to reformat manually instead
#         of importing metafor::escalc function or dplyr verbs
#############################################################' 


# Reading in Data
dat_raw <- read.table(text = "
                  trial rit nit ric nic
                 Balcon 14 56 15 58
                Clausen 18 66 19 64
            Multicentre 15 100 12 95
                 Barber 10 52 12 47
                 Norris 21 226 24 228
                 Kahler 3 38 6 31
                Ledwich 2 20 3 20
                  ", header = TRUE)

# Formatting Data
# Note: For package creation rewrite without dependencies

dat <- metafor::escalc(measure = "OR",
                       ai = rit, n1i = nit,
                       ci = ric, n2i = nic,
                       slab = trial,
                       data = dat_raw,
                       add=0) |>
  dplyr::rename(theta_i = yi,
                v_i = vi)|>
  dplyr::mutate(sd_i = sqrt(v_i))|>
  dplyr::mutate(lower_ci = theta_i - 1.96*sd_i,
                upper_ci = theta_i + 1.96*sd_i)|>
  dplyr::mutate(gamma_i = 0.5*(qlogis(rit/nit)+qlogis(ric/nic)))
