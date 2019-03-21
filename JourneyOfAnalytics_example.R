# Author - Ann Venkataraman
# Date - 01 aug 2017
# Description - Monte Carlo Simulations in R

# Laod library packages:
library(data.table)

# set global options:
options(scipen = 999)   # we don't want to use scientific notation .
options(digits = 2)   # we want only 2 digits after the decimal.




# Basic Monte Carlo code: 
runs <- 100000
sims <- rnorm(runs,mean=4,sd=10)
mcmc_val <- sum(sims >= 3 & sims <= 6)/runs



# For a given group of 8th grade male students, the average weight
# is 160 pounds and the standard deviation is 5.5 pounds. What is the probability
# that a student weighs over 205 pounds if we were to sample 5000 similar students?
runs <- 5000
sims <- rnorm(runs,mean=160,sd=5.5)
mcmc_val <- sum(sims >= 205)/runs
# Answer - probability is almost 0. 


# Probability for weight of student = 165 pounds.
runs <- 5000
sims <- rnorm(runs,mean=160,sd=5.5)
mcmc_val <- sum(sims >= 165)/runs
# Answer - 0.2 i.e  a 20% chance .


# Probability of getting more than 6 heads if we flip a coin 10 times.
# We conduct this experiemnt 10000
runs <- 10000
coin_trial <- function(){
   sum(sample(c(0,1), size =10, replace = T)) > 6
}
coin_flips <- sum(replicate(runs, coin_trial())) / runs


# Assume 3 dice are rolled simultaneousely. What is the probability of 
# getting 6 on all 3 dice ?
runs_500k <- 50000
dice_roll <- function(){
   x = sample(1:6, size =3, replace = T)
   # x
   # x >= 6
   # (sum(x >= 6)/3) == 1
   return( (sum(x >= 6)/3) == 1)
}
dice_check <- sum(replicate(runs_500k, dice_roll())) / runs_500k



runs_500k <- 50000
dice_roll <- function(){
   x = sample(1:6, size =6, replace = T)
   # x
   # x >= 6
   # (sum(x >= 6)/3) == 1
   return( (sum(x >= 6)/6) )
}
dice_check <- replicate(runs_500k, dice_roll()) 
table(dice_check)


plot(table(dice_check), xlab = 'Dice_probability', ylab = 'Frequency', 
     main = 'All 3 dice concurrently show up as 6')







# A/B testing for pagebounce. 
runs <- 100000
site1 <- rbeta(runs,12,19)
site2 <- rbeta(runs,47,320)
site_pval <- sum(site1 > site2)/runs

site_chk <- sum(site1 <= site2) / runs

