## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
state <- c(S=4999, I=1, R=0)
parameters <- c(
  mu=1/75, # 1 divided by life expectancy (75 years old)
  alpha=0, # no disease-related death
  beta=0.0005, # transmission rate
  nu=1, # 1 year for infected to recover
  p=0 # no vaccination at birth
)
times <- seq(0, 250, by=0.1)
model <- sir_basic_model(times, state, parameters)
model$parameters
plot(model)

## -----------------------------------------------------------------------------
state <- c(s=0.99,i=0.01,r=0)
parameters <- c(
  lambda = 0.05,
  nu=1/(14/365) # 2 weeks to recover
)
ages<-seq(0, 90, by=0.01)
model <- sir_static_model(ages, state, parameters)
model$parameters
plot(model)

## -----------------------------------------------------------------------------
k <- 2 # number of population
state <- c(
  # proportion of each compartment for each population
  s = c(0.8, 0.6), 
  i = c(0.2, 0.4),
  r = c(  0,   0)
)
beta_matrix <- c(
  c(0.05, 0.00),
  c(0.00, 0.05)
)
parameters <- list(
  beta = matrix(beta_matrix, nrow=k, ncol=k, byrow=TRUE),
  nu = c(1/30, 1/30),
  mu = 0.001,
  k = k
)
times<-seq(0,10000,by=0.5)
model <- sir_subpops_model(times, state, parameters)
model$parameters
plot(model) # returns plot for each population

## -----------------------------------------------------------------------------
model <- mseir_model(
  a=seq(from=1,to=20,length=500), # age range from 0 -> 20 yo
  gamma=1/0.5, # 6 months in the maternal antibodies
  lambda=0.2,  # 5 years in the susceptible class
  sigma=26.07, # 14 days in the latent class
  nu=36.5      # 10 days in the infected class
)
model$parameters
plot(model)

