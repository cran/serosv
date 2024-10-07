## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## -----------------------------------------------------------------------------
data <- parvob19_be_2001_2003
pl <- penalized_spline_model(data$age, status = data$seropositive, s = "tp", framework = "pl") 
pl$info

## ----fig.width=7, fig.height=4------------------------------------------------
plot(pl)

## -----------------------------------------------------------------------------
data <- parvob19_be_2001_2003
glmm <- penalized_spline_model(data$age, status = data$seropositive, s = "tp", framework = "glmm") 
glmm$info$gam

## ----fig.width=7, fig.height=4------------------------------------------------
plot(glmm)

