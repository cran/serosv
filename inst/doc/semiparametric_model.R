## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)
library(dplyr)

## -----------------------------------------------------------------------------
pl <- parvob19_be_2001_2003 %>% 
  rename(status = seropositive) %>% 
  penalized_spline_model(s = "tp", framework = "pl") 
pl$info

## ----fig.width=7, fig.height=4------------------------------------------------
plot(pl)

## -----------------------------------------------------------------------------
glmm <- parvob19_be_2001_2003 %>% 
  rename(status = seropositive) %>% 
  penalized_spline_model(s = "tp", framework = "glmm") 
glmm$info$gam

## ----fig.width=7, fig.height=4------------------------------------------------
plot(glmm)

