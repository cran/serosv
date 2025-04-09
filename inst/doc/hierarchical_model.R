## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## -----------------------------------------------------------------------------
df <- mumps_uk_1986_1987
model <- hierarchical_bayesian_model(df, type="far3")

model$info
plot(model)

## -----------------------------------------------------------------------------
df <- rubella_uk_1986_1987
model <- hierarchical_bayesian_model(df, type="log_logistic")

model$type
plot(model)

