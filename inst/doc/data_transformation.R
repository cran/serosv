## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## -----------------------------------------------------------------------------
linelisting_data <- hcv_be_2006
aggregated_data <- transform_data(linelisting_data$dur, linelisting_data$seropositive)
aggregated_data

