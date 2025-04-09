## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(serosv)
library(dplyr)
library(magrittr)

## -----------------------------------------------------------------------------
linelisting <- parvob19_fi_1997_1998[order(parvob19_fi_1997_1998$age), ]
aggregated <- hav_bg_1964

# View the 2 different data format
head(linelisting)
head(aggregated)

# fit with aggregated data
model1 <- polynomial_model(aggregated, type = "Muench")
plot(model1)
# fit with linelisting data
model2 <- linelisting %>% 
  rename(status = seropositive) %>% 
  polynomial_model(type = "Muench")
plot(model2)

## -----------------------------------------------------------------------------
transform_data(
  linelisting$age, 
  linelisting$seropositive,
  heterogeneity_col = "age") %>% 
  polynomial_model(type = "Muench") %>% 
  plot()

