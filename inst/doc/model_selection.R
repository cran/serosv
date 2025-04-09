## ----setup, output=FALSE------------------------------------------------------
library(serosv)
library(dplyr)
library(magrittr)

## ----warning=FALSE------------------------------------------------------------
data <- parvob19_fi_1997_1998[order(parvob19_fi_1997_1998$age), ] %>% 
  rename(status = seropositive)
  
aggregated <- transform_data(data$age, data$status, heterogeneity_col = "age")

# fit with linelisting data
model1 <- polynomial_model(data, type = "Muench")
# fit with aggregated data
model2 <- polynomial_model(aggregated, type = "Muench")
# fit with aggregated data
model3 <- polynomial_model(aggregated, type = "Griffith")
# fit with gam
model4 <- penalized_spline_model(aggregated)

## -----------------------------------------------------------------------------
# provide models with name
compare_models(muench_linelist = model1, muench_aggregated = model2, griffith = model3, penalized_spline = model4)

# provide models without name
compare_models(model1, model2, model3, model4)

# user can provide arbitrary number of models
compare_models(model3, model4)

