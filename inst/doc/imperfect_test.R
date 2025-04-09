## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)
library(ggplot2)

## -----------------------------------------------------------------------------
# ---- estimate real prevalence using Bayesian approach ----
data <- rubella_uk_1986_1987
output <- correct_prevalence(data, warmup = 1000, iter = 4000, init_se=0.9, init_sp = 0.8, study_size_se=1000, study_size_sp=3000)

# check fitted value 
output$info[1:2, ]

# ---- estimate real prevalence using frequentist approach ----
freq_output <- correct_prevalence(data, bayesian = FALSE, init_se=0.9, init_sp = 0.8)

# check info
freq_output$info

## -----------------------------------------------------------------------------
# compare original prevalence and corrected prevalence
ggplot()+
  geom_point(aes(x = data$age, y = data$pos/data$tot, color="apparent prevalence")) + 
  geom_point(aes(x = output$corrected_se$age, y = output$corrected_se$sero, color="estimated prevalence (bayesian)" )) +
  geom_point(aes(x = freq_output$corrected_se$age, y = freq_output$corrected_se$sero, color="estimated prevalence (frequentist)" )) +
  scale_color_manual(
    values = c(
      "apparent prevalence" = "red", 
      "estimated prevalence (bayesian)" = "blueviolet",
      "estimated prevalence (frequentist)" = "royalblue")
  )+ 
  labs(x = "Age", y = "Prevalence")

## -----------------------------------------------------------------------------
suppressWarnings(
  corrected_data <- farrington_model(
  output$corrected_se,
  start=list(alpha=0.07,beta=0.1,gamma=0.03))
)

plot(corrected_data)

## -----------------------------------------------------------------------------
suppressWarnings(
  corrected_data <- farrington_model(
  freq_output$corrected_se,
  start=list(alpha=0.07,beta=0.1,gamma=0.03))
)

plot(corrected_data)

## -----------------------------------------------------------------------------
suppressWarnings(
  original_data <- farrington_model(
  data,
  start=list(alpha=0.07,beta=0.1,gamma=0.03))
)
plot(original_data)

