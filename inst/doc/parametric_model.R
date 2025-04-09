## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE, warning=FALSE, message=FALSE------------------------
library(serosv)
library(dplyr)
library(magrittr)

## -----------------------------------------------------------------------------
data <- hav_bg_1964

## -----------------------------------------------------------------------------
muench1 <- polynomial_model(data, k = 1)
summary(muench1$info)

muench2 <- polynomial_model(data, type = "Muench")
summary(muench2$info)

## -----------------------------------------------------------------------------
plot(muench2) 

## -----------------------------------------------------------------------------
gf_model <- polynomial_model(data, type = "Griffith")
plot(gf_model)

## -----------------------------------------------------------------------------
grf_model <- polynomial_model(data, type = "Grenfell")
plot(grf_model)

## ----warning=FALSE------------------------------------------------------------
farrington_md <- farrington_model(
   rubella_uk_1986_1987,
   start=list(alpha=0.07,beta=0.1,gamma=0.03)
   )
plot(farrington_md)

## -----------------------------------------------------------------------------
hcv <- hcv_be_2006[order(hcv_be_2006$dur), ]

wb_md <- hcv %>% 
  rename(
    t = dur, status = seropositive
  ) %>% weibull_model()
plot(wb_md) 

## ----warning=FALSE------------------------------------------------------------
hav <- hav_be_1993_1994
best_p <- find_best_fp_powers(
  hav,
  p=seq(-2,3,0.1), mc=FALSE, degree=2, link="cloglog"
)
best_p

## -----------------------------------------------------------------------------
model <- fp_model(hav, p=c(1.5, 1.6), link="cloglog")
plot(model)

