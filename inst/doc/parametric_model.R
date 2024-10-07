## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## -----------------------------------------------------------------------------
a <- hav_bg_1964
neg <- a$tot -a$pos
pos <- a$pos
age <- a$age
tot <- a$tot

## -----------------------------------------------------------------------------
muench1 <- polynomial_model(age, pos = pos, tot = tot, k = 1)
summary(muench1$info)

muench2 <- polynomial_model(age, pos = pos, tot = tot, type = "Muench")
summary(muench2$info)

## -----------------------------------------------------------------------------
plot(muench2) 

## -----------------------------------------------------------------------------
gf_model <- polynomial_model(age, pos = pos, tot = tot, type = "Griffith")
plot(gf_model)

## -----------------------------------------------------------------------------
grf_model <- polynomial_model(age, pos = pos, tot = tot, type = "Grenfell")
plot(grf_model)

## ----warning=FALSE------------------------------------------------------------
rubella <- rubella_uk_1986_1987
rubella$neg <- rubella$tot - rubella$pos

farrington_md <- farrington_model(
   rubella$age, pos = rubella$pos, tot = rubella$tot,
   start=list(alpha=0.07,beta=0.1,gamma=0.03)
   )
plot(farrington_md)

## -----------------------------------------------------------------------------
hcv <- hcv_be_2006[order(hcv_be_2006$dur), ]
dur <- hcv$dur
infected <- hcv$seropositive

wb_md <- weibull_model(
   t = dur,
   status = infected
   )
plot(wb_md) 

## ----warning=FALSE------------------------------------------------------------
hav <- hav_be_1993_1994
best_p <- find_best_fp_powers(
  hav$age, pos = hav$pos,tot = hav$tot,
  p=seq(-2,3,0.1), mc=FALSE, degree=2, link="cloglog"
)
best_p

## -----------------------------------------------------------------------------
model <- fp_model(
  hav$age, pos = hav$pos, tot = hav$tot,
  p=c(1.5, 1.6), link="cloglog")
compute_ci.fp_model(model)
plot(model)

