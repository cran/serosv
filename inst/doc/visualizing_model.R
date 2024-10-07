## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## -----------------------------------------------------------------------------
hav <- hav_be_1993_1994
model <- fp_model(
  hav$age, hav$pos, hav$tot,
  p=c(1.5, 1.6), link="cloglog")
plot(model)

## ----warning=FALSE------------------------------------------------------------
rubella <- rubella_uk_1986_1987

farrington_md <- farrington_model(
   rubella$age, pos = rubella$pos, tot = rubella$tot,
   start=list(alpha=0.07,beta=0.1,gamma=0.03)
   )
plot(farrington_md)

## -----------------------------------------------------------------------------
a <- hav_bg_1964
pos <- a$pos
age <- a$age
tot <- a$tot
gf_model <- polynomial_model(age, pos = pos, tot = tot, type = "Griffith")

# customize plot
plot(gf_model) +
  set_plot_style(
    sero = "#3de071",
    foi = "#2f22e0",
    ci = "#aaf2b2",
    foi_line = "dotted", 
    sero_line = "dotdash"
  )

