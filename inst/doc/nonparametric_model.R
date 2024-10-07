## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, output=FALSE------------------------------------------------------
library(serosv)

## -----------------------------------------------------------------------------
mump <- mumps_uk_1986_1987
age <- mump$age
pos <- mump$pos
tot <- mump$tot
y <- pos/tot

## ----fig.width=7, fig.height=3------------------------------------------------
plot_gcv(
   age, pos, tot,
   nn_seq = seq(0.2, 0.8, by=0.1),
   h_seq = seq(5, 25, by=1)
 )

## -----------------------------------------------------------------------------
lp <- lp_model(age, pos = pos, tot = tot, kern="tcub", nn=0.7, deg=2)
plot(lp)

