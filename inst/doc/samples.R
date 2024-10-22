## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=T-------------------------------------------------------------------
library(CRuff)

## -----------------------------------------------------------------------------
attach(Europe)
sto=aggregate(Stat~Sex*Region,FUN=mean)
Ord=sort(sto$Stat,index.ret=T)$ix
print(sto[Ord,])

## -----------------------------------------------------------------------------
print(table(data.frame(Sex,Region)))

