## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(tabularise)
library(equatiomatic)

## -----------------------------------------------------------------------------
data("ToothGrowth", package = "datasets")
# Add labels and units
ToothGrowth <- svBase::labelise(ToothGrowth,
  label = list(len = "Tooth growth", supp = "Supplement", dose = "Vitamin C"),
  units = list(len = "mm", dose = "mg/d"))

## -----------------------------------------------------------------------------
head(ToothGrowth)

## -----------------------------------------------------------------------------
library(tabularise)
tabularise$headtail(ToothGrowth) 

## -----------------------------------------------------------------------------
tabularise$headtail(ToothGrowth) |>
  flextable::add_header_lines(
    values = "Effect of Vitamin C on Tooth Growth in Guinea Pigs")

## -----------------------------------------------------------------------------
eq <- equation("y = \\alpha + \\beta_1 \\cdot x + \\beta_2 \\cdot x^2")
eq

## -----------------------------------------------------------------------------
tg_lm <- lm(len ~ dose * supp, data = ToothGrowth)
tg_eq <- equation(tg_lm, terms_per_line = 3, wrap = TRUE)
tg_eq

