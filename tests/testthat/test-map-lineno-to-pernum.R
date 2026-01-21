# tests/testthat/test-map-lineno-to-pernum.R

library(testthat)
library(tibble)
library(dplyr)
library(rprojroot)

root <- find_root(is_rstudio_project)
setwd(root)

source("src/helpers/clean-household-data.R")




test_that("convert_person_refs maps PELNMOM from LINENO to PERNUM within households", {
  
  input <- tibble(
    hhid    = c(1, 1, 1, 2, 2, 2, 2),
    perid   = c(1, 2, 3, 4, 5, 6, 7),
    PERNUM  = c(1, 2, 3, 1, 2, 3, 4),
    LINENO  = c(3, 1, 2, 2, 3, 4, 1),
    MOMLOC  = c(0, 1, 1, 0 , 0, 2, 2),
    PELNMOM = c(0, 3, 3, 0, 0, 3, 3)
  )
  
  expected <- tibble(
    hhid    = c(1, 1, 1, 2, 2, 2, 2),
    perid   = c(1, 2, 3, 4, 5, 6, 7),
    PERNUM  = c(1, 2, 3, 1, 2, 3, 4),
    LINENO  = c(3, 1, 2, 2, 3, 4, 1),
    MOMLOC  = c(0, 1, 1, 0 , 0, 2, 2),
    PELNMOM = c(0, 3, 3, 0, 0, 3, 3),
    PELNMOM_PERNUM = c(0, 1, 1, 0, 0, 2, 2)
  )
  
  result <- convert_person_refs(
    df = input,
    hhid_col = hhid,
    target_ref_col = "PERNUM",
    source_ref_col = "LINENO",
    translate_cols = "PELNMOM"
  )
  
  print(result)
  print(expected)
  
  expect_equal(result, expected)
})
