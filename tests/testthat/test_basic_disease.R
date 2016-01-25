library(testthat)

context("Basic Disease")

test_that("Test of object returned from 'psygenetDisease' is S4 and 'PsyGeNET.Psy'", {
  dta <- psygenetDisease(disease = "Alcoholism", 
  database = "CURATED", verbose = FALSE)
  
  expect_true(isS4(dta), info="'dta' is not a S4 object.")
  expect_is(dta, "DataGeNET.Psy", info="'dta' is not an 'DataGeNET.Psy'.")
})

test_that("Wrong database on 'psygenetDisease'", {
  expect_error(
    dta <- psygenetDisease(disease = "Alcoholism", 
      database = "", verbose = FALSE)
  )
})

test_that("Query single and existing disease into PsyGeNET", {
  dta <- psygenetDisease(disease = "Alcoholism", 
    database = "CURATED", verbose = FALSE)
})

test_that("Query multiple and existing disease into PsyGeNET", {
  dta <- psygenetDisease(disease = c("Alcoholism", "Cocaine Dependence"),
    database = "CURATED", verbose = FALSE)
})

test_that("Query single and non existing disease into PsyGeNET", {
  expect_warning(
    dta <- psygenetDisease(disease = "NAlcoholism", 
      database = "CURATED", verbose = FALSE)
  )
})

test_that("Query multiple and non existing disease into PsyGeNET", {
  expect_warning(
    dta <- psygenetDisease(disease = c("NAlcoholism", "NCocaine Dependence"),
      database = "CURATED", verbose = FALSE)
  )
})

test_that("Query multiple and existing and non existing disease into PsyGeNET", {
  expect_warning(
    dta <- psygenetDisease(disease = c("NAlcoholism", "Cocaine Dependence"),
      database = "CURATED", verbose = FALSE)
  )
})

test_that("Query multiple, existing and duplicated disease into PsyGeNET", {
  expect_warning(
    dta <- psygenetDisease( 
      disease  = c("Alcoholism", "Cocaine Dependence", "Alcoholism"),
      database = "CURATED", 
      verbose  = FALSE 
    )
  )
})
