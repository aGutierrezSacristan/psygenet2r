library(testthat)

context("Basic Gene")

test_that("Test of object returned from 'psygenetGene' is S4 and 'PsyGeNET.Psy'", {
  dta <- psygenetGene(gene = "SLC6A4", database = "CURATED", verbose = FALSE)
  
  expect_true(isS4(dta), info="'dta' is not a S4 object.")
  expect_is(dta, "DataGeNET.Psy", info="'dta' is not an 'DataGeNET.Psy'.")
})

test_that("Wrong database on 'psygenetGene'", {
  expect_error(
    dta <- psygenetGene(gene = "SLC6A4", database = "", verbose = FALSE)
  )
})

test_that("Query single and existing gene into PsyGeNET", {
  dta <- psygenetGene(gene = "SLC6A4", database = "CURATED", verbose = FALSE)
})

test_that("Query multiple and existing genes into PsyGeNET", {
  dta <- psygenetGene(gene = c("SLC6A4", "DRD2",  "MAOA"),
    database = "CURATED", verbose = FALSE)
})

test_that("Query single and non existing genes into PsyGeNET", {
  expect_warning(
    dta <- psygenetGene(gene = "NSLC6A4", 
      database = "CURATED", verbose = FALSE)
  )
})

test_that("Query multiple and non existing genes into PsyGeNET", {
  expect_warning(
    dta <- psygenetGene(gene = c("NSLC6A4", "NDRD2"),
      database = "CURATED", verbose = FALSE)
  )
})

test_that("Query multiple and existing and non existing genes into PsyGeNET", {
  expect_warning(
    dta <- psygenetGene(gene = c("NSLC6A4", "DRD2"),
      database = "CURATED", verbose = FALSE)
  )
})

test_that("Query multiple, existing and duplicated genes into PsyGeNET", {
  expect_warning(
    dta <- psygenetGene( 
      gene     = c("SLC6A4", "DRD2",  "MAOA", "SLC6A4", "DRD2"),
      database = "CURATED", 
      verbose  = FALSE 
    )
  )
})

test_that("Query existing Gene Id as character into PsyGeNET", {
  dta <- psygenetGene(gene = "4852", database = "CURATED", verbose  = FALSE)
  
  expect_equal(ndisease(dta), 13)
  expect_equal(ngene(dta), 1)
})

test_that("Query existing Gene Id as numeric into PsyGeNET", {
  dta <- psygenetGene(gene = 4852, database = "CURATED", verbose  = FALSE)
  
  expect_equal(ndisease(dta), 13)
  expect_equal(ngene(dta), 1)
})
