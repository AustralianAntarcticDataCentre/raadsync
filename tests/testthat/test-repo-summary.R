library(testthat)
library(raadsync)

context("repo summary")


test_that("repo summary is sensible", {

   expect_that(  cf <- read_repo_config(local_config_file = NULL), is_a("data.frame"))
  expect_that(all(cf$do_sync), is_false())
})
