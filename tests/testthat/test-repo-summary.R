
context("repo summary")


test_that("repo summary is sensible", {

  expect_that(  cf <- read_repo_config(local_config_file = NULL), is_a("data.frame"))
  
  expect_that(file.exists(repo_summary(cf)), is_true())
  expect_that(all(cf$do_sync), is_false())
})
