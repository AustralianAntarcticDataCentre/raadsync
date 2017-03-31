context("repo summary")

test_that("config file looks OK", {
  expect_that(  cf <- read_repo_config(local_config_file = NULL), is_a("data.frame"))
  expect_that(all(cf$do_sync), is_false())
})

test_that("repo summary is sensible", {
    ##skip("skipping repo_summary because it fails with travis-ci for an unknown reason")
    skip_if_not(rmarkdown::pandoc_available("1.12.3"),"skipping repo_summary test because pandoc is not available or is not a recent enough version")
    cf <- read_repo_config(local_config_file = NULL)
    summary_filename <- repo_summary(cf)
    expect_true(file.exists(summary_filename))
})
