context("sync")

cf <- read_repo_config(local_config_file = NULL)
cf <- subset(cf, name == "NSIDC SMMR-SSM/I Nasateam sea ice concentration")
cf$do_sync <- TRUE
tok <- "--accept=\"*nt_200703*\""
cf$method_flags <- paste(cf$method_flags, "--accept=\"*nt_20160101*\"", "--accept=\"*nt_20170512*\"")

lfr <- tempdir()
cf$local_file_root <- lfr

test_that("sync  works", {
  expect_true(sync_repo(cf))
})

test_that("sync got some files", {
  files <- data.frame(fullname = file.path(lfr, list.files(lfr, recursive = TRUE)), stringsAsFactors = FALSE)
  expect_gt(nrow(files), 0L)
})

