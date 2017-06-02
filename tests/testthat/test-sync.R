context("sync")

cf <- read_repo_config(local_config_file = NULL)
cf <- subset(cf, name == "NSIDC SMMR-SSM/I Nasateam sea ice concentration")

cf$do_sync <- TRUE
tok <- "--accept=\"*nt_200703*\""
cf$method_flags <- paste(cf$method_flags, "--accept=\"*nt_20120101*\"", "--accept=\"*nt_20130512*\"")

lfr <- tempdir()
cf$local_file_root <- lfr

test_that("sync  works", {
  expect_true(sync_repo(cf))
})

test_that("sync got some files", {
  files <- data.frame(fullname = file.path(lfr, list.files(lfr, recursive = TRUE)), stringsAsFactors = FALSE)
  expect_gt(nrow(files), 0L)
})


## this'll fail as we can't log in without the local config
cf <- read_repo_config(local_config_file = NULL)
cf <- subset(cf, name == "Ssalto/Duacs gridded mean and climatological sea level anomalies")
cf$do_sync <- TRUE
test_that("sync not works", {
  expect_output(sync_repo(cf), "Connection refused")
})

