context("sync")

cf <- read_repo_config(local_config_file = NULL)
cf <- subset(cf, name == "NSIDC SMMR-SSM/I Nasateam sea ice concentration")
cf$do_sync <- TRUE

cf$method_flags <- paste(cf$method_flags, "--accept=\"*nt_2007030*\"")


lfr <- tempdir()
cf$local_file_root <- lfr
tf <- tempfile()
print(tf)
test_that("sync  works", {
  #sink(tf)
  expect_true(sync_repo(cf, verbose = FALSE))
  #sink(NULL)
})

test_that("sync got some files", {
  files <- data.frame(fullname = list.files(lfr, recursive = TRUE, full.names = TRUE))
  expect_gt(nrow(files), 0L)
})
