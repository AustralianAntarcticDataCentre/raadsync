
context("exports")

agetfile <- "http://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2"
test_that("exported functions", {
  expect_that(oceandata_parameter_map("V"), is_a("data.frame"))
  expect_that(oceandata_parameter_map("T"), is_a("data.frame"))
  expect_that(op <- oceandata_parameters(), is_a("data.frame"))
  expect_that(ncol(op), equals(3L))
  expect_that(opm <- oceandata_platform_map(), is_a("data.frame"))
  expect_that(ncol(opm), equals(2L))
  expect_that(otpm <- oceandata_timeperiod_map(), is_a("data.frame"))
  expect_that(ncol(otpm), equals(2L))
  expect_that(dirname(oceandata_url_mapper(agetfile)), equals("oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032"))

})
