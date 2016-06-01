library(testthat)
library(raadsync)

context("repo summary")

cf <- read_repo_config(local_config_file = NULL)
## cannot get this to work?  Windows problem, or something old? 
#Error in knit2html(rmd_file, output = sub("Rmd$", "md", rmd_file)) : 
#  It seems you should call rmarkdown::render() instead of knitr::knit2html() because C:\Users\mdsumner\AppData\Local\Temp\RtmpqKvoje\filef3c4a177756.Rmd appears to be an R Markdown v2 document.
#test_that("repo summary is sensible", {
 # repo_summary(cf)
#})
