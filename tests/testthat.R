Sys.setenv("R_TESTS" = "")

library(testthat)
library(GoeFlexStatCrawler)

#test_check("GoeFlexStatCrawler")
test_dir("tests/testthat")
