library(testthat)
library(GoeFlexStatCrawler)

#test_check("GoeFlexStatCrawler")
source("module_data.R")
test_dir("tests/testthat", reporter = "Summary")
#auto_test("R", "tests/testthat")
