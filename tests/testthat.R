library(testthat)
library(GoeFlexStatCrawler)

#test_check("GoeFlexStatCrawler")
test_dir("tests/testthat", reporter = "Summary")
#auto_test("R", "tests/testthat")
