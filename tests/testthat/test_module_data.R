library(GoeFlexStatCrawler)
context("modul_data function")

# test, if output is as expected (i.e. output the same as entered directly on FlexStat)
test_that("Testing if the grade mean output of the function is the same as in FlexStat", {
  expect_equal(module_data(65, 12, 31463)[,8], "1.369") #SS2017, Wiwi Fac, Advanced Statistical Programming with R
  expect_equal(module_data(66, 12, 217)[,8], c("3.263", "3.039"))   #S17/18, WiWi Fac., Econometrics I
  expect_equal(module_data(67, 4, 58844)[,8], c("1.100", "-", "2.050", "-")) #WSS2018, Phil. Fac, Basic seminar linguistics 1.2
  expect_equal(module_data(63, 6, 9039)[,8], c("-", "-")) #SS2016, Fac. for Physics, Physics II3428
  expect_equal(module_data(65, 5, 3428)[,8], "1.404") #SS2017, Fac, for Mathem. and Inform., module "Datenschutz und Datensicherheit"
})


# test, if error messages are produced, when arguments not entered correctly
test_that("Error messages if arguments are not entered correctly", {
  expect_error(module_data(85, 12, 217),
               "The chosen semester value was not entered in the correct form or does not exist.") # not existing semester value
  expect_error(module_data("one", 12, 217),
               "The chosen semester value was not entered in the correct form or does not exist.") # semester value entered in wrong form
  expect_error(module_data(64, 3289, 217),
               "The chosen faculty value was not entered in the correct form or does not exist.") # not existing faculty value
  expect_error(module_data(64, "twelve", 217),
               "The chosen faculty value was not entered in the correct form or does not exist.") # faculty value entered in wrong format
  expect_error(module_data(64, 12, 1000000000),
               "The chosen module value was not entered in the correct form or does not exist for the chosen faculty.") # not existing module value
  expect_error(module_data(64, 12, "Econometrics I"),
               "The chosen module value was not entered in the correct form or does not exist for the chosen faculty.") # module entered in wrong form
})
