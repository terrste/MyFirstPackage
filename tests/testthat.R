iif(!require(testthat)) install.packages("testthat",repos = "http://cran.us.r-project.org")
library(testthat)
library(MyFirstPackage)

test_check("MyFirstPackage")
