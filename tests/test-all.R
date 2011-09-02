library(testthat)
library(plyr)
library(plumbr)

test_dir(system.file("tests", package = "plumbr"), StopReporter)
