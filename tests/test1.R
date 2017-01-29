library(testthat)
library(PackageForCoursera)
test1<-"accident_2012.csv.bz2"
expect_equal(test1, make_filename(2012))
