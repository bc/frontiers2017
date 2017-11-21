## load dependencies
library(testthat)
load_all()
options(rgl.useNULL = TRUE) 
devtools::install()
## test package
devtools::test()
