packages <- c("MASS", 
              "R.matlab", 
              "psych", 
              "quantmod", 
              "rvest", 
              "dplyr", 
              "TTR",
              "nlshrink", 
              "uuid", 
              "R.utils", 
              "pcaPP", 
              "reticulate", 
              "clime", 
              "RcppCNPy")

packages_to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}
