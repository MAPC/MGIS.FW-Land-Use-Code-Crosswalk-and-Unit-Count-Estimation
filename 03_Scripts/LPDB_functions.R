################################################################################
# Functions for use in MAPC's Land Parcel Database Research, Cleaning, and Spatial Operations
# Author: Brandon Stanaway 
# Contact: bstanaway@mapc.org, bstanaway.econ@gmail.com
# Github: 
# Date Created: 6/6/2025
# 
# Purpose: Create single file to be used for several different LPDB scripts
#
################################################################################

# 1. NA Function

na.finder_f <- function(df){
  print(sapply(X = df, FUN =  function(x) sum(is.na(x))))
}