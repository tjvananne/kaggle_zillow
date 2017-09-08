

# GBL_config
print("loading GBL_zil_config file")

library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
library(xgboost)
library(dummies)


GBL_PATH_TO_DATA <- '../input'
GBL_PATH_TO_CACHE <- '../cache'