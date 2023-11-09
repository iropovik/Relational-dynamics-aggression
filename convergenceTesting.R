rm(list = ls())
list.of.packages <- c("tidyverse", "magrittr", "psych", "openxlsx", "RSiena", "metafor", "lavaan", "parallel", "purrr", "haven", "readr", "writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

#Convergence testing settings
classId <- c("0401")

# Define eligible models for each classroom
reducedModel <- as.character(c("0401"))
fullModel <- as.character(c())

# c("0101", "0401", "0402", "0501", "0601", "0602", "0603", "0801", "0901", "1002", "1003", "1101", "1501", "1502", "1703", "1901", "1902")

# Number of iterations and firstg parameter setting
firstgSetting <- .01
iterN2 <- 500 #1000
iterN3 <- 5000 #10000

# Create RSiena algorithm
sienaAlgorithm <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = firstgSetting, n2start = iterN2, n3 = iterN3)
sienaAlgorithmHCP <- sienaAlgorithmCreate(projname = "multiplex_algo", firstg = 0.001, n2start = 1000, n3 = 5000)

# Run the check
source("testingEngine.R")
result
