#!/usr/bin/env Rscript

library(plumber)
pr("cat_api.R") %>%
  pr_run(port = 5234)