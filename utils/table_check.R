# Table Checks 
# R
method_tbl |> 
  pull(r_links) |> 
  discard(\(x) str_length(x) == 0) |> 
  length()

list.files("R/") |> 
  length()

# SAS
method_tbl |> 
  pull(sas_links) |> 
  discard(\(x) str_length(x) == 0) |> 
  length()

list.files("SAS/") |> 
  length()

library(tidyverse)
# Python 
method_tbl |> 
  pull(python_links) |> 
  discard(\(x) str_length(x) == 0) |> 
  length()

list.files("python/") |> 
  length()
# Comp
a <- method_tbl |> 
  pull(comparison_links) |> 
  discard(\(x) str_length(x) == 0) |> 
  str_extract("\\/[^\\/]+\\.html") |> 
  str_remove("\\/") |> 
  str_remove("\\.html")

b <- list.files("Comp/") |> 
  str_remove("\\.qmd")

setdiff(b,a)

