## library() calls go here
library(targets)
library(tarchetypes)

# conflicts and other options
library(conflicted)
conflict_prefer("filter", "dplyr", quiet = TRUE)
options(usethis.quiet = TRUE)

# packages for this analysis
suppressPackageStartupMessages({
  library(tidyverse)
  library(epiextractr)
  library(slider)
  library(haven)
})