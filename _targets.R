## Load your packages, e.g. library(targets).
source("packages.R")

## Globals
data_version <- "2023 November 6"

## Functions
lapply(list.files("R", full.names = TRUE), source)

tar_plan(
  tar_format_feather(org_microdata, create_org_microdata(data_version)),
  historical_counts = create_counts(org_microdata),
  latest_counts = pull_latest_counts(historical_counts),
  tar_file(latest_counts_csv, target_to_csv(latest_counts))
)