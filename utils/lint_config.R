# Create configuration file for lintr
# Source this file in package root directory

# List here files to exclude from lint checking, as a character vector
excluded_files <- c(
  list.files("common", recursive = TRUE, full.names = TRUE),
  list.files("data-gaps", recursive = TRUE, full.names = TRUE),
  list.files("docs", recursive = TRUE, full.names = TRUE),
  list.files("dwr-files", recursive = TRUE, full.names = TRUE),
  list.files("explore", recursive = TRUE, full.names = TRUE),
  list.files("gis", recursive = TRUE, full.names = TRUE),
  list.files("output", recursive = TRUE, full.names = TRUE),
  list.files("reports", recursive = TRUE, full.names = TRUE),
  list.files("supply-data", recursive = TRUE, full.names = TRUE),
  list.files("wruds_downloads", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE)
)

### Do not edit after this line ###

library(magrittr)
library(dplyr)

# Make sure we start fresh
if (file.exists(".lintr")) { file.remove(".lintr") }

# List current lints
lintr::lint_dir() %>%
  as.data.frame %>%
  group_by(linter) %>%
  tally(sort = TRUE) %$%
  sprintf("linters: with_defaults(\n    %s\n    dummy_linter = NULL\n  )\n",
          paste0(linter, " = NULL, # ", n, collapse = "\n    ")) %>%
  cat(file = ".lintr")

sprintf("exclusions: list(\n    %s\n  )\n",
        paste0('"', excluded_files, '"', collapse = ",\n    ")) %>%
  cat(file = ".lintr", append = TRUE)

# Clean up workspace
remove(excluded_files)




