#!/usr/bin/env Rscript

# Thin launcher for the bibliometric pipeline.
# Usage:
#   Rscript scripts/run_pipeline.R

source("scripts/bibliometric_pipeline.R")

# Apply same environment overrides supported by the main script.
if (nzchar(Sys.getenv("TARGET_N", ""))) config$target_n_records <- as.integer(Sys.getenv("TARGET_N"))
if (nzchar(Sys.getenv("MIN_YEAR", ""))) config$min_year <- as.integer(Sys.getenv("MIN_YEAR"))
if (nzchar(Sys.getenv("MAX_YEAR", ""))) config$max_year <- as.integer(Sys.getenv("MAX_YEAR"))
if (nzchar(Sys.getenv("DIMENSIONS_CSV", ""))) config$dimensions_csv <- Sys.getenv("DIMENSIONS_CSV")
if (nzchar(Sys.getenv("SCOPUS_CSV", ""))) config$scopus_csv <- Sys.getenv("SCOPUS_CSV")
if (nzchar(Sys.getenv("WOS_FILE", ""))) config$wos_txt_or_csv <- Sys.getenv("WOS_FILE")
if (nzchar(Sys.getenv("LAUNCH_BIBLIOSHINY", ""))) {
  config$launch_biblioshiny <- tolower(Sys.getenv("LAUNCH_BIBLIOSHINY")) %in% c("1", "true", "yes", "y")
}
if (nzchar(Sys.getenv("APPLY_STRICT_FILTER", ""))) {
  config$apply_strict_relevance_filter <- tolower(Sys.getenv("APPLY_STRICT_FILTER")) %in% c("1", "true", "yes", "y")
}
if (nzchar(Sys.getenv("USE_EXISTING_RAW", ""))) {
  config$use_existing_raw_if_available <- tolower(Sys.getenv("USE_EXISTING_RAW")) %in% c("1", "true", "yes", "y")
}
if (nzchar(Sys.getenv("USE_FALLBACK_APIS", ""))) {
  config$use_openalex_crossref_fallback <- tolower(Sys.getenv("USE_FALLBACK_APIS")) %in% c("1", "true", "yes", "y")
}

run_pipeline(config)

