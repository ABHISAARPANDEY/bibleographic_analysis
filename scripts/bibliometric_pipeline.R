#!/usr/bin/env Rscript

# ============================================================
# Bibliometric Pipeline (Financial Inclusion + Agricultural Finance/Productivity)
# ============================================================
# This script builds an end-to-end bibliometric workflow:
# 1) Collect data (OpenAlex, CrossRef, optional Dimensions CSV, optional Scopus/WoS CSV)
# 2) Clean and normalize metadata
# 3) Prepare bibliometrix-compatible data
# 4) Run bibliometric analysis
# 5) Build network analysis
# 6) Run thematic and conceptual analysis
# 7) Export plots and tables
# 8) Optionally launch biblioshiny dashboard
#
# Project structure expected:
# /data/raw
# /data/clean
# /results/plots
# /results/tables
# /scripts
# ============================================================

required_packages <- c(
  "httr2", "jsonlite", "dplyr", "stringr", "tidyr", "purrr",
  "readr", "janitor", "lubridate", "stopwords", "bibliometrix", "ggplot2",
  "shiny", "plotly", "DT"
)

install_and_load_packages <- function(pkgs) {
  default_lib <- .libPaths()[1]
  if (file.access(default_lib, mode = 2) != 0) {
    local_lib <- normalizePath(".Rlib", mustWork = FALSE)
    dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(local_lib, .libPaths()))
  }

  installed <- rownames(installed.packages())
  missing <- setdiff(pkgs, installed)
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org", lib = .libPaths()[1])
  }
  suppressPackageStartupMessages(
    invisible(lapply(pkgs, library, character.only = TRUE))
  )
}

install_and_load_packages(required_packages)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ---------------------------
# 0) User configuration
# ---------------------------
config <- list(
  query_openalex = '("financial inclusion" AND "agricultural finance") OR ("farm credit" AND "agricultural productivity")',
  query_crossref = '("financial inclusion" AND "agricultural finance") OR ("farm credit" AND "agricultural productivity")',
  min_year = 2000,
  max_year = 2025,
  target_n_records = 1000,         # recommended between 500 and 2000
  per_page_openalex = 200,         # OpenAlex max per-page usually 200
  rows_crossref = 100,             # CrossRef rows per request
  email_for_openalex = Sys.getenv("OPENALEX_EMAIL", unset = ""), # optional but recommended
  dimensions_csv = NULL,           # e.g., "data/raw/dimensions_export.csv"
  scopus_csv = NULL,               # e.g., "data/raw/scopus.csv"
  wos_txt_or_csv = NULL,           # e.g., "data/raw/wos.txt"
  launch_biblioshiny = FALSE,      # set TRUE to open interactive dashboard
  request_retry_max_tries = 5,
  request_retry_base_seconds = 1,
  apply_strict_relevance_filter = TRUE,
  use_existing_raw_if_available = TRUE,
  target_primary_sources = c("scopus", "wos", "dimensions"),
  use_openalex_crossref_fallback = FALSE
)

# ---------------------------
# Utility helpers
# ---------------------------
safe_lower <- function(x) {
  x <- ifelse(is.na(x), "", x)
  tolower(x)
}

coalesce_chr <- function(...) {
  vals <- list(...)
  out <- vals[[1]]
  for (i in 2:length(vals)) out <- dplyr::coalesce(out, vals[[i]])
  out
}

publication_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

decode_openalex_abstract <- function(inverted_index) {
  if (is.null(inverted_index) || length(inverted_index) == 0) return(NA_character_)
  words <- names(inverted_index)
  positions <- unlist(inverted_index, use.names = FALSE)
  tokens <- rep(NA_character_, max(positions) + 1)
  idx <- 1
  for (w in words) {
    pos <- unlist(inverted_index[[w]], use.names = FALSE)
    tokens[pos + 1] <- w
    idx <- idx + 1
  }
  paste(tokens, collapse = " ")
}

normalize_author <- function(authors_raw) {
  # Converts "Lastname, Firstname; Last2, First2" to title-case and trimmed.
  authors_raw %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_split(";") %>%
    map_chr(function(a_vec) {
      a_vec <- str_trim(a_vec)
      a_vec <- a_vec[a_vec != ""]
      if (length(a_vec) == 0) return(NA_character_)
      a_vec <- str_to_title(a_vec)
      paste(a_vec, collapse = "; ")
    })
}

clean_keywords <- function(keywords_raw) {
  sw <- unique(c(stopwords::stopwords("en"), "agricultural", "finance", "financial", "inclusion"))
  keywords_raw %>%
    coalesce("") %>%
    safe_lower() %>%
    str_replace_all("[^a-z0-9,;\\s-]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_split("[,;]") %>%
    map_chr(function(x) {
      x <- str_trim(x)
      x <- x[x != ""]
      x <- x[!x %in% sw]
      x <- unique(x)
      paste(x, collapse = "; ")
    })
}

extract_country_from_affiliation <- function(affil) {
  affil <- ifelse(is.na(affil), "", affil)
  # Simple heuristic: keep text after last comma as country-like field.
  out <- str_split(affil, ",") %>% map_chr(~str_trim(tail(.x, 1)))
  out[out == ""] <- NA_character_
  out
}

perform_request_with_retry <- function(req, max_tries = 5, base_wait_seconds = 1) {
  attempt <- 1
  repeat {
    out <- tryCatch(req_perform(req), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (attempt >= max_tries) stop(out)
    wait_s <- base_wait_seconds * (2 ^ (attempt - 1))
    message("Request failed (attempt ", attempt, "). Retrying in ", wait_s, "s...")
    Sys.sleep(wait_s)
    attempt <- attempt + 1
  }
}

as_record_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(split(x, seq_len(nrow(x))))
  if (is.atomic(x)) return(list())
  x
}

# ---------------------------
# 1) Data collection
# ---------------------------
fetch_openalex <- function(query, min_year, max_year, target_n, per_page = 200, email = NULL, retry_max_tries = 5, retry_base_seconds = 1) {
  message("Collecting from OpenAlex...")

  base_url <- "https://api.openalex.org/works"
  all_rows <- list()
  cursor <- "*"

  # OpenAlex supports Boolean in search only partially; we use search + date filter.
  # For strict boolean logic, query can be run as broader search and cleaned downstream.
  fetched <- 0
  repeat {
    req <- request(base_url) |>
      req_url_query(
        search = query,
        filter = paste0("from_publication_date:", min_year, "-01-01,to_publication_date:", max_year, "-12-31"),
        per_page = per_page,
        cursor = cursor,
        select = paste(
          c("id", "doi", "display_name", "publication_year", "primary_location",
            "authorships", "abstract_inverted_index", "cited_by_count", "keywords", "concepts"),
          collapse = ","
        )
      )
    if (!is.null(email) && nzchar(email)) {
      req <- req_url_query(req, mailto = email)
    }

    res <- perform_request_with_retry(req, max_tries = retry_max_tries, base_wait_seconds = retry_base_seconds)
    dat <- resp_body_json(res, simplifyVector = FALSE)
    results <- as_record_list(dat$results)
    if (length(results) == 0) break

    chunk <- tibble(
      source_db = "openalex",
      id = map_chr(results, ~ .x$id %||% NA_character_),
      title = map_chr(results, ~ .x$display_name %||% NA_character_),
      authors = map_chr(results, function(x) {
        au <- x$authorships
        if (is.null(au) || length(au) == 0) return(NA_character_)
        names_vec <- map_chr(au, ~ .x$author$display_name %||% NA_character_)
        names_vec <- names_vec[!is.na(names_vec)]
        paste(names_vec, collapse = "; ")
      }),
      year = map_int(results, ~ as.integer(.x$publication_year %||% NA_integer_)),
      journal = map_chr(results, function(x) {
        x$primary_location$source$display_name %||% NA_character_
      }),
      abstract = map_chr(results, ~ decode_openalex_abstract(.x$abstract_inverted_index)),
      keywords = map_chr(results, function(x) {
        kw <- x$keywords
        if (!is.null(kw) && length(kw) > 0) {
          vals <- map_chr(kw, ~ .x$display_name %||% NA_character_)
        } else {
          vals <- map_chr(x$concepts %||% list(), ~ .x$display_name %||% NA_character_)
        }
        vals <- vals[!is.na(vals)]
        paste(unique(vals), collapse = "; ")
      }),
      doi = map_chr(results, ~ str_remove(.x$doi %||% NA_character_, "^https?://(dx\\.)?doi\\.org/")),
      citations = map_dbl(results, ~ as.numeric(.x$cited_by_count %||% NA_real_)),
      affiliation = map_chr(results, function(x) {
        au <- x$authorships
        if (is.null(au) || length(au) == 0) return(NA_character_)
        inst <- map_chr(au, function(a) {
          ins <- a$institutions
          if (is.null(ins) || length(ins) == 0) return(NA_character_)
          ins[[1]]$display_name %||% NA_character_
        })
        paste(unique(inst[!is.na(inst)]), collapse = "; ")
      })
    )

    all_rows[[length(all_rows) + 1]] <- chunk
    fetched <- fetched + nrow(chunk)
    message("OpenAlex fetched: ", fetched)

    next_cursor <- dat$meta$next_cursor %||% NULL
    if (is.null(next_cursor) || is.na(next_cursor) || fetched >= target_n) break
    cursor <- next_cursor
    Sys.sleep(0.2) # polite rate-limiting
  }

  bind_rows(all_rows) %>%
    distinct() %>%
    slice_head(n = target_n)
}

fetch_crossref <- function(query, min_year, max_year, target_n, rows = 100, retry_max_tries = 5, retry_base_seconds = 1) {
  message("Collecting from CrossRef...")

  base_url <- "https://api.crossref.org/works"
  all_rows <- list()
  offset <- 0
  fetched <- 0

  repeat {
    req <- request(base_url) |>
      req_url_query(
        query.bibliographic = query,
        filter = paste0("from-pub-date:", min_year, "-01-01,until-pub-date:", max_year, "-12-31"),
        rows = rows,
        offset = offset
      )

    res <- perform_request_with_retry(req, max_tries = retry_max_tries, base_wait_seconds = retry_base_seconds)
    dat <- resp_body_json(res, simplifyVector = FALSE)
    items <- as_record_list(dat$message$items)
    if (length(items) == 0) break

    chunk <- tibble(
      source_db = "crossref",
      id = map_chr(items, ~ .x$DOI %||% NA_character_),
      title = map_chr(items, function(x) {
        tt <- x$title
        if (is.null(tt) || length(tt) == 0) return(NA_character_)
        tt[[1]]
      }),
      authors = map_chr(items, function(x) {
        au <- x$author
        if (is.null(au) || length(au) == 0) return(NA_character_)
        vals <- map_chr(au, function(a) {
          nm <- paste(a$family %||% "", a$given %||% "")
          str_squish(nm)
        })
        vals <- vals[vals != ""]
        paste(vals, collapse = "; ")
      }),
      year = map_int(items, function(x) {
        y <- x$published$`date-parts`[[1]][1] %||%
          x$issued$`date-parts`[[1]][1] %||%
          NA_integer_
        as.integer(y)
      }),
      journal = map_chr(items, function(x) {
        ct <- x$`container-title`
        if (is.null(ct) || length(ct) == 0) return(NA_character_)
        ct[[1]]
      }),
      abstract = map_chr(items, ~ .x$abstract %||% NA_character_),
      keywords = map_chr(items, function(x) {
        ss <- x$subject
        if (is.null(ss) || length(ss) == 0) return(NA_character_)
        paste(unique(ss), collapse = "; ")
      }),
      doi = map_chr(items, ~ .x$DOI %||% NA_character_),
      citations = map_dbl(items, ~ as.numeric(.x$`is-referenced-by-count` %||% NA_real_)),
      affiliation = map_chr(items, function(x) {
        au <- x$author
        if (is.null(au) || length(au) == 0) return(NA_character_)
        vals <- map_chr(au, function(a) {
          aff <- a$affiliation
          if (is.null(aff) || length(aff) == 0) return(NA_character_)
          nm <- aff[[1]]$name %||% NA_character_
          nm
        })
        paste(unique(vals[!is.na(vals)]), collapse = "; ")
      })
    )

    all_rows[[length(all_rows) + 1]] <- chunk
    fetched <- fetched + nrow(chunk)
    message("CrossRef fetched: ", fetched)

    offset <- offset + rows
    if (fetched >= target_n) break
    Sys.sleep(0.2)
  }

  bind_rows(all_rows) %>%
    distinct() %>%
    slice_head(n = target_n)
}

read_dimensions_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) return(tibble())
  message("Loading Dimensions CSV: ", path)

  raw <- read_csv(path, show_col_types = FALSE) |> clean_names()
  tibble(
    source_db = "dimensions",
    id = coalesce_chr(raw$doi, raw$id, raw$title),
    title = coalesce_chr(raw$title, raw$publication_title),
    authors = coalesce_chr(raw$authors, raw$author),
    year = as.integer(coalesce_chr(raw$year, raw$publication_year)),
    journal = coalesce_chr(raw$source_title, raw$journal, raw$journal_title),
    abstract = raw$abstract,
    keywords = coalesce_chr(raw$keywords, raw$author_keywords),
    doi = raw$doi,
    citations = as.numeric(coalesce_chr(raw$times_cited, raw$citation_count)),
    affiliation = coalesce_chr(raw$research_orgs, raw$affiliations)
  )
}

read_optional_scopus_wos <- function(scopus_csv = NULL, wos_txt_or_csv = NULL) {
  # Imports for additional records and direct bibliometrix conversion.
  scopus_df <- NULL
  wos_df <- NULL

  if (!is.null(scopus_csv) && file.exists(scopus_csv)) {
    message("Converting Scopus file with convert2df...")
    scopus_df <- convert2df(file = scopus_csv, dbsource = "scopus", format = "csv")
  }
  if (!is.null(wos_txt_or_csv) && file.exists(wos_txt_or_csv)) {
    # WoS exports are often plain text.
    message("Converting WoS file with convert2df...")
    wos_df <- convert2df(file = wos_txt_or_csv, dbsource = "wos", format = ifelse(grepl("\\.csv$", wos_txt_or_csv, ignore.case = TRUE), "csv", "plaintext"))
  }

  list(scopus = scopus_df, wos = wos_df)
}

# ---------------------------
# 2) Data cleaning
# ---------------------------
clean_dataset <- function(df, min_year, max_year) {
  if (nrow(df) == 0) return(df)

  df2 <- df %>%
    mutate(
      doi = str_trim(str_to_lower(doi)),
      title = str_squish(title),
      authors = normalize_author(authors),
      year = as.integer(year),
      journal = str_squish(journal),
      abstract = str_squish(str_replace_all(abstract, "<[^>]+>", " ")),
      keywords = clean_keywords(keywords),
      citations = as.numeric(citations),
      affiliation = str_squish(affiliation)
    ) %>%
    filter(!is.na(year), year >= min_year, year <= max_year) %>%
    mutate(
      dedup_key = ifelse(!is.na(doi) & doi != "", doi, safe_lower(title))
    ) %>%
    arrange(desc(citations)) %>%          # keep highest cited among duplicates
    distinct(dedup_key, .keep_all = TRUE) %>%
    select(-dedup_key) %>%
    mutate(
      # Handle missing values with explicit placeholders where useful.
      title = ifelse(is.na(title) | title == "", "untitled record", title),
      authors = ifelse(is.na(authors) | authors == "", "unknown", authors),
      journal = ifelse(is.na(journal) | journal == "", "unknown source", journal),
      abstract = ifelse(is.na(abstract), "", abstract),
      keywords = ifelse(is.na(keywords), "", keywords),
      doi = ifelse(is.na(doi), "", doi),
      citations = ifelse(is.na(citations), 0, citations),
      affiliation = ifelse(is.na(affiliation), "", affiliation)
    )

  df2
}

apply_strict_relevance_filter <- function(df) {
  if (nrow(df) == 0) return(df)

  text_blob <- paste(
    safe_lower(df$title),
    safe_lower(df$abstract),
    safe_lower(df$keywords)
  )

  # Core concept groups
  has_fin_inclusion <- str_detect(text_blob, "financial inclusion|inclusive finance|financial access")
  has_agri_finance <- str_detect(text_blob, "agricultural finance|farm credit|rural credit|agri\\s*credit|agricultural credit")
  has_productivity <- str_detect(text_blob, "agricultural productivity|farm productivity|crop yield|total factor productivity|productivity")
  has_agri_context <- str_detect(text_blob, "agricultur|farm|smallholder|rural")

  # Strict logic:
  # (financial inclusion AND agricultural finance) OR (farm/agri credit AND agricultural productivity)
  pass_logic <- (has_fin_inclusion & has_agri_finance) | (has_agri_finance & has_productivity)
  pass_logic <- pass_logic & has_agri_context

  # Remove noisy non-research records commonly seen in API harvests.
  noisy_title <- str_detect(
    safe_lower(df$title),
    "decision letter|author response|corrigendum|erratum|retraction|editorial|book review|call for papers"
  )

  out <- df %>%
    mutate(pass_logic = pass_logic, noisy_title = noisy_title) %>%
    filter(pass_logic, !noisy_title) %>%
    select(-pass_logic, -noisy_title)

  out
}

# ---------------------------
# 3) Convert to bibliometric format
# ---------------------------
to_bibliometrix_df <- function(clean_df, optional_biblio = list(scopus = NULL, wos = NULL)) {
  # Build WoS-like bibliometrix fields from API records.
  api_biblio <- clean_df %>%
    transmute(
      AU = authors,
      TI = title,
      SR = title,
      SO = journal,
      JI = journal,
      J9 = journal,
      PY = year,
      AB = abstract,
      DE = keywords,           # author keywords
      ID = keywords,           # keywords plus
      DI = doi,
      TC = as.numeric(citations),
      CR = "",
      C1 = affiliation,        # affiliation string
      CU = extract_country_from_affiliation(affiliation),
      DT = "Article",
      DB = source_db
    )

  out <- api_biblio

  # If user imported Scopus/WoS via convert2df, harmonize selected key columns and append.
  harmonize_biblio <- function(x, db_name) {
    if (is.null(x) || nrow(x) == 0) return(NULL)
    needed <- c("AU", "TI", "SR", "SO", "JI", "J9", "PY", "AB", "DE", "ID", "DI", "TC", "CR", "C1", "CU", "DT")
    for (nm in needed) if (!nm %in% names(x)) x[[nm]] <- NA
    x$DB <- db_name
    x[, c(needed, "DB")]
  }

  scopus_part <- harmonize_biblio(optional_biblio$scopus, "scopus")
  wos_part <- harmonize_biblio(optional_biblio$wos, "wos")

  bind_rows(out, scopus_part, wos_part) %>%
    mutate(
      PY = as.integer(PY),
      TC = as.numeric(TC),
      DI = str_to_lower(str_trim(DI))
    ) %>%
    mutate(
      dedup_key = ifelse(!is.na(DI) & DI != "", DI, safe_lower(TI))
    ) %>%
    arrange(desc(TC)) %>%
    distinct(dedup_key, .keep_all = TRUE) %>%
    select(-dedup_key)
}

# ---------------------------
# 4-7) Bibliometric + network + thematic + export
# ---------------------------
run_bibliometric_outputs <- function(M, plots_dir = "results/plots", tables_dir = "results/tables") {
  if (nrow(M) == 0) stop("No records available for bibliometric analysis.")

  # Clean potentially malformed affiliation/country fields before bibliometrix internals.
  M <- M %>%
    mutate(
      C1 = coalesce(C1, ""),
      CU = coalesce(CU, ""),
      C1 = str_replace_all(C1, "\\s+", " "),
      CU = str_replace_all(CU, "\\s+", " "),
      C1 = str_replace_all(C1, ";+", ";"),
      CU = str_replace_all(CU, ";+", ";"),
      C1 = str_replace_all(C1, "^;+|;+$", ""),
      CU = str_replace_all(CU, "^;+|;+$", ""),
      C1 = ifelse(C1 == "", "Unknown", C1),
      CU = ifelse(CU == "", "Unknown", CU)
    )

  message("Running biblioAnalysis...")
  try_biblio <- function(x) biblioAnalysis(x, sep = ";")
  results <- tryCatch(
    try_biblio(M),
    error = function(e1) {
      message("biblioAnalysis retry #1 (blank affiliations): ", conditionMessage(e1))
      M1 <- M %>% mutate(CU = NA_character_, C1 = NA_character_)
      tryCatch(
        try_biblio(M1),
        error = function(e2) {
          message("biblioAnalysis retry #2 (drop non-essential columns): ", conditionMessage(e2))
          M2 <- M1 %>%
            select(any_of(c("AU", "TI", "SR", "SO", "JI", "J9", "PY", "AB", "DE", "ID", "DI", "TC", "CR", "DT", "DB")))
          try_biblio(M2)
        }
      )
    }
  )
  sum_res <- summary(object = results, k = 20, pause = FALSE)

  # Save key summary tables if they exist.
  save_summary_table <- function(obj, name) {
    if (!is.null(obj) && (is.matrix(obj) || is.data.frame(obj))) {
      write_csv(as.data.frame(obj), file.path(tables_dir, paste0(name, ".csv")))
    }
  }

  save_summary_table(sum_res$MainInformation, "main_information")
  save_summary_table(sum_res$MostProdAuthors, "most_relevant_authors")
  save_summary_table(sum_res$MostRelSources, "most_relevant_journals")
  save_summary_table(sum_res$MostCitedPapers, "most_cited_documents")

  figure_captions <- tibble::tribble(
    ~file, ~caption,
    "annual_scientific_production.png", "Annual scientific production trend of the filtered bibliometric corpus.",
    "top20_authors_bar.png", "Top 20 authors by number of documents.",
    "top20_journals_bar.png", "Top 20 journals/sources by document count.",
    "top20_keywords_bar.png", "Top 20 keywords in the filtered corpus.",
    "top20_countries_bar.png", "Top countries by author affiliation mentions.",
    "network_coauthorship.png", "Co-authorship collaboration network.",
    "network_keyword_cooccurrence.png", "Keyword co-occurrence network.",
    "network_country_collaboration.png", "Country collaboration network.",
    "thematic_map.png", "Thematic map of the knowledge structure.",
    "conceptual_structure.png", "Conceptual structure using MCA.",
    "thematic_evolution.png", "Thematic evolution across selected time periods.",
    "three_fields_plot.png", "Three-fields linkage among authors, keywords, and sources."
  )
  write_csv(figure_captions, file.path(tables_dir, "figure_captions.csv"))

  # Additional clean tables for report writing.
  annual_tbl <- M %>%
    filter(!is.na(PY)) %>%
    mutate(PY = as.integer(PY)) %>%
    count(PY, name = "articles", sort = FALSE)
  write_csv(annual_tbl, file.path(tables_dir, "annual_scientific_production_table.csv"))

  top_authors_tbl <- M %>%
    mutate(AU = coalesce(AU, "")) %>%
    separate_rows(AU, sep = ";") %>%
    mutate(AU = str_trim(AU)) %>%
    filter(AU != "", AU != "unknown") %>%
    count(AU, sort = TRUE, name = "articles") %>%
    slice_head(n = 20)
  write_csv(top_authors_tbl, file.path(tables_dir, "top20_authors_frequency.csv"))

  top_journals_tbl <- M %>%
    mutate(SO = coalesce(SO, "unknown source"), SO = str_trim(SO)) %>%
    count(SO, sort = TRUE, name = "articles") %>%
    slice_head(n = 20)
  write_csv(top_journals_tbl, file.path(tables_dir, "top20_journals_frequency.csv"))

  country_tbl <- M %>%
    mutate(CU = coalesce(CU, "")) %>%
    separate_rows(CU, sep = ";") %>%
    mutate(CU = str_trim(CU)) %>%
    filter(CU != "", CU != "Unknown") %>%
    count(CU, sort = TRUE, name = "articles") %>%
    slice_head(n = 20)
  write_csv(country_tbl, file.path(tables_dir, "top20_countries_frequency.csv"))

  # Annual scientific production plot
  png(file.path(plots_dir, "annual_scientific_production.png"), width = 1200, height = 800, res = 120)
  plot(x = results, k = 20, pause = FALSE)
  dev.off()

  # Top 20 keywords frequency table (BONUS)
  if ("DE" %in% names(M)) {
    kw_tbl <- M %>%
      mutate(DE = coalesce(DE, "")) %>%
      separate_rows(DE, sep = ";") %>%
      mutate(DE = str_trim(safe_lower(DE))) %>%
      filter(DE != "") %>%
      count(DE, sort = TRUE) %>%
      slice_head(n = 20)
    write_csv(kw_tbl, file.path(tables_dir, "top20_keywords.csv"))
  }

  # Publication-grade bar charts (teacher-friendly visuals).
  if (nrow(top_authors_tbl) > 0) {
    p_auth <- ggplot(top_authors_tbl, aes(x = reorder(AU, articles), y = articles)) +
      geom_col(fill = "#2C7FB8") +
      coord_flip() +
      labs(title = "Top 20 Authors by Number of Documents", x = "Author", y = "Documents") +
      publication_theme()
    ggsave(file.path(plots_dir, "top20_authors_bar.png"), p_auth, width = 10, height = 7, dpi = 300)
  }

  if (nrow(top_journals_tbl) > 0) {
    p_jrn <- ggplot(top_journals_tbl, aes(x = reorder(SO, articles), y = articles)) +
      geom_col(fill = "#1B9E77") +
      coord_flip() +
      labs(title = "Top 20 Journals/Sources", x = "Journal/Source", y = "Documents") +
      publication_theme()
    ggsave(file.path(plots_dir, "top20_journals_bar.png"), p_jrn, width = 10, height = 7, dpi = 300)
  }

  if (exists("kw_tbl") && nrow(kw_tbl) > 0) {
    p_kw <- ggplot(kw_tbl, aes(x = reorder(DE, n), y = n)) +
      geom_col(fill = "#D95F02") +
      coord_flip() +
      labs(title = "Top 20 Keywords", x = "Keyword", y = "Frequency") +
      publication_theme()
    ggsave(file.path(plots_dir, "top20_keywords_bar.png"), p_kw, width = 10, height = 7, dpi = 300)
  }

  if (nrow(country_tbl) > 0) {
    p_cty <- ggplot(country_tbl, aes(x = reorder(CU, articles), y = articles)) +
      geom_col(fill = "#7570B3") +
      coord_flip() +
      labs(title = "Top Countries (by author affiliation text)", x = "Country", y = "Documents") +
      publication_theme()
    ggsave(file.path(plots_dir, "top20_countries_bar.png"), p_cty, width = 10, height = 7, dpi = 300)
  }

  # Citation analysis (BONUS)
  if ("CR" %in% names(M) && is.character(M$CR)) {
    cites <- citations(M, field = "article", sep = ";")
    if (!is.null(cites$Cited[1])) {
      write_csv(as.data.frame(cites$Cited), file.path(tables_dir, "citation_analysis_top_articles.csv"))
    }
  }

  # 5) Network analysis
  safe_plot_step <- function(expr, step_name) {
    tryCatch(expr, error = function(e) message(step_name, " skipped: ", conditionMessage(e)))
  }

  message("Building co-authorship network...")
  safe_plot_step({
    M_auth <- M %>% mutate(AU = ifelse(is.na(AU) | AU %in% c("", "unknown"), "", AU)) %>% filter(AU != "")
    if (nrow(M_auth) > 2) {
      net_auth <- biblioNetwork(M_auth, analysis = "collaboration", network = "authors", sep = ";")
      png(file.path(plots_dir, "network_coauthorship.png"), width = 1400, height = 1000, res = 130)
      networkPlot(net_auth, n = min(50, nrow(M_auth)), type = "fruchterman", size = TRUE, remove.isolates = TRUE, labelsize = 0.8, title = "Co-authorship network")
      dev.off()
    } else {
      message("Co-authorship network skipped: insufficient author data.")
    }
  }, "Co-authorship network")

  message("Building keyword co-occurrence network...")
  safe_plot_step({
    M_kw <- M %>% mutate(ID = ifelse(is.na(ID), "", ID)) %>% filter(ID != "")
    if (nrow(M_kw) > 2) {
      net_kw <- biblioNetwork(M_kw, analysis = "co-occurrences", network = "keywords", sep = ";")
      png(file.path(plots_dir, "network_keyword_cooccurrence.png"), width = 1400, height = 1000, res = 130)
      networkPlot(net_kw, n = min(60, nrow(M_kw)), type = "fruchterman", size = TRUE, remove.isolates = TRUE, labelsize = 0.8, title = "Keyword co-occurrence")
      dev.off()
    } else {
      message("Keyword network skipped: insufficient keyword data.")
    }
  }, "Keyword network")

  message("Building country collaboration network...")
  safe_plot_step({
    M_country <- M %>% mutate(CU = ifelse(is.na(CU), "", CU)) %>% filter(CU != "")
    if (nrow(M_country) > 2) {
      net_country <- biblioNetwork(M_country, analysis = "collaboration", network = "countries", sep = ";")
      png(file.path(plots_dir, "network_country_collaboration.png"), width = 1400, height = 1000, res = 130)
      networkPlot(net_country, n = min(40, nrow(M_country)), type = "fruchterman", size = TRUE, remove.isolates = TRUE, labelsize = 0.9, title = "Country collaboration")
      dev.off()
    } else {
      message("Country network skipped: insufficient country data.")
    }
  }, "Country network")

  # 6) Thematic and conceptual analysis
  message("Running thematic map...")
  safe_plot_step({
    png(file.path(plots_dir, "thematic_map.png"), width = 1400, height = 1000, res = 130)
    thematicMap(M, field = "ID", n = 250, minfreq = 5, stemming = FALSE, size = 0.7, n.labels = 8, repel = TRUE)
    dev.off()
  }, "Thematic map")

  message("Running conceptual structure...")
  safe_plot_step({
    png(file.path(plots_dir, "conceptual_structure.png"), width = 1400, height = 1000, res = 130)
    conceptualStructure(M, field = "ID", method = "MCA", minDegree = 4, k.max = 6, stemming = FALSE, labelsize = 10, documents = 8)
    dev.off()
  }, "Conceptual structure")

  # Thematic evolution across time slices (publication-quality trend figure).
  safe_plot_step({
    years <- suppressWarnings(sort(unique(as.integer(M$PY[!is.na(M$PY)]))))
    if (length(years) >= 6) {
      brks <- unique(c(min(years), floor(quantile(years, probs = c(0.33, 0.66))), max(years)))
      brks <- sort(unique(as.integer(brks)))
      if (length(brks) >= 3) {
        png(file.path(plots_dir, "thematic_evolution.png"), width = 1600, height = 1000, res = 140)
        thematicEvolution(M, field = "ID", years = brks, n = 250, minfreq = 3, stemming = FALSE, size = 0.4, repel = TRUE)
        dev.off()
      }
    }
  }, "Thematic evolution")

  # Save a strategic-themes helper table using thematic frequencies by period.
  safe_plot_step({
    theme_table <- M %>%
      mutate(PY = as.integer(PY), ID = coalesce(ID, "")) %>%
      filter(!is.na(PY), ID != "") %>%
      separate_rows(ID, sep = ";") %>%
      mutate(ID = str_trim(safe_lower(ID))) %>%
      filter(ID != "") %>%
      mutate(period = case_when(
        PY <= 2010 ~ "2000-2010",
        PY <= 2017 ~ "2011-2017",
        TRUE ~ "2018-2025"
      )) %>%
      count(period, ID, sort = TRUE, name = "frequency") %>%
      group_by(period) %>%
      slice_head(n = 20) %>%
      ungroup()
    write_csv(theme_table, file.path(tables_dir, "thematic_evolution_top_terms.csv"))
  }, "Thematic evolution table")

  # Three-fields Sankey-style plot (Authors-Keywords-Sources).
  safe_plot_step({
    png(file.path(plots_dir, "three_fields_plot.png"), width = 1600, height = 1000, res = 140)
    threeFieldsPlot(M, fields = c("AU", "DE", "SO"), n = c(20, 20, 20), fontsize = 10)
    dev.off()
  }, "Three-fields plot")

  invisible(list(results = results, summary = sum_res))
}

# ---------------------------
# Main pipeline runner
# ---------------------------
run_pipeline <- function(config) {
  scopus_exists <- !is.null(config$scopus_csv) && file.exists(config$scopus_csv)
  wos_exists <- !is.null(config$wos_txt_or_csv) && file.exists(config$wos_txt_or_csv)
  dimensions_exists <- !is.null(config$dimensions_csv) && file.exists(config$dimensions_csv)
  any_primary_file <- scopus_exists || wos_exists || dimensions_exists

  if (!any_primary_file && !isTRUE(config$use_openalex_crossref_fallback)) {
    warning("Primary files not found (Scopus/WoS/Dimensions). Enabling OpenAlex/CrossRef fallback automatically.")
    config$use_openalex_crossref_fallback <- TRUE
  }

  # Step 1: Collect (or reuse existing raw file)
  raw_path <- "data/raw/combined_raw.csv"
  if (isTRUE(config$use_existing_raw_if_available) && file.exists(raw_path)) {
    message("Using existing raw dataset at ", raw_path)
    combined_raw <- read_csv(raw_path, show_col_types = FALSE)
  } else {
    # Primary target databases: Scopus, WoS, Dimensions
    # (Scopus/WoS are imported via convert2df later; here we keep Dimensions in raw API-like schema.)
    dimensions_raw <- read_dimensions_csv(config$dimensions_csv)
    combined_raw <- dimensions_raw

    # Optional fallback to OpenAlex/CrossRef only when requested.
    if (isTRUE(config$use_openalex_crossref_fallback)) {
      openalex_raw <- fetch_openalex(
        query = config$query_openalex,
        min_year = config$min_year,
        max_year = config$max_year,
        target_n = config$target_n_records,
        per_page = config$per_page_openalex,
        email = config$email_for_openalex,
        retry_max_tries = config$request_retry_max_tries,
        retry_base_seconds = config$request_retry_base_seconds
      )

      crossref_raw <- fetch_crossref(
        query = config$query_crossref,
        min_year = config$min_year,
        max_year = config$max_year,
        target_n = config$target_n_records,
        rows = config$rows_crossref,
        retry_max_tries = config$request_retry_max_tries,
        retry_base_seconds = config$request_retry_base_seconds
      )
      combined_raw <- bind_rows(combined_raw, openalex_raw, crossref_raw)
    }

    if (nrow(combined_raw) == 0) {
      warning("No raw rows found from Dimensions/fallback APIs. Will rely on Scopus/WoS imports only.")
      combined_raw <- tibble(
        source_db = character(), id = character(), title = character(), authors = character(),
        year = integer(), journal = character(), abstract = character(), keywords = character(),
        doi = character(), citations = numeric(), affiliation = character()
      )
    } else {
      write_csv(combined_raw, raw_path)
    }
  }

  # Step 2: Clean
  clean_df <- clean_dataset(combined_raw, config$min_year, config$max_year)
  if (nrow(clean_df) > 0) {
    write_csv(clean_df, "data/clean/combined_clean.csv")
    if (isTRUE(config$apply_strict_relevance_filter)) {
      clean_df <- apply_strict_relevance_filter(clean_df)
      if (nrow(clean_df) > 0) {
        write_csv(clean_df, "data/clean/combined_clean_strict.csv")
      }
      message("Strict relevance filter applied. Remaining records: ", nrow(clean_df))
    }
  }

  # Optional Step (Scopus/WoS)
  optional_imports <- read_optional_scopus_wos(
    scopus_csv = config$scopus_csv,
    wos_txt_or_csv = config$wos_txt_or_csv
  )

  # Step 3: Bibliometric format
  M <- to_bibliometrix_df(clean_df, optional_imports)
  if (nrow(M) == 0) {
    stop("No records found. Add at least one valid file among SCOPUS_CSV/WOS_FILE/DIMENSIONS_CSV or enable USE_FALLBACK_APIS=true.")
  }

  # If primary target is Scopus/WoS/Dimensions, prefer those rows in final DB when available.
  if (length(config$target_primary_sources) > 0) {
    primary <- tolower(config$target_primary_sources)
    M_primary <- M %>% filter(tolower(DB) %in% primary)
    if (nrow(M_primary) > 0) M <- M_primary
  }

  write_csv(M, "data/clean/bibliometrix_ready.csv")

  # Steps 4-7: Analysis + export
  out <- run_bibliometric_outputs(M, plots_dir = "results/plots", tables_dir = "results/tables")

  # Step 8: Biblioshiny
  if (isTRUE(config$launch_biblioshiny)) {
    message("Launching biblioshiny...")
    biblioshiny()
  }

  message("Pipeline completed successfully.")
  out
}

# ---------------------------
# Execute
# ---------------------------
if (sys.nframe() == 0) {
  # Create folders if script is run standalone.
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
  dir.create("results/plots", recursive = TRUE, showWarnings = FALSE)
  dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

  # Optional environment overrides for easy no-edit runs:
  # TARGET_N=800 MIN_YEAR=2005 MAX_YEAR=2025 OPENALEX_EMAIL=you@domain.com Rscript scripts/bibliometric_pipeline.R
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
}
