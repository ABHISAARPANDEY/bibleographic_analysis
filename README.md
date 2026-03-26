# Bibliometric Analysis Pipeline (R)

This project is fully set up to run bibliometric analysis for:

`("financial inclusion" AND "agricultural finance") OR ("farm credit" AND "agricultural productivity")`

It collects records from OpenAlex and CrossRef, optionally merges Dimensions/Scopus/WoS exports, cleans data, builds bibliometrix-ready data, runs analyses, and exports tables/plots.

## Project Structure

- `data/raw` - raw merged data from APIs/imports
- `data/clean` - cleaned and bibliometrix-ready files
- `results/plots` - PNG charts and network/thematic figures
- `results/tables` - CSV summary tables
- `scripts` - pipeline scripts

## Quick Start (Ready To Go)

From project root:

```bash
Rscript scripts/run_pipeline.R
```

The script auto-installs required R packages if missing.

## Optional Environment Variables

Run with custom settings without editing code:

```bash
OPENALEX_EMAIL="you@example.com" \
TARGET_N=1000 \
MIN_YEAR=2000 \
MAX_YEAR=2025 \
Rscript scripts/run_pipeline.R
```

Optional imports:

```bash
DIMENSIONS_CSV="data/raw/dimensions_export.csv" \
SCOPUS_CSV="data/raw/scopus.csv" \
WOS_FILE="data/raw/wos.txt" \
Rscript scripts/run_pipeline.R
```

Primary target databases are now:
- Scopus (via `SCOPUS_CSV`)
- Web of Science (via `WOS_FILE`)
- Dimensions (via `DIMENSIONS_CSV`)

OpenAlex/CrossRef are fallback only (off by default). Enable fallback:

```bash
USE_FALLBACK_APIS=true Rscript scripts/run_pipeline.R
```

To launch Biblioshiny after processing:

```bash
LAUNCH_BIBLIOSHINY=true Rscript scripts/run_pipeline.R
```

## Main Script

- `scripts/bibliometric_pipeline.R`

The script includes:

1. Data collection (OpenAlex + CrossRef + optional Dimensions)
2. Data cleaning (dedupe by DOI/title, author normalization, keyword cleaning, missing-value handling)
3. Conversion to bibliometrix-compatible dataframe
4. Bibliometric analysis (`biblioAnalysis`, `summary`, plots)
5. Network analysis (`biblioNetwork`, `networkPlot`)
6. Thematic/conceptual analysis (`thematicMap`, `conceptualStructure`)
7. Exports (PNG + CSV)
8. Optional dashboard (`biblioshiny()`)
9. Bonus outputs (year filtering, top-20 keywords, citation analysis)

## Interactive Dashboard

Launch interactive dashboard:

```bash
Rscript scripts/dashboard_app.R
```

Dashboard includes:
- year and citation filters
- optional journal-only toggle
- interactive plots (annual trend, authors, journals, keywords)
- interactive data tables for export/copy

## Expected Outputs

- `data/raw/combined_raw.csv`
- `data/clean/combined_clean.csv`
- `data/clean/bibliometrix_ready.csv`
- `results/tables/main_information.csv`
- `results/tables/most_relevant_authors.csv`
- `results/tables/most_relevant_journals.csv`
- `results/tables/most_cited_documents.csv`
- `results/tables/top20_keywords.csv`
- `results/tables/citation_analysis_top_articles.csv`
- `results/plots/*.png` (production, networks, thematic, conceptual)

