# Bibliometric Analysis Pipeline - Detailed Usage Guide

## 1) What this project is about

This project builds a complete bibliometric workflow in R for the topic:

- **Financial Inclusion**
- **Agricultural Finance**
- **Agricultural Productivity**

The workflow is designed for thesis/research use and supports:

- data collection from APIs and external exports
- data cleaning and de-duplication
- bibliometric conversion for `bibliometrix`
- descriptive, citation, thematic, and network outputs
- publication-style figures and CSV tables
- an interactive dashboard for exploration

Primary intended use:

- **Thesis/report writing** with static high-quality figures
- **Presentation/demo** with interactive dashboard

---

## 2) Project structure

```text
data/
  raw/            # raw collected/imported files
  clean/          # cleaned and analysis-ready datasets
results/
  plots/          # generated PNG figures
  tables/         # generated CSV tables
scripts/
  bibliometric_pipeline.R   # main pipeline
  run_pipeline.R            # launcher script
  dashboard_app.R           # interactive dashboard
README.md
DETAILED_USAGE_GUIDE.md
```

---

## 3) Data source strategy (important)

Current pipeline design:

- **Primary target databases**:
  - Scopus (CSV export)
  - Web of Science (plain text or CSV export)
  - Dimensions (CSV export)
- **Fallback APIs** (optional):
  - OpenAlex
  - CrossRef

If primary files are missing, the pipeline can fallback to APIs (if enabled).

---

## 4) External database setup (Scopus, WoS, Dimensions)

## 4.1 Scopus export

Use Scopus advanced query:

```text
(TITLE-ABS-KEY("financial inclusion") AND TITLE-ABS-KEY("agricultural finance"))
OR
(TITLE-ABS-KEY("farm credit") AND TITLE-ABS-KEY("agricultural productivity"))
```

Recommended filters:

- Years: 2000-2025
- Document type: Article (and Review optional)
- Language: English

Export format and fields:

- Format: CSV
- Include: citation info, bibliographic info, abstract/keywords, references
- Save as: `scopus.csv`

Place file at:

- `data/raw/scopus.csv`

## 4.2 Web of Science export

Suggested query in WoS:

```text
TS=(("financial inclusion" AND "agricultural finance") OR ("farm credit" AND "agricultural productivity"))
```

Recommended filters:

- Timespan: 2000-2025
- Document type: Article (and Review optional)
- Language: English

Export:

- Preferred: Plain text (`.txt`) for `convert2df` compatibility
- Save as: `wos.txt`

Place file at:

- `data/raw/wos.txt`

## 4.3 Dimensions export

Run matching keyword query in Dimensions.

Export:

- CSV
- Include as many metadata columns as available (title, abstract, DOI, year, authors, source, keywords, citations, affiliations)
- Save as: `dimensions_export.csv`

Place file at:

- `data/raw/dimensions_export.csv`

---

## 5) Command to run with external databases only

Use this when you have Scopus/WoS/Dimensions files and do not want fallback APIs:

```bash
USE_EXISTING_RAW=false APPLY_STRICT_FILTER=true \
SCOPUS_CSV="data/raw/scopus.csv" \
WOS_FILE="data/raw/wos.txt" \
DIMENSIONS_CSV="data/raw/dimensions_export.csv" \
USE_FALLBACK_APIS=false \
Rscript scripts/run_pipeline.R
```

---

## 6) Command to run with latest fallback APIs

Use this when external files are missing or incomplete:

```bash
USE_EXISTING_RAW=false APPLY_STRICT_FILTER=true USE_FALLBACK_APIS=true TARGET_N=1500 \
Rscript scripts/run_pipeline.R
```

---

## 7) What each script does

- `scripts/bibliometric_pipeline.R`
  - contains all functions for fetch/clean/analyze/export
- `scripts/run_pipeline.R`
  - lightweight runner with environment variable overrides
- `scripts/dashboard_app.R`
  - interactive dashboard (plots, filters, generated outputs preview)

---

## 8) Data processing flow

1. **Collection**
   - Raw records gathered from selected sources
2. **Cleaning**
   - DOI/title deduplication
   - author normalization
   - keyword normalization
   - missing value handling
3. **Strict relevance filter**
   - keeps records aligned to topic logic
4. **Bibliometric conversion**
   - maps fields into bibliometrix-compatible structure
5. **Analysis**
   - summary, citation, themes, and networks
6. **Export**
   - tables to `results/tables`
   - plots to `results/plots`

---

## 9) Main outputs and how to interpret

Key tables:

- `most_relevant_authors.csv` -> productive authors
- `most_relevant_journals.csv` -> dominant publication sources
- `most_cited_documents.csv` -> influential studies
- `top20_keywords.csv` -> research focus areas
- `annual_scientific_production_table.csv` -> publication trend
- `figure_captions.csv` -> ready figure captions for thesis

Key plots:

- `annual_scientific_production.png`
- `top20_authors_bar.png`
- `top20_journals_bar.png`
- `top20_keywords_bar.png`
- `top20_countries_bar.png`

Optional plots (only if data supports them):

- `network_coauthorship.png`
- `network_keyword_cooccurrence.png`
- `network_country_collaboration.png`
- `thematic_map.png`
- `conceptual_structure.png`
- `thematic_evolution.png`
- `three_fields_plot.png`

---

## 10) Dashboard usage

Launch:

```bash
Rscript scripts/dashboard_app.R
```

Dashboard features:

- year and citation filters
- journal-only toggle
- interactive trend and frequency plots
- document table
- download filtered CSV
- generated outputs tab (image preview + exported table list)

---

## 11) Environment variables reference

- `TARGET_N` -> API target records per source
- `MIN_YEAR` / `MAX_YEAR` -> year filtering
- `OPENALEX_EMAIL` -> optional polite OpenAlex identifier
- `SCOPUS_CSV` -> Scopus export path
- `WOS_FILE` -> WoS export path
- `DIMENSIONS_CSV` -> Dimensions export path
- `USE_EXISTING_RAW` -> reuse `data/raw/combined_raw.csv` (`true/false`)
- `APPLY_STRICT_FILTER` -> strict topical filter (`true/false`)
- `USE_FALLBACK_APIS` -> allow OpenAlex/CrossRef (`true/false`)
- `LAUNCH_BIBLIOSHINY` -> open biblioshiny after run (`true/false`)

---

## 12) Common issues and fixes

## Issue A: Empty `combined_clean.csv` or `bibliometrix_ready.csv`

Cause:

- source files missing or query returned no records

Fix:

- verify files exist in `data/raw/`
- or run with `USE_FALLBACK_APIS=true`

## Issue B: "No records available for bibliometric analysis"

Cause:

- strict filter removed everything

Fix:

- run with `APPLY_STRICT_FILTER=false` temporarily
- inspect records and adjust query/filter logic

## Issue C: Dashboard opens but no images in "Generated Outputs"

Cause:

- pipeline has not been run after cleanup

Fix:

- run pipeline first, then reopen dashboard

## Issue D: Dependency installation error

Cause:

- no write access to default R library or network constraints

Fix:

- project uses local `.Rlib` fallback
- ensure internet access for package install

---

## 13) Recommended workflow for thesis submission

1. Import high-quality Scopus/WoS/Dimensions exports
2. Run pipeline with fallback disabled
3. Review strict cleaned dataset for scope quality
4. Keep publication figures and tables
5. Use `figure_captions.csv` in report drafting
6. Use dashboard for interactive validation and presentation

---

## 14) Reproducibility note

For reproducible results in writing:

- record run date/time
- save exact command used
- mention data source mix (Scopus/WoS/Dimensions vs fallback APIs)
- preserve output folder snapshot with thesis draft

