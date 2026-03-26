## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE  # Set to TRUE if you want to run the examples
)

## ----setup--------------------------------------------------------------------
library(contentanalysis)
library(dplyr)

## ----download-----------------------------------------------------------------
# Download example paper
paper_url <- "https://raw.githubusercontent.com/massimoaria/contentanalysis/master/inst/examples/example_paper.pdf"
download.file(paper_url, destfile = "example_paper.pdf", mode = "wb")

## ----import-basic-------------------------------------------------------------
# Import with automatic section detection
doc <- pdf2txt_auto("example_paper.pdf", n_columns = 2, citation_type = "author_year")

# Check what sections were detected
names(doc)

## ----import-manual, eval=FALSE------------------------------------------------
# # Single column
# doc_single <- pdf2txt_auto("example_paper.pdf", n_columns = 1)
# 
# # Three columns
# doc_three <- pdf2txt_auto("example_paper.pdf", n_columns = 3)
# 
# # Without section splitting
# text_only <- pdf2txt_auto("example_paper.pdf", sections = FALSE)

## ----analysis-----------------------------------------------------------------
analysis <- analyze_scientific_content(
  text = doc,
  doi = "10.1016/j.mlwa.2021.100094",        # Paper's DOI for CrossRef lookup
  mailto = "your@email.com",                 # Required for CrossRef API
  citation_type = "author_year",             # Citation style  
  window_size = 10,                          # Words around citations
  remove_stopwords = TRUE,
  ngram_range = c(1, 3),
  use_sections_for_citations = TRUE
)

## ----results-structure--------------------------------------------------------
names(analysis)

## ----summary------------------------------------------------------------------
analysis$summary

## ----reference-sources--------------------------------------------------------
# View enriched references
head(analysis$parsed_references[, c("ref_first_author", "ref_year", 
                                     "ref_journal", "ref_source")])

# Check data sources
table(analysis$parsed_references$ref_source)

## ----openalex-data------------------------------------------------------------
# Check if OpenAlex data is available
if (!is.null(analysis$references_oa)) {
  # View enriched metadata
  head(analysis$references_oa[, c("title", "publication_year", "cited_by_count", 
                                   "type", "is_oa")])
  
  # Analyze citation impact
  cat("Citation impact statistics:\n")
  print(summary(analysis$references_oa$cited_by_count))
  
  # Open access status
  if ("is_oa" %in% names(analysis$references_oa)) {
    oa_count <- sum(analysis$references_oa$is_oa, na.rm = TRUE)
    cat("\nOpen Access references:", oa_count, "out of", 
        nrow(analysis$references_oa), "\n")
  }
}

## ----matching-quality---------------------------------------------------------
# View matching results with confidence levels
matched <- analysis$citation_references_mapping %>%
  select(citation_text_clean, cite_author, cite_year, 
         ref_authors, ref_year, match_confidence)

head(matched)

# Match quality distribution
cat("Match quality distribution:\n")
print(table(matched$match_confidence))

# High-confidence matches
high_conf <- matched %>%
  filter(match_confidence %in% c("high", "high_second_author"))
cat("\nHigh-confidence matches:", nrow(high_conf), "out of", 
    nrow(matched), "\n")

## ----citations----------------------------------------------------------------
# View all citations
head(analysis$citations)

# Citation types found
table(analysis$citations$citation_type)

# Citations by section
analysis$citation_metrics$section_distribution

## ----citation-types-----------------------------------------------------------
# Narrative vs. parenthetical
analysis$citation_metrics$narrative_ratio

# Citation density
cat("Citation density:", 
    analysis$citation_metrics$density$citations_per_1000_words,
    "citations per 1000 words\n")

## ----contexts-----------------------------------------------------------------
# View citation contexts with matched references
contexts <- analysis$citation_contexts %>%
  select(citation_text_clean, section, ref_full_text, 
         full_context, match_confidence)

head(contexts)

# Find citations in specific section
intro_citations <- analysis$citation_contexts %>%
  filter(section == "Introduction")

cat("Citations in Introduction:", nrow(intro_citations), "\n")

## ----network-create, fig.width=8, fig.height=6--------------------------------
# Create interactive citation network
network <- create_citation_network(
  citation_analysis_results = analysis,
  max_distance = 800,          # Maximum distance in characters
  min_connections = 2,          # Minimum connections to include a node
  show_labels = TRUE            # Show citation labels
)

# Display the network
network

## ----network-stats------------------------------------------------------------
# Get network statistics
stats <- attr(network, "stats")

# Network size
cat("Number of nodes:", stats$n_nodes, "\n")
cat("Number of edges:", stats$n_edges, "\n")
cat("Average distance:", stats$avg_distance, "characters\n")
cat("Maximum distance:", stats$max_distance, "characters\n")

# Distribution by section
print(stats$section_distribution)

# Citations appearing in multiple sections
if (nrow(stats$multi_section_citations) > 0) {
  cat("\nCitations appearing in multiple sections:\n")
  print(stats$multi_section_citations)
}

# Color mapping
cat("\nSection colors:\n")
print(stats$section_colors)

## ----network-custom, eval=FALSE-----------------------------------------------
# # Focus on very close citations only
# network_close <- create_citation_network(
#   analysis,
#   max_distance = 300,
#   min_connections = 1
# )
# 
# # Show only highly connected "hub" citations
# network_hubs <- create_citation_network(
#   analysis,
#   max_distance = 1000,
#   min_connections = 5,
#   show_labels = TRUE
# )
# 
# # Clean visualization without labels
# network_clean <- create_citation_network(
#   analysis,
#   max_distance = 800,
#   min_connections = 2,
#   show_labels = FALSE
# )

## ----network-analysis---------------------------------------------------------
# Find hub citations (most connected)
hub_threshold <- quantile(stats$section_distribution$n, 0.75)
cat("Hub citations (top 25%):\n")
print(stats$section_distribution %>% filter(n >= hub_threshold))

# Analyze network density
network_density <- stats$n_edges / (stats$n_nodes * (stats$n_nodes - 1) / 2)
cat("\nNetwork density:", round(network_density, 3), "\n")

## ----network-data-------------------------------------------------------------
# View raw co-occurrence data
network_data <- analysis$network_data
head(network_data)

# Citations appearing very close together
close_citations <- network_data %>%
  filter(distance < 100)  # Within 100 characters

cat("Number of very close citation pairs:", nrow(close_citations), "\n")

## ----word-freq----------------------------------------------------------------
# Top 20 most frequent words
head(analysis$word_frequencies, 20)

## ----ngrams-------------------------------------------------------------------
# Bigrams
head(analysis$ngrams$`2gram`)

# Trigrams
head(analysis$ngrams$`3gram`)

## ----readability--------------------------------------------------------------
# Calculate readability for the full text
readability <- calculate_readability_indices(
  doc$Full_text,
  detailed = TRUE
)

print(readability)

# Compare readability across sections
sections_to_analyze <- c("Abstract", "Introduction", "Methods", "Discussion")
readability_by_section <- lapply(sections_to_analyze, function(section) {
  if (section %in% names(doc)) {
    calculate_readability_indices(doc[[section]], detailed = FALSE)
  }
})
names(readability_by_section) <- sections_to_analyze

# View results
do.call(rbind, readability_by_section)

## ----word-dist----------------------------------------------------------------
# Terms of interest
terms <- c("random forest", "machine learning", "accuracy", "tree")

# Calculate distribution
dist <- calculate_word_distribution(
  text = doc,
  selected_words = terms,
  use_sections = TRUE
)

# View results
dist %>%
  select(segment_name, word, count, percentage) %>%
  arrange(segment_name, desc(percentage))

## ----plot, fig.width=8, fig.height=5, eval=TRUE-------------------------------
# Interactive plot
plot_word_distribution(
  dist,
  plot_type = "line",
  show_points = TRUE,
  smooth = TRUE
)

# Area plot
plot_word_distribution(
  dist,
  plot_type = "area"
)

## ----find-citations-----------------------------------------------------------
# Citations to specific author
analysis$citation_references_mapping %>%
  filter(grepl("Breiman", ref_authors, ignore.case = TRUE))

# Citations in Discussion section
analysis$citations %>%
  filter(section == "Discussion") %>%
  select(citation_text, citation_type, section)

## ----citation-impact----------------------------------------------------------
if (!is.null(analysis$references_oa)) {
  # Top cited references
  top_cited <- analysis$references_oa %>%
    arrange(desc(cited_by_count)) %>%
    select(title, publication_year, cited_by_count, is_oa) %>%
    head(10)
  
  print(top_cited)
}

## ----custom-stop, eval=FALSE--------------------------------------------------
# custom_stops <- c("however", "therefore", "thus", "moreover")
# 
# analysis_custom <- analyze_scientific_content(
#   text = doc,
#   doi = "10.1016/j.mlwa.2021.100094",
#   mailto = "your@email.com",
#   custom_stopwords = custom_stops,
#   remove_stopwords = TRUE
# )

## ----segments, fig.height=5, fig.width=8, eval=FALSE--------------------------
# # Divide into 20 equal segments
# dist_segments <- calculate_word_distribution(
#   text = doc,
#   selected_words = terms,
#   use_sections = FALSE,
#   n_segments = 20
# )
# 
# plot_word_distribution(dist_segments, smooth = TRUE)

## ----crossref-setup, eval=FALSE-----------------------------------------------
# # Always provide your email for the polite pool
# analysis <- analyze_scientific_content(
#   text = doc,
#   doi = "10.xxxx/xxxxx",
#   mailto = "your@email.com"  # Required for CrossRef polite pool
# )

## ----openalex-setup, eval=FALSE-----------------------------------------------
# # Optional: Set API key for higher rate limits
# # Get free key at: https://openalex.org/
# openalexR::oa_apikey("your-api-key-here")
# 
# # Then run your analysis as usual
# analysis <- analyze_scientific_content(
#   text = doc,
#   doi = "10.xxxx/xxxxx",
#   mailto = "your@email.com"
# )

## ----export, eval=FALSE-------------------------------------------------------
# # Export citations
# write.csv(analysis$citations, "citations.csv", row.names = FALSE)
# 
# # Export matched references with confidence scores
# write.csv(analysis$citation_references_mapping,
#           "matched_citations.csv", row.names = FALSE)
# 
# # Export enriched references
# write.csv(analysis$parsed_references,
#           "enriched_references.csv", row.names = FALSE)
# 
# # Export OpenAlex metadata (if available)
# if (!is.null(analysis$references_oa)) {
#   write.csv(analysis$references_oa,
#             "openalex_metadata.csv", row.names = FALSE)
# }
# 
# # Export word frequencies
# write.csv(analysis$word_frequencies,
#           "word_frequencies.csv", row.names = FALSE)
# 
# # Export network statistics
# if (!is.null(network)) {
#   stats <- attr(network, "stats")
#   write.csv(stats$section_distribution,
#             "network_section_distribution.csv", row.names = FALSE)
#   if (nrow(stats$multi_section_citations) > 0) {
#     write.csv(stats$multi_section_citations,
#               "network_multi_section_citations.csv", row.names = FALSE)
#   }
# }

## ----batch, eval=FALSE--------------------------------------------------------
# # Process multiple papers with API enrichment
# papers <- c("paper1.pdf", "paper2.pdf", "paper3.pdf")
# dois <- c("10.xxxx/1", "10.xxxx/2", "10.xxxx/3")
# 
# results <- list()
# networks <- list()
# 
# for (i in seq_along(papers)) {
#   # Import PDF
#   doc <- pdf2txt_auto(papers[i], n_columns = 2)
# 
#   # Analyze with API enrichment
#   results[[i]] <- analyze_scientific_content(
#     doc,
#     doi = dois[i],
#     mailto = "your@email.com"
#   )
# 
#   # Create network for each paper
#   networks[[i]] <- create_citation_network(
#     results[[i]],
#     max_distance = 800,
#     min_connections = 2
#   )
# }
# 
# # Combine citation counts
# citation_counts <- sapply(results, function(x) x$summary$citations_extracted)
# names(citation_counts) <- papers
# 
# # Compare network statistics
# network_stats <- lapply(networks, function(net) {
#   stats <- attr(net, "stats")
#   c(nodes = stats$n_nodes,
#     edges = stats$n_edges,
#     avg_distance = stats$avg_distance)
# })
# 
# do.call(rbind, network_stats)
# 
# # Analyze reference sources across papers
# ref_sources <- lapply(results, function(x) {
#   if (!is.null(x$parsed_references)) {
#     table(x$parsed_references$ref_source)
#   }
# })
# names(ref_sources) <- papers
# ref_sources

