#' rcaptext: Text Analysis Tools for Capstone Projects
#'
#' A comprehensive R package for text analysis and natural language processing,
#' specifically designed for capstone projects and exploratory data analysis
#' of large text corpora.
#'
#' @section Main Features:
#'
#' \strong{Corpus Management:}
#' \itemize{
#'   \item \code{\link{load_corpus}}: Load SwiftKey text corpora safely
#'   \item \code{\link{sample_corpus}}: Create representative samples
#' }
#'
#' \strong{Text Preprocessing:}
#' \itemize{
#'   \item \code{\link{clean_text}}: Comprehensive text normalization
#'   \item \code{\link{flag_non_english}}: Detect non-English content
#'   \item \code{\link{filter_non_english_unigrams}}: Filter multilingual text
#' }
#'
#' \strong{Tokenization:}
#' \itemize{
#'   \item \code{\link{tokenize_unigrams}}: Individual word tokens
#'   \item \code{\link{tokenize_bigrams}}: Two-word sequences
#'   \item \code{\link{tokenize_trigrams}}: Three-word sequences
#'   \item \code{\link{tokenize_ngrams}}: General n-gram creation
#' }
#'
#' \strong{Frequency Analysis:}
#' \itemize{
#'   \item \code{\link{freq_unigrams}}: Word frequency tables
#'   \item \code{\link{freq_ngrams}}: N-gram frequency analysis
#'   \item \code{\link{coverage_from_freq}}: Cumulative coverage statistics
#' }
#'
#' \strong{Visualization:}
#' \itemize{
#'   \item \code{\link{plot_cumulative_coverage}}: Coverage curves
#'   \item \code{\link{plot_top_terms}}: Most frequent terms
#'   \item \code{\link{plot_rank_frequency}}: Zipf distribution plots
#' }
#'
#' @section Typical Workflow:
#'
#' \preformatted{
#' # 1. Load corpus
#' corpus <- load_corpus("en_US")
#' 
#' # 2. Create sample for development
#' sample_data <- sample_corpus(corpus, prop = 0.05)
#' 
#' # 3. Clean text
#' sample_data$text <- clean_text(sample_data$text)
#' 
#' # 4. Tokenize
#' unigrams <- tokenize_unigrams(sample_data)
#' bigrams <- tokenize_bigrams(sample_data)
#' 
#' # 5. Analyze frequencies
#' freq_uni <- freq_unigrams(unigrams)
#' freq_bi <- freq_bigrams(bigrams)
#' 
#' # 6. Visualize results
#' plot_top_terms(freq_uni)
#' plot_cumulative_coverage(freq_uni)
#' }
#'
#' @section Data Expectations:
#'
#' The package is designed to work with SwiftKey text corpora, but can handle
#' any text data organized in tibbles with \code{source} and \code{text} columns.
#'
#' Expected file structure for \code{\link{load_corpus}}:
#' \itemize{
#'   \item \code{en_US.blogs.txt}
#'   \item \code{en_US.news.txt}
#'   \item \code{en_US.twitter.txt}
#' }
#'
#' @keywords internal
"_PACKAGE"