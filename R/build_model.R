#' Build Complete N-gram Language Model from Training Corpus
#'
#' End-to-end pipeline that takes a training corpus and produces a complete,
#' pruned n-gram language model ready for text prediction. Handles text cleaning,
#' tokenization, frequency computation, optional foreign word filtering, and
#' model pruning with automatic persistence to disk.
#'
#' @param train_corpus Tibble. Training data with at least \code{source} column
#'   and a text column (cleaned or raw).
#' @param text_col Character. Name of the cleaned text column (default: "text_clean").
#' @param ensure_clean Logical. If TRUE and \code{text_col} is missing, creates it
#'   from \code{text} column using \code{\link{clean_text}} (default: TRUE).
#' @param min_chars Integer. Drop rows with fewer than this many characters in
#'   \code{text_col} before tokenization (default: 1).
#' @param sample_prop Numeric or NULL. If numeric (e.g., 0.1), samples this proportion
#'   of rows within each source for RAM control. If NULL, uses all data (default: NULL).
#' @param seed Integer. Random seed for reproducibility, used in sampling (default: 123).
#' @param oov_filter Logical. If TRUE, applies \code{\link{filter_non_english_unigrams}}
#'   to remove likely foreign/misspelled words (default: TRUE).
#' @param dict Character. Hunspell dictionary for OOV filtering (default: "en_US").
#'   Only used if \code{oov_filter=TRUE}.
#' @param min_len_oov Integer. Minimum word length when filtering non-English words
#'   (default: 2). Shorter words are automatically removed.
#' @param keep_stopwords Logical. Keep English stopwords when filtering non-English
#'   (default: TRUE). Recommended TRUE for next-word prediction models.
#' @param min_count_bi Integer. Pruning threshold for bigrams - removes bigrams with
#'   count < min_count_bi (default: 2).
#' @param min_count_tri Integer. Pruning threshold for trigrams - removes trigrams with
#'   count < min_count_tri (default: 2).
#' @param topN_bi Integer. Keep only top-N most probable bigram continuations per
#'   history word w1 (default: 12).
#' @param topN_tri Integer. Keep only top-N most probable trigram continuations per
#'   history pair (w1, w2) (default: 8).
#' @param out_dir Character. Directory where RDS files will be written (default: "data/processed").
#'   Created automatically if missing.
#' @param save Logical. If TRUE, writes RDS files: \code{uni_lookup.rds},
#'   \code{bi_pruned.rds}, \code{tri_pruned.rds}, \code{lang_meta.rds} (default: TRUE).
#' @param return_freq Logical. If TRUE, includes raw frequency tables in the returned
#'   list (can be large). Useful for debugging (default: FALSE).
#'
#' @return A list containing the complete language model:
#'   \describe{
#'     \item{uni_lookup}{Tibble. Unigram lookup table (word, n, p)}
#'     \item{bi_pruned}{Tibble. Pruned bigram model (w1, w2, n, p_cond)}
#'     \item{tri_pruned}{Tibble. Pruned trigram model (w1, w2, w3, n, p_cond)}
#'     \item{meta}{List. Metadata (timestamp, parameters, sizes before/after pruning)}
#'     \item{freq_uni, freq_bi, freq_tri}{(Optional) Raw frequency tables if return_freq=TRUE}
#'   }
#'
#' @details
#' The pipeline executes these steps:
#' \enumerate{
#'   \item **Text Preparation**: Ensures cleaned text exists, filters short/empty lines
#'   \item **Optional Sampling**: Reduces corpus size for RAM control (stratified by source)
#'   \item **Tokenization**: Creates unigrams, bigrams, and trigrams using tidytext
#'   \item **Frequency Computation**: Counts n-gram occurrences and computes probabilities
#'   \item **OOV Filtering** (optional): Removes likely non-English words using hunspell
#'   \item **Model Building**: Computes conditional probabilities P(w2|w1) and P(w3|w1,w2)
#'   \item **Pruning**: Removes rare n-grams and keeps only top-N per history
#'   \item **Persistence**: Saves pruned tables to RDS for fast loading
#' }
#'
#' Memory optimization tips:
#' - Use \code{sample_prop} to work with a fraction of data
#' - Increase \code{min_count_bi} and \code{min_count_tri} to prune more aggressively
#' - Decrease \code{topN_bi} and \code{topN_tri} for smaller models
#' - Set \code{return_freq=FALSE} to avoid keeping large frequency tables in memory
#'
#' @examples
#' \dontrun{
#' # Basic usage with full corpus
#' corpus <- load_corpus("en_US")
#' splits <- split_corpus(corpus, prop_test = 0.1)
#'
#' model <- build_model(
#'   train_corpus = splits$train,
#'   text_col = "text_clean",
#'   out_dir = "models/my_model"
#' )
#'
#' # Memory-efficient: sample 10% + aggressive pruning
#' model_small <- build_model(
#'   train_corpus = splits$train,
#'   sample_prop = 0.1,
#'   min_count_bi = 5,
#'   min_count_tri = 3,
#'   topN_bi = 8,
#'   topN_tri = 5
#' )
#'
#' # Without OOV filtering (faster but includes foreign words)
#' model_fast <- build_model(
#'   train_corpus = splits$train,
#'   oov_filter = FALSE
#' )
#'
#' # Check model statistics
#' print(model$meta$sizes)
#' }
#'
#' @seealso
#' \code{\link{split_corpus}} for creating train/test splits,
#' \code{\link{build_pruned_lang_model}} for the underlying model builder,
#' \code{\link{predict_next}} for using the model
#'
#' @export
#' @importFrom dplyr filter mutate group_by slice_sample ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang := sym
build_model <- function(train_corpus,
                        text_col = "text_clean",
                        ensure_clean = TRUE,
                        min_chars = 1,
                        sample_prop = NULL,
                        seed = 123,
                        oov_filter = TRUE,
                        dict = "en_US",
                        min_len_oov = 2,
                        keep_stopwords = TRUE,
                        min_count_bi = 2,
                        min_count_tri = 2,
                        topN_bi = 12,
                        topN_tri = 8,
                        out_dir = "data/processed",
                        save = TRUE,
                        return_freq = FALSE) {

  # ---- Basic checks ----
  if (!"source" %in% names(train_corpus)) {
    stop("`train_corpus` must have a `source` column (blogs/news/twitter).")
  }
  
  if (!text_col %in% names(train_corpus)) {
    if (isTRUE(ensure_clean) && "text" %in% names(train_corpus)) {
      message(">> `", text_col, "` not found. Creating it from `text` via clean_text()...")
      train_corpus <- train_corpus %>%
        dplyr::mutate(!!rlang::sym(text_col) := clean_text(.data[["text"]]))
    } else {
      stop("Column `", text_col, "` not found and `ensure_clean=FALSE`. ",
           "Provide a cleaned text column or set ensure_clean=TRUE.")
    }
  }

  # ---- Drop short/empty lines ----
  message(">> Filtering short/empty lines (min_chars = ", min_chars, ")")
  train_corpus <- train_corpus %>%
    dplyr::filter(!is.na(.data[[text_col]]), nchar(.data[[text_col]]) >= min_chars)

  # ---- Optional sampling for RAM control (stratified by source) ----
  if (!is.null(sample_prop)) {
    if (sample_prop <= 0 || sample_prop > 1) {
      stop("`sample_prop` must be in (0,1].")
    }
    set.seed(seed)
    message(">> Sampling TRAIN by source (prop = ", sample_prop, ", seed = ", seed, ")")
    train_corpus <- train_corpus %>%
      dplyr::group_by(.data$source) %>%
      dplyr::slice_sample(prop = sample_prop) %>%
      dplyr::ungroup()
  }

  # ---- Tokenize ----
  message(">> Tokenizing (UNI/BI/TRI)")
  uni <- tokenize_unigrams(train_corpus, text_col = !!rlang::sym(text_col))
  bi  <- tokenize_bigrams(train_corpus,  text_col = !!rlang::sym(text_col))
  tri <- tokenize_trigrams(train_corpus, text_col = !!rlang::sym(text_col))

  # ---- Frequencies ----
  message(">> Computing frequency tables")
  freq_uni <- freq_unigrams(uni)   # word, n, p
  freq_bi  <- freq_bigrams(bi)     # w1, w2, n, p
  freq_tri <- freq_trigrams(tri)   # w1, w2, w3, n, p

  # ---- Optional OOV/foreign filter on unigrams ----
  if (isTRUE(oov_filter)) {
    message(">> Filtering likely non-English unigrams (hunspell dict = ", dict, ")")
    freq_uni <- filter_non_english_unigrams(
      freq_uni, 
      dict = dict,
      min_len = min_len_oov,
      keep_stopwords = keep_stopwords
    )
  } else {
    message(">> Skipping OOV/foreign filter on unigrams")
  }

  # ---- Build pruned language model + save ----
  message(">> Building pruned language model (min_count_bi=", min_count_bi,
          ", min_count_tri=", min_count_tri, 
          ", topN_bi=", topN_bi, 
          ", topN_tri=", topN_tri, ")")
  
  lang_model <- build_pruned_lang_model(
    freq_uni = freq_uni,
    freq_bi  = freq_bi,
    freq_tri = freq_tri,
    min_count_bi  = min_count_bi,
    min_count_tri = min_count_tri,
    topN_bi  = topN_bi,
    topN_tri = topN_tri,
    save     = save,
    out_dir  = out_dir
  )

  message(">> Model built successfully! Sizes (before/after pruning):")
  print(lang_model$meta$sizes)
  
  if (isTRUE(save)) {
    message(">> RDS files saved to: ", out_dir)
  }

  # ---- Return ----
  if (isTRUE(return_freq)) {
    lang_model$freq_uni <- freq_uni
    lang_model$freq_bi  <- freq_bi
    lang_model$freq_tri <- freq_tri
  }
  
  lang_model
}
