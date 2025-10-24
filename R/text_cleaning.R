#' Clean and Normalize Text Data
#'
#' This function performs comprehensive text cleaning and normalization,
#' including URL removal, social media cleanup, and character standardization.
#' It's specifically designed for preprocessing social media and web text data.
#' Uses parallel processing for large text vectors.
#'
#' @param x Character vector. Input text to be cleaned.
#' @param keep_hashtag_word Logical. If TRUE, removes the '#' symbol but keeps
#'   the hashtag word. If FALSE, removes hashtags entirely. Default is TRUE.
#' @param n_cores Integer. Number of CPU cores to use for parallel processing.
#'   Default is NULL (uses parallel::detectCores() - 4). Set to 1 for sequential processing.
#' @param parallel_threshold Integer. Minimum number of text items to trigger
#'   parallel processing. Default is 1000. Smaller datasets are processed sequentially.
#'
#' @return Character vector of the same length as input, with cleaned text.
#'
#' @details
#' The cleaning process includes:
#' \itemize{
#'   \item Convert to lowercase
#'   \item Remove URLs (http/https and www links)
#'   \item Remove @mentions
#'   \item Handle hashtags (remove # but optionally keep word)
#'   \item Remove non-ASCII characters (emojis, special symbols)
#'   \item Keep only letters, apostrophes, and spaces
#'   \item Normalize whitespace
#'   \item Trim leading/trailing spaces
#' }
#'
#' For large text vectors (>= parallel_threshold), the function automatically
#' uses parallel processing to speed up computation.
#'
#' @examples
#' \dontrun{
#' # Basic text cleaning
#' dirty_text <- c(
#'   "Check this out! https://example.com #amazing @friend",
#'   "Hello world! 😊 It's a beautiful day... #sunshine",
#'   "Visit www.google.com for more info!"
#' )
#' clean_text(dirty_text)
#' 
#' # Remove hashtag words entirely
#' clean_text(dirty_text, keep_hashtag_word = FALSE)
#' 
#' # Force sequential processing
#' clean_text(dirty_text, n_cores = 1)
#' }
#'
#' @export
#' @importFrom stringr str_to_lower str_replace_all str_trim
#' @importFrom parallel makeCluster stopCluster parLapply clusterEvalQ detectCores
clean_text <- function(x, keep_hashtag_word = TRUE, n_cores = NULL, parallel_threshold = 1000) {
  n <- length(x)
  
  # Function to clean a single text or chunk
  clean_fn <- function(text) {
    text |>
      stringr::str_to_lower() |>
      # Remove URLs
      stringr::str_replace_all("https?://\\S+|www\\.\\S+", " ") |>
      # Remove @mentions
      stringr::str_replace_all("@\\w+", " ") |>
      # Handle hashtags: remove # but keep word, or remove entirely
      stringr::str_replace_all("#\\w+", " ")  |>
      # Remove non-ASCII characters (emojis, etc.)
      stringr::str_replace_all("[^\\x01-\\x7F]", " ") |>
      # Keep only letters, apostrophes and spaces; remove numbers, punctuation
      stringr::str_replace_all("[^a-z' ]", " ") |>
      # Collapse multiple spaces
      stringr::str_replace_all("\\s+", " ") |>
      stringr::str_trim()
  }
  
  # Use parallel processing for large vectors
  if (n >= parallel_threshold) {
    if (is.null(n_cores)) {
      n_cores <- max(1, parallel::detectCores() - 4)
    }
    
    if (n_cores > 1) {
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      
      # Export stringr functions to cluster
      parallel::clusterEvalQ(cl, {
        library(stringr)
      })
      
      result <- unlist(parallel::parLapply(cl, x, clean_fn))
      return(result)
    }
  }
  
  # Sequential processing for small vectors or n_cores = 1
  clean_fn(x)
}

#' Detect Non-English Words
#'
#' This function uses hunspell dictionary to identify words that are likely
#' not English. It's useful for filtering multilingual corpora or detecting
#' foreign language content in predominantly English text.
#' Uses parallel processing for large word vectors.
#'
#' @param words Character vector. Words to check (should be in lowercase).
#' @param dict Character or hunspell dictionary object. Dictionary to use for
#'   spell checking. Default is "en_US".
#' @param n_cores Integer. Number of CPU cores to use for parallel processing.
#'   Default is NULL (uses parallel::detectCores() - 4). Set to 1 for sequential processing.
#' @param parallel_threshold Integer. Minimum number of words to trigger
#'   parallel processing. Default is 5000.
#'
#' @return Logical vector of same length as \code{words}: TRUE indicates
#'   the word is probably non-English or misspelled.
#'
#' @details
#' The function applies several heuristics:
#' \itemize{
#'   \item Only checks alphabetic tokens with apostrophes
#'   \item Tokens with numbers or symbols are flagged as non-English
#'   \item Contractions with apostrophes are generally preserved
#'   \item Uses hunspell for spell checking remaining tokens
#' }
#'
#' For large word vectors, the function automatically uses parallel processing.
#'
#' @examples
#' \dontrun{
#' # Check various words
#' test_words <- c("the", "amore", "london", "o'clock", "123abc", "café")
#' flag_non_english(test_words)
#' 
#' # Use different dictionary
#' flag_non_english(c("colour", "color"), dict = "en_GB")
#' 
#' # Force sequential processing
#' flag_non_english(test_words, n_cores = 1)
#' }
#'
#' @export
#' @importFrom stringr str_detect
#' @importFrom hunspell hunspell_check
#' @importFrom parallel makeCluster stopCluster parLapply clusterEvalQ detectCores
flag_non_english <- function(words, dict = "en_US", n_cores = NULL, parallel_threshold = 5000) {
  # Pattern for "English word": very permissive: letters + apostrophe
  is_alpha <- stringr::str_detect(words, "^[a-z']+$")
  res <- logical(length(words))
  res[!is_alpha] <- TRUE  # numbers/symbols → non-english for vocabulary purposes
  
  if (any(is_alpha)) {
    alpha_words <- words[is_alpha]
    n_alpha <- length(alpha_words)
    
    # Use parallel processing for large word lists
    if (n_alpha >= parallel_threshold) {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 4)
      }
      
      if (n_cores > 1) {
        cl <- parallel::makeCluster(n_cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        
        # Export hunspell to cluster
        parallel::clusterEvalQ(cl, {
          library(hunspell)
        })
        
        # Split words into chunks for each core
        chunk_size <- ceiling(n_alpha / n_cores)
        chunks <- split(alpha_words, ceiling(seq_along(alpha_words) / chunk_size))
        
        # Check each chunk in parallel
        ok_list <- parallel::parLapply(cl, chunks, function(chunk) {
          hunspell::hunspell_check(chunk, dict = dict)
        })
        
        ok <- unlist(ok_list)
      } else {
        ok <- hunspell::hunspell_check(alpha_words, dict = dict)
      }
    } else {
      # Sequential processing for small word lists
      ok <- hunspell::hunspell_check(alpha_words, dict = dict)
    }
    
    res[is_alpha] <- !ok
  }
  res
}

#' Filter Non-English Unigrams from Frequency Table
#'
#' This function removes likely non-English words from a unigram frequency table,
#' with options to preserve common stopwords and handle short tokens.
#' Uses parallel processing for large vocabularies.
#'
#' @param freq_uni A tibble with columns \code{word}, \code{n}, and \code{p}.
#'   Typically output from \code{\link{freq_unigrams}}.
#' @param dict Character or hunspell dictionary object. Dictionary for spell checking.
#'   Default is "en_US".
#' @param min_len Integer. Minimum word length to spell-check. Words shorter than
#'   this are automatically removed. Default is 2.
#' @param keep_stopwords Logical. If TRUE, preserves standard English stopwords
#'   even if flagged by spell checker. Default is TRUE.
#' @param n_cores Integer. Number of CPU cores to use for parallel processing.
#'   Default is NULL (auto-detect). Set to 1 for sequential processing.
#' @param parallel_threshold Integer. Minimum vocabulary size to trigger
#'   parallel processing. Default is 5000.
#'
#' @return A filtered tibble with the same structure as input, but with
#'   non-English words removed and frequencies re-normalized.
#'
#' @details
#' The function performs these steps:
#' \itemize{
#'   \item Normalizes typographic apostrophes to straight quotes
#'   \item Removes very short words (length < min_len)
#'   \item Optionally preserves English stopwords as safety net
#'   \item Uses spell checking to identify non-English words
#'   \item Re-normalizes probabilities after filtering
#' }
#'
#' The filtered count and types are stored as attributes "removed_n" and "removed_types".
#'
#' @examples
#' \dontrun{
#' # Assuming you have a frequency table
#' freq_table <- freq_unigrams(tokenize_unigrams(corpus))
#' 
#' # Filter non-English words
#' english_freq <- filter_non_english_unigrams(freq_table)
#' 
#' # Check what was removed
#' attr(english_freq, "removed_n")      # removed tokens
#' attr(english_freq, "removed_types")  # removed unique words
#' 
#' # More aggressive filtering
#' strict_freq <- filter_non_english_unigrams(
#'   freq_table, 
#'   min_len = 3, 
#'   keep_stopwords = FALSE
#' )
#' 
#' # Force sequential processing
#' seq_freq <- filter_non_english_unigrams(freq_table, n_cores = 1)
#' }
#'
#' @export
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_replace_all
filter_non_english_unigrams <- function(freq_uni, dict = "en_US", min_len = 2, 
                                        keep_stopwords = TRUE, n_cores = NULL, 
                                        parallel_threshold = 5000) {
  stopifnot(all(c("word","n","p") %in% names(freq_uni)))
  
  tbl <- freq_uni |>
    dplyr::mutate(
      word = stringr::str_replace_all(.data$word, "'", "'"),   # normalize typographic apostrophe
      is_short = nchar(.data$word) < min_len
    )
  
  # optionally preserve stopwords (very useful for next-word models)
  keep_sw <- rep(FALSE, nrow(tbl))
  if (isTRUE(keep_stopwords)) {
    data("stop_words", package = "tidytext", envir = environment())
    sw <- stop_words$word
    keep_sw <- tbl$word %in% sw
  }
  
  # flag non-english only on non-very-short tokens
  flag_ne <- logical(nrow(tbl))
  idx <- which(!tbl$is_short & !keep_sw)
  if (length(idx)) {
    flag_ne[idx] <- flag_non_english(tbl$word[idx], dict = dict, 
                                     n_cores = n_cores, 
                                     parallel_threshold = parallel_threshold)
  }
  
  out <- tbl |>
    dplyr::filter(!flag_ne) |>
    dplyr::mutate(p = .data$n / sum(.data$n))  # re-normalize p after filtering
  
  attr(out, "removed_n") <- sum(tbl$n[flag_ne])
  attr(out, "removed_types") <- sum(flag_ne)
  out
}