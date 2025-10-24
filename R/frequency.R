#' Compute Frequency Table for Unigrams
#'
#' This function creates a frequency table from tokenized unigrams, showing
#' both absolute counts and relative frequencies for each unique word.
#' Uses parallel processing for large datasets to improve performance.
#'
#' @param unigrams A tibble with a column named "word", typically from
#'   \code{\link{tokenize_unigrams}}.
#' @param n_cores Integer. Number of CPU cores for parallel processing.
#'   Default is NULL (uses parallel::detectCores() - 6). Set to 1 for sequential processing.
#' @param parallel_threshold Integer. Minimum number of rows to trigger parallel processing.
#'   Default is 100000. Smaller datasets are processed sequentially.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{word}{Character. The unique word}
#'     \item{n}{Integer. Count of occurrences}
#'     \item{p}{Numeric. Relative frequency (proportion)}
#'   }
#'   Sorted by count in descending order.
#'
#' @details
#' For large datasets (>= parallel_threshold), the function splits data into chunks,
#' computes partial counts in parallel, then aggregates results. This significantly
#' speeds up frequency computation on million-row datasets.
#'
#' @examples
#' \dontrun{
#' corpus <- load_corpus("en_US")
#' unigrams <- tokenize_unigrams(corpus)
#' freq_table <- freq_unigrams(unigrams)
#' head(freq_table)
#' 
#' # Check most common words
#' head(freq_table, 10)
#' 
#' # Force sequential processing
#' freq_seq <- freq_unigrams(unigrams, n_cores = 1)
#' }
#'
#' @export
#' @importFrom dplyr count mutate bind_rows
#' @importFrom parallel makeCluster stopCluster parLapply clusterEvalQ detectCores
freq_unigrams <- function(unigrams, n_cores = NULL, parallel_threshold = 100000) {
  n_rows <- nrow(unigrams)
  
  # Sequential processing for small datasets
  if (n_rows < parallel_threshold) {
    return(
      unigrams |>
        dplyr::count(.data$word, sort = TRUE, name = "n") |>
        dplyr::mutate(p = .data$n / sum(.data$n))
    )
  }
  
  # Parallel processing for large datasets
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 6)
  }
  
  if (n_cores == 1) {
    return(
      unigrams |>
        dplyr::count(.data$word, sort = TRUE, name = "n") |>
        dplyr::mutate(p = .data$n / sum(.data$n))
    )
  }
  
  # Setup parallel cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit({
    parallel::stopCluster(cl)
    gc()
  })
  
  # Export required packages
  parallel::clusterEvalQ(cl, {
    library(dplyr)
  })
  
  # Split data into chunks
  chunk_size <- ceiling(n_rows / n_cores)
  chunks <- split(unigrams, ceiling(seq_len(n_rows) / chunk_size))
  
  # Compute partial counts in parallel
  partial_counts <- parallel::parLapply(cl, chunks, function(chunk) {
    chunk |> dplyr::count(word, name = "n")
  })
  
  # Aggregate results
  result <- dplyr::bind_rows(partial_counts) |>
    dplyr::count(.data$word, wt = .data$n, sort = TRUE, name = "n") |>
    dplyr::mutate(p = .data$n / sum(.data$n))
  
  result
}

#' Compute Frequency Table for N-grams
#'
#' This function creates frequency tables from n-gram data, splitting each
#' n-gram into separate word components and computing frequencies.
#' Uses parallel processing for large datasets to improve performance.
#'
#' @param ngrams_tbl A tibble with an "ng" column containing n-grams,
#'   typically from \code{\link{tokenize_ngrams}}.
#' @param n Integer. Length of the n-gram (2 for bigrams, 3 for trigrams).
#'   Must be >= 2.
#' @param n_cores Integer. Number of CPU cores for parallel processing.
#'   Default is NULL (uses parallel::detectCores() - 6). Set to 1 for sequential processing.
#' @param parallel_threshold Integer. Minimum number of rows to trigger parallel processing.
#'   Default is 50000. Smaller datasets are processed sequentially.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{w1, w2, ...}{Character. Individual words in the n-gram}
#'     \item{n}{Integer. Count of occurrences}
#'     \item{p}{Numeric. Relative frequency (proportion)}
#'   }
#'   Sorted by count in descending order.
#'
#' @details
#' The function splits n-grams (e.g., "hello world") into separate columns
#' (w1 = "hello", w2 = "world") for easier analysis and language modeling.
#' Incomplete n-grams (with missing words) are filtered out.
#'
#' For large datasets (>= parallel_threshold), the function processes data in parallel,
#' splitting and counting n-grams across multiple cores before aggregating results.
#'
#' @examples
#' \dontrun{
#' # Bigram frequencies
#' corpus <- load_corpus("en_US")
#' bigrams <- tokenize_bigrams(corpus)
#' freq_bi <- freq_ngrams(bigrams, n = 2)
#' head(freq_bi)
#' 
#' # Trigram frequencies
#' trigrams <- tokenize_trigrams(corpus)
#' freq_tri <- freq_ngrams(trigrams, n = 3)
#' head(freq_tri)
#' 
#' # Force sequential processing
#' freq_bi_seq <- freq_ngrams(bigrams, n = 2, n_cores = 1)
#' }
#'
#' @seealso \code{\link{freq_bigrams}}, \code{\link{freq_trigrams}} for convenience functions
#'
#' @export
#' @importFrom tidyr separate
#' @importFrom dplyr count mutate filter across all_of if_all bind_rows
#' @importFrom parallel makeCluster stopCluster parLapply clusterEvalQ detectCores
freq_ngrams <- function(ngrams_tbl, n = 2, n_cores = NULL, parallel_threshold = 50000) {
  stopifnot("ng" %in% names(ngrams_tbl))
  stopifnot(n >= 2)
  
  n_rows <- nrow(ngrams_tbl)
  cols <- paste0("w", seq_len(n))
  
  # Sequential processing for small datasets
  if (n_rows < parallel_threshold) {
    out <- ngrams_tbl |>
      tidyr::separate(.data$ng, into = cols, sep = " ", remove = TRUE, fill = "right") |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ !is.na(.x) & .x != ""))
    
    out <- out |>
      dplyr::count(dplyr::across(dplyr::all_of(cols)), sort = TRUE, name = "n") |>
      dplyr::mutate(p = .data$n / sum(.data$n))
    
    return(out)
  }
  
  # Parallel processing for large datasets
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 6)
  }
  
  if (n_cores == 1) {
    out <- ngrams_tbl |>
      tidyr::separate(.data$ng, into = cols, sep = " ", remove = TRUE, fill = "right") |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ !is.na(.x) & .x != ""))
    
    out <- out |>
      dplyr::count(dplyr::across(dplyr::all_of(cols)), sort = TRUE, name = "n") |>
      dplyr::mutate(p = .data$n / sum(.data$n))
    
    return(out)
  }
  
  # Setup parallel cluster
  cl <- parallel::makeCluster(n_cores)
  on.exit({
    parallel::stopCluster(cl)
    gc()
  })
  
  # Export required packages and variables
  parallel::clusterEvalQ(cl, {
    library(dplyr)
    library(tidyr)
  })
  
  # Split data into chunks
  chunk_size <- ceiling(n_rows / n_cores)
  chunks <- split(ngrams_tbl, ceiling(seq_len(n_rows) / chunk_size))
  
  # Process chunks in parallel
  partial_counts <- parallel::parLapply(cl, chunks, function(chunk) {
    cols_local <- paste0("w", seq_len(n))
    chunk |>
      tidyr::separate(ng, into = cols_local, sep = " ", remove = TRUE, fill = "right") |>
      dplyr::filter(dplyr::if_all(dplyr::all_of(cols_local), ~ !is.na(.x) & .x != "")) |>
      dplyr::count(dplyr::across(dplyr::all_of(cols_local)), name = "n")
  }, n = n)
  
  # Aggregate results
  result <- dplyr::bind_rows(partial_counts) |>
    dplyr::count(dplyr::across(dplyr::all_of(cols)), wt = .data$n, sort = TRUE, name = "n") |>
    dplyr::mutate(p = .data$n / sum(.data$n))
  
  result
}

#' Compute Bigram Frequencies
#'
#' Convenience function to compute frequency table for bigrams.
#' Equivalent to \code{freq_ngrams(bigrams_tbl, n = 2)}.
#'
#' @param bigrams_tbl A tibble with bigram data from \code{\link{tokenize_bigrams}}.
#'
#' @return A frequency table with columns w1, w2, n, p.
#'
#' @examples
#' \dontrun{
#' corpus <- load_corpus("en_US")
#' bigrams <- tokenize_bigrams(corpus)
#' freq_table <- freq_bigrams(bigrams)
#' head(freq_table)
#' }
#'
#' @export
freq_bigrams <- function(bigrams_tbl) {
  freq_ngrams(bigrams_tbl, n = 2)
}

#' Compute Trigram Frequencies
#'
#' Convenience function to compute frequency table for trigrams.
#' Equivalent to \code{freq_ngrams(trigrams_tbl, n = 3)}.
#'
#' @param trigrams_tbl A tibble with trigram data from \code{\link{tokenize_trigrams}}.
#'
#' @return A frequency table with columns w1, w2, w3, n, p.
#'
#' @examples
#' \dontrun{
#' corpus <- load_corpus("en_US")
#' trigrams <- tokenize_trigrams(corpus)
#' freq_table <- freq_trigrams(trigrams)
#' head(freq_table)
#' }
#'
#' @export
freq_trigrams <- function(trigrams_tbl) {
  freq_ngrams(trigrams_tbl, n = 3)
}