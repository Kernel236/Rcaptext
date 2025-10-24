#' Compute Frequency Table for Unigrams
#'
#' This function creates a frequency table from tokenized unigrams, showing
#' both absolute counts and relative frequencies for each unique word.
#'
#' @param unigrams A tibble with a column named "word", typically from
#'   \code{\link{tokenize_unigrams}}.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{word}{Character. The unique word}
#'     \item{n}{Integer. Count of occurrences}
#'     \item{p}{Numeric. Relative frequency (proportion)}
#'   }
#'   Sorted by count in descending order.
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
#' }
#'
#' @export
#' @importFrom dplyr count mutate
freq_unigrams <- function(unigrams) {
  unigrams |>
    dplyr::count(.data$word, sort = TRUE, name = "n") |>
    dplyr::mutate(p = .data$n / sum(.data$n))
}

#' Compute Frequency Table for N-grams
#'
#' This function creates frequency tables from n-gram data, splitting each
#' n-gram into separate word components and computing frequencies.
#'
#' @param ngrams_tbl A tibble with an "ng" column containing n-grams,
#'   typically from \code{\link{tokenize_ngrams}}.
#' @param n Integer. Length of the n-gram (2 for bigrams, 3 for trigrams).
#'   Must be >= 2.
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
#' }
#'
#' @seealso \code{\link{freq_bigrams}}, \code{\link{freq_trigrams}} for convenience functions
#'
#' @export
#' @importFrom tidyr separate
#' @importFrom dplyr count mutate filter across all_of if_all
freq_ngrams <- function(ngrams_tbl, n = 2) {
  stopifnot("ng" %in% names(ngrams_tbl))
  stopifnot(n >= 2)
  
  # separate the n-gram into columns w1..wn
  cols <- paste0("w", seq_len(n))
  out <- ngrams_tbl |>
    tidyr::separate(.data$ng, into = cols, sep = " ", remove = TRUE, fill = "right") |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(cols), ~ !is.na(.x) & .x != ""))
  
  # counts and absolute/relative frequencies
  out <- out |>
    dplyr::count(dplyr::across(dplyr::all_of(cols)), sort = TRUE, name = "n") |>
    dplyr::mutate(p = .data$n / sum(.data$n))

  out
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