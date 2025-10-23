#' Tokenize Text into Individual Words (Unigrams)
#'
#' This function converts text into individual word tokens using tidytext's
#' tokenization engine. It's the first step in most text analysis workflows.
#'
#' @param corpus A tibble containing at least a text column, typically from
#'   \code{\link{load_corpus}} or \code{\link{sample_corpus}}.
#' @param text_col Character or unquoted column name containing the text to tokenize.
#'   Default is "text".
#'
#' @return A tibble with one row per word (token) and any other preserved 
#'   columns from the input (e.g., source). The text column is replaced with
#'   a "word" column.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Uses tidytext::unnest_tokens for robust tokenization
#'   \item Automatically converts to lowercase
#'   \item Removes empty or NA tokens
#'   \item Preserves other columns from input tibble
#' }
#'
#' @examples
#' \dontrun{
#' # Basic tokenization
#' corpus <- load_corpus("en_US")
#' unigrams <- tokenize_unigrams(corpus)
#' head(unigrams)
#' 
#' # Tokenize a specific column
#' custom_data <- tibble::tibble(
#'   id = 1:3,
#'   content = c("Hello world", "Text analysis", "R programming")
#' )
#' tokens <- tokenize_unigrams(custom_data, text_col = "content")
#' }
#'
#' @seealso \code{\link{tokenize_ngrams}} for n-gram tokenization
#'
#' @export
#' @importFrom tidytext unnest_tokens
#' @importFrom dplyr filter
tokenize_unigrams <- function(corpus, text_col = "text") {
  corpus |>
    tidytext::unnest_tokens(output = "word", input = {{text_col}}, drop = TRUE) |>
    dplyr::filter(!is.na(.data$word), .data$word != "")
}

#' Tokenize Text into N-grams
#'
#' This function creates n-grams (sequences of n consecutive words) from text.
#' Common values are n=2 (bigrams) and n=3 (trigrams), useful for capturing
#' word context and building language models.
#'
#' @param corpus A tibble containing at least a text column.
#' @param n Integer. Length of the n-gram (2 = bigram, 3 = trigram, etc.).
#'   Must be >= 2.
#' @param text_col Character or unquoted column name containing text to tokenize.
#'   Default is "text".
#'
#' @return A tibble with one row per n-gram and preserved columns from input.
#'   The text column is replaced with an "ng" column containing the n-gram string.
#'
#' @details
#' N-grams are created as space-separated strings of consecutive words.
#' For example, the text "hello world example" would create:
#' \itemize{
#'   \item Bigrams (n=2): "hello world", "world example"
#'   \item Trigrams (n=3): "hello world example"
#' }
#'
#' @examples
#' \dontrun{
#' # Create bigrams
#' corpus <- load_corpus("en_US") 
#' bigrams <- tokenize_ngrams(corpus, n = 2)
#' head(bigrams)
#' 
#' # Create trigrams
#' trigrams <- tokenize_ngrams(corpus, n = 3)
#' head(trigrams)
#' 
#' # Custom text column
#' custom_data <- tibble::tibble(
#'   content = c("Natural language processing", "Machine learning algorithms")
#' )
#' ngrams <- tokenize_ngrams(custom_data, n = 2, text_col = "content")
#' }
#'
#' @seealso \code{\link{tokenize_bigrams}}, \code{\link{tokenize_trigrams}} for convenience functions
#'
#' @export
#' @importFrom tidytext unnest_tokens
#' @importFrom dplyr filter
tokenize_ngrams <- function(corpus, n = 2, text_col = "text") {
  stopifnot(n >= 2)
  corpus |>
    tidytext::unnest_tokens(
      output = "ng",
      input  = {{ text_col }},
      token  = "ngrams",
      n      = n,
      drop   = TRUE
    ) |>
    dplyr::filter(!is.na(.data$ng), .data$ng != "")
}

#' Create Bigrams (2-grams)
#'
#' Convenience function to create bigrams (2-word sequences) from text.
#' This is equivalent to \code{tokenize_ngrams(corpus, n = 2)}.
#'
#' @param corpus A tibble containing text data.
#' @param text_col Character or unquoted column name containing text. Default is "text".
#'
#' @return A tibble with bigrams in the "ng" column.
#'
#' @examples
#' \dontrun{
#' corpus <- load_corpus("en_US")
#' bigrams <- tokenize_bigrams(corpus)
#' head(bigrams)
#' }
#'
#' @export
tokenize_bigrams <- function(corpus, text_col = "text") {
  tokenize_ngrams(corpus, n = 2, text_col = {{ text_col }})
}

#' Create Trigrams (3-grams)
#'
#' Convenience function to create trigrams (3-word sequences) from text.
#' This is equivalent to \code{tokenize_ngrams(corpus, n = 3)}.
#'
#' @param corpus A tibble containing text data.
#' @param text_col Character or unquoted column name containing text. Default is "text".
#'
#' @return A tibble with trigrams in the "ng" column.
#'
#' @examples
#' \dontrun{
#' corpus <- load_corpus("en_US")
#' trigrams <- tokenize_trigrams(corpus)
#' head(trigrams)
#' }
#'
#' @export
tokenize_trigrams <- function(corpus, text_col = "text") {
  tokenize_ngrams(corpus, n = 3, text_col = {{ text_col }})
}