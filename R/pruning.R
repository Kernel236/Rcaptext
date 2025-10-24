#' Prune n-grams by minimum count threshold
#'
#' Removes n-grams with frequency counts below a specified threshold to reduce noise
#' from rare events and decrease model size. This is a common preprocessing step
#' in language modeling to focus on more reliable, frequently occurring patterns.
#'
#' @param tbl A data frame containing n-gram frequency data with required column:
#'   \itemize{
#'     \item \code{n}: Frequency count of the n-gram (numeric)
#'   }
#'   All other columns are preserved in the output.
#' @param min_count Integer. Minimum frequency count threshold (default: 2).
#'   N-grams with count < min_count are removed from the dataset.
#'
#' @return A data frame identical to the input but filtered to include only
#'   n-grams with frequency count >= min_count. The structure and column names
#'   are preserved.
#'
#' @examples
#' # Create sample n-gram data with varying frequencies
#' ngrams <- data.frame(
#'   word1 = c("the", "a", "an", "this", "that"),
#'   word2 = c("cat", "dog", "apple", "book", "car"),
#'   n = c(15, 8, 1, 25, 2)
#' )
#' 
#' # Remove rare n-grams (count < 3)
#' filtered <- prune_by_min_count(ngrams, min_count = 3)
#' print(filtered)
#' # Only keeps "the cat" (n=15), "a dog" (n=8), and "this book" (n=25)
#' 
#' # More conservative filtering (count < 10)
#' conservative <- prune_by_min_count(ngrams, min_count = 10)
#' print(conservative)
#' # Only keeps "the cat" (n=15) and "this book" (n=25)
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
prune_by_min_count <- function(tbl, min_count = 2) {
  stopifnot("n" %in% names(tbl))
  tbl %>% dplyr::filter(n >= min_count)
}

#' Keep top-N continuations per history context
#'
#' For each unique history context (e.g., previous word(s)), keeps only the top N
#' most probable continuations based on a target probability column. This dramatically
#' reduces model size while preserving the most important predictions for each context.
#' Essential for building compact, efficient language models.
#'
#' @param tbl A data frame containing n-gram data with conditional probabilities.
#' @param history_cols Character vector specifying the column names that define
#'   the history context. Examples:
#'   \itemize{
#'     \item For bigrams: \code{c("w1")} - group by first word
#'     \item For trigrams: \code{c("w1", "w2")} - group by first two words
#'   }
#' @param target_col Unquoted column name containing the probability or score
#'   to rank by (e.g., \code{p_cond} for conditional probability). The function
#'   will keep the N highest values of this column for each history context.
#' @param N Integer. Maximum number of continuations to keep per history context
#'   (default: 10). If a history context has fewer than N continuations, all are kept.
#'
#' @return A data frame with the same structure as input, but containing at most
#'   N rows for each unique combination of history_cols values. Rows are selected
#'   based on the highest values of target_col, with ties broken deterministically.
#'
#' @examples
#' # Create bigram data with conditional probabilities
#' bigrams <- data.frame(
#'   w1 = rep(c("the", "a"), each = 4),
#'   w2 = c("cat", "dog", "bird", "fish", "book", "car", "house", "tree"),
#'   n = c(10, 8, 5, 2, 6, 4, 3, 1),
#'   p_cond = c(0.4, 0.32, 0.2, 0.08, 0.43, 0.29, 0.21, 0.07)
#' )
#' 
#' # Keep top 2 continuations for each first word
#' top2 <- prune_topN_per_history(
#'   bigrams, 
#'   history_cols = c("w1"), 
#'   target_col = p_cond, 
#'   N = 2
#' )
#' print(top2)
#' # For "the": keeps "cat" (0.4) and "dog" (0.32)
#' # For "a": keeps "book" (0.43) and "car" (0.29)
#' 
#' # Example with trigrams
#' trigrams <- data.frame(
#'   w1 = rep("the", 6),
#'   w2 = rep(c("big", "small"), each = 3), 
#'   w3 = rep(c("cat", "dog", "bird"), 2),
#'   p_cond = c(0.5, 0.3, 0.2, 0.6, 0.25, 0.15)
#' )
#' 
#' # Keep top 2 continuations for each (w1, w2) pair
#' top2_tri <- prune_topN_per_history(
#'   trigrams,
#'   history_cols = c("w1", "w2"),
#'   target_col = p_cond,
#'   N = 2
#' )
#' print(top2_tri)
#'
#' @importFrom dplyr group_by slice_max ungroup across all_of
#' @importFrom magrittr %>%
#' @export
prune_topN_per_history <- function(tbl, history_cols, target_col, N = 10) {
  stopifnot(all(history_cols %in% names(tbl)))
  tbl %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(history_cols))) %>%
    dplyr::slice_max(order_by = {{ target_col }}, n = N, with_ties = FALSE) %>%
    dplyr::ungroup()
}