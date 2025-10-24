#' Split Corpus into Train and Test Sets
#'
#' Splits a corpus into training and test sets, with optional stratification by source.
#' This is essential for evaluating language models on held-out data.
#'
#' @param corpus Tibble containing at least \code{source} and a text column.
#' @param prop_test Numeric in (0,1). Proportion of rows to allocate to the test set (default: 0.1).
#' @param by_source Logical. If TRUE, stratifies the split within each source category
#'   (blogs, news, twitter) to maintain source distribution. Default: TRUE.
#' @param seed Integer. Random seed for reproducibility (default: 123).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{train}{Tibble. Training set (1 - prop_test of rows)}
#'     \item{test}{Tibble. Test set (prop_test of rows)}
#'   }
#'
#' @details
#' When \code{by_source = TRUE}, the function ensures that each source category
#' contributes proportionally to both train and test sets. This prevents bias
#' when sources have different characteristics (e.g., Twitter vs. news articles).
#'
#' The split preserves all columns from the original corpus.
#'
#' @examples
#' \dontrun{
#' # Basic 90/10 train/test split
#' corpus <- load_corpus("en_US")
#' splits <- split_corpus(corpus, prop_test = 0.1)
#' train_data <- splits$train
#' test_data <- splits$test
#'
#' # Without stratification
#' splits_simple <- split_corpus(corpus, prop_test = 0.2, by_source = FALSE)
#'
#' # Check distribution
#' table(splits$train$source)  # Should be proportional
#' table(splits$test$source)
#' }
#'
#' @export
#' @importFrom dplyr group_by slice_sample ungroup mutate row_number anti_join select
#' @importFrom magrittr %>%
split_corpus <- function(corpus, prop_test = 0.1, by_source = TRUE, seed = 123) {
  stopifnot("source" %in% names(corpus))
  stopifnot(prop_test > 0 && prop_test < 1)
  
  set.seed(seed)
  
  # Sample test set (stratified or not)
  if (by_source) {
    test <- corpus %>%
      dplyr::group_by(.data$source) %>%
      dplyr::slice_sample(prop = prop_test) %>%
      dplyr::ungroup()
  } else {
    test <- corpus %>% 
      dplyr::slice_sample(prop = prop_test)
  }
  
  # Get train set via anti_join (all rows not in test)
  corpus_idx <- corpus %>% dplyr::mutate(.row_id = dplyr::row_number())
  test_idx   <- test   %>% dplyr::mutate(.row_id = dplyr::row_number())
  
  train <- dplyr::anti_join(corpus_idx, test_idx, by = ".row_id") %>%
    dplyr::select(-.row_id)
  test <- test_idx %>% dplyr::select(-.row_id)
  
  list(train = train, test = test)
}
