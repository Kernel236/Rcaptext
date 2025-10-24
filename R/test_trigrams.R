#' Build Test Trigrams from Held-Out Corpus
#'
#' Creates test trigrams (w1, w2 → target) from a held-out corpus for evaluating
#' language model predictions. Each row represents a prediction task where the model
#' must predict \code{target} given the context \code{(w1, w2)}.
#'
#' @param corpus Tibble. Typically the test split from \code{\link{split_corpus}},
#'   must contain \code{source} and a cleaned text column.
#' @param text_col Character. Name of the cleaned text column (default: "text_clean").
#' @param prop Numeric in (0,1]. Proportion of corpus rows to sample for creating
#'   test trigrams (default: 0.2). Useful for controlling test set size.
#' @param seed Integer. Random seed for reproducibility (default: 123).
#' @param min_words Integer. Minimum number of words required in a text line after
#'   cleaning. Lines with fewer words are dropped (default: 3, minimum for trigrams).
#' @param by_source Logical. If TRUE, samples proportionally within each source
#'   category (default: TRUE).
#' @param drop_na_targets Logical. If TRUE, removes rows where target is NA or empty
#'   (default: TRUE).
#'
#' @return Tibble with columns:
#'   \describe{
#'     \item{input_text}{Character. The context string "w1 w2" to feed to predict_next()}
#'     \item{w1}{Character. First context word}
#'     \item{w2}{Character. Second context word}
#'     \item{target}{Character. The actual next word (ground truth)}
#'     \item{source}{Character. Source category (blogs/news/twitter)}
#'   }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Optionally samples corpus rows (stratified by source if \code{by_source=TRUE})
#'   \item Filters out lines with fewer than \code{min_words} tokens
#'   \item Tokenizes into trigrams using tidytext
#'   \item Splits each trigram into (w1, w2, target)
#'   \item Creates \code{input_text = "w1 w2"} for prediction
#' }
#'
#' Use this output with \code{\link{predict_next}} to evaluate model accuracy:
#' \code{predict_next(input_text, tri_pruned, bi_pruned, uni_lookup)}
#'
#' @examples
#' \dontrun{
#' # Split corpus
#' splits <- split_corpus(corpus, prop_test = 0.1)
#'
#' # Create test trigrams from test set
#' test_trigrams <- make_test_trigrams(
#'   corpus = splits$test,
#'   text_col = "text_clean",
#'   prop = 0.2,
#'   min_words = 4
#' )
#'
#' # Evaluate model
#' predictions <- test_trigrams %>%
#'   rowwise() %>%
#'   mutate(pred = list(predict_next(input_text, tri, bi, uni, top_k=3))) %>%
#'   unnest(pred)
#'
#' # Check accuracy
#' mean(predictions$target == predictions$word)
#' }
#'
#' @export
#' @importFrom dplyr group_by slice_sample ungroup mutate filter select
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr separate
#' @importFrom stringr str_count
#' @importFrom magrittr %>%
make_test_trigrams <- function(
    corpus,
    text_col = "text_clean",
    prop = 0.2,
    seed = 123,
    min_words = 3,
    by_source = TRUE,
    drop_na_targets = TRUE
) {
  stopifnot("source" %in% names(corpus))
  stopifnot(text_col %in% names(corpus))
  stopifnot(prop > 0 && prop <= 1)
  stopifnot(min_words >= 3)  # Need at least 3 words for trigrams
  
  set.seed(seed)

  # 1) Optional sampling of corpus rows (stratified by source)
  sampled <- if (isTRUE(by_source)) {
    corpus %>%
      dplyr::group_by(.data$source) %>%
      dplyr::slice_sample(prop = prop) %>%
      dplyr::ungroup()
  } else {
    corpus %>% 
      dplyr::slice_sample(prop = prop)
  }

  # 2) Filter rows with at least min_words tokens (post-clean)
  # Simple token count: count pattern [a-z']+
  sampled <- sampled %>%
    dplyr::mutate(
      .text = .data[[text_col]],
      .wc = stringr::str_count(.data$.text, "(?i)[a-z']+")
    ) %>%
    dplyr::filter(!is.na(.data$.text), .data$.wc >= min_words)

  # 3) Generate trigrams per row and separate into w1, w2, w3
  tri <- sampled %>%
    tidytext::unnest_tokens(output = "ng", input = .data$.text, 
                           token = "ngrams", n = 3, drop = TRUE) %>%
    tidyr::separate(.data$ng, into = c("w1","w2","w3"), sep = " ", remove = TRUE) %>%
    dplyr::filter(!is.na(.data$w1), !is.na(.data$w2), !is.na(.data$w3), 
                  .data$w1 != "", .data$w2 != "", .data$w3 != "")

  # 4) Build input_text and target columns
  out <- tri %>%
    dplyr::mutate(
      input_text = paste(.data$w1, .data$w2), 
      target = .data$w3
    ) %>%
    dplyr::select(.data$input_text, .data$w1, .data$w2, .data$target, .data$source)

  if (isTRUE(drop_na_targets)) {
    out <- out %>% 
      dplyr::filter(!is.na(.data$target), .data$target != "")
  }

  out
}
