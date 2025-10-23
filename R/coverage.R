#' Build Cumulative Coverage Analysis
#'
#' This function analyzes how many unique terms are needed to cover certain
#' percentages of the total corpus. This is useful for vocabulary reduction
#' and understanding text distributions (Zipf's law).
#'
#' @param freq_tbl A frequency table with at least a column for relative
#'   frequency. For unigrams, should have "word" column. For n-grams,
#'   should have w1..wn columns or a "term" column.
#' @param p_col Character. Name of the column containing relative frequency.
#'   Default is "p".
#' @param thresholds Numeric vector. Cumulative coverage thresholds to analyze
#'   (e.g., c(0.5, 0.9) for 50% and 90% coverage). Default is c(0.5, 0.9).
#' @param name_col Character. Name of the column containing term names.
#'   Default is "word". If missing and w1..wn columns exist, a "term" column
#'   will be created automatically.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{cum_tbl}{Tibble with rank, cumulative probability, and original data}
#'     \item{summary}{Tibble showing how many unique terms needed for each threshold}
#'   }
#'
#' @details
#' The analysis helps answer questions like:
#' \itemize{
#'   \item How many words cover 50% of all text?
#'   \item What vocabulary size is needed for 90% coverage?
#'   \item How does coverage relate to rank (Zipf distribution)?
#' }
#'
#' @examples
#' \dontrun{
#' # Analyze unigram coverage
#' corpus <- load_corpus("en_US")
#' unigrams <- tokenize_unigrams(corpus)
#' freq_table <- freq_unigrams(unigrams)
#' 
#' coverage <- coverage_from_freq(freq_table, thresholds = c(0.5, 0.8, 0.9))
#' print(coverage$summary)
#' 
#' # Analyze bigram coverage
#' bigrams <- tokenize_bigrams(corpus)
#' freq_bi <- freq_bigrams(bigrams)
#' coverage_bi <- coverage_from_freq(freq_bi, thresholds = c(0.5, 0.9))
#' }
#'
#' @export
#' @importFrom dplyr arrange mutate row_number
#' @importFrom tidyr unite
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
coverage_from_freq <- function(freq_tbl, p_col = "p", thresholds = c(0.5, 0.9), name_col = "word") {
  # basic checks
  stopifnot(p_col %in% names(freq_tbl))
  
  # create "term" column if name_col doesn't exist
  tbl <- freq_tbl
  if (!name_col %in% names(tbl)) {
    # search for w1..wn columns and unite them into "term"
    w_cols <- grep("^w\\d+$", names(tbl), value = TRUE)
    if (length(w_cols) == 0) {
      stop("No name column found and no w1..wn columns available to build a term column.")
    }
    tbl <- tbl |>
      tidyr::unite(col = "term", dplyr::all_of(w_cols), sep = " ", remove = FALSE)
    name_col <- "term"
  }
  
  # sort by descending frequency, create rank and cumulative
  tbl <- tbl |>
    dplyr::arrange(dplyr::desc(.data[[p_col]])) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      cum_p = cumsum(.data[[p_col]])
    )
  
  # for each threshold, how many entries are needed?
  sum_tbl <- purrr::map_dfr(thresholds, function(th) {
    idx <- which(tbl$cum_p >= th)[1]
    tibble::tibble(
      threshold = th,
      n_unique  = idx,
      top_example = tbl[[name_col]][idx]
    )
  })
  
  list(
    cum_tbl = tbl,
    summary = sum_tbl
  )
}