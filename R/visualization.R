#' Plot Cumulative Coverage Curve
#'
#' This function creates a visualization showing how cumulative coverage
#' increases with the number of unique terms included. Useful for understanding
#' vocabulary distributions and determining optimal vocabulary sizes.
#'
#' @param freq_tbl A frequency table with relative frequency column.
#' @param p_col Character. Name of column containing relative frequencies.
#'   Default is "p".
#' @param name_col Character. Name of column containing term names.
#'   Default is "word". If missing, will attempt to create from w1..wn columns.
#'
#' @return A ggplot2 object showing the cumulative coverage curve.
#'
#' @details
#' The plot shows:
#' \itemize{
#'   \item X-axis: Number of unique terms (rank order)
#'   \item Y-axis: Cumulative coverage percentage
#'   \item Curve shape indicates distribution characteristics (Zipf-like)
#' }
#'
#' @examples
#' \dontrun{
#' # Plot unigram coverage
#' corpus <- load_corpus("en_US")
#' unigrams <- tokenize_unigrams(corpus)
#' freq_table <- freq_unigrams(unigrams)
#' 
#' plot_cumulative_coverage(freq_table)
#' 
#' # Plot bigram coverage
#' bigrams <- tokenize_bigrams(corpus)
#' freq_bi <- freq_bigrams(bigrams)
#' plot_cumulative_coverage(freq_bi)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line labs scale_y_continuous theme_minimal
#' @importFrom scales percent
plot_cumulative_coverage <- function(freq_tbl, p_col = "p", name_col = "word") {
  cov <- coverage_from_freq(freq_tbl, p_col = p_col, thresholds = numeric(), name_col = name_col)
  ggplot2::ggplot(cov$cum_tbl, ggplot2::aes(x = .data$rank, y = .data$cum_p)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Unique terms included (rank)",
      y = "Cumulative coverage",
      title = "Cumulative coverage curve"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal()
}

#' Plot Top Terms by Frequency
#'
#' This function creates a horizontal bar chart showing the most frequent
#' terms in the corpus. Useful for quick inspection of dominant vocabulary.
#'
#' @param freq_tbl A frequency table with count column and term identification.
#' @param top Integer. Number of top terms to display. Default is 20.
#' @param name_col Character. Name of column containing term names.
#'   Default is "word". If missing, will attempt to create from w1..wn columns.
#'
#' @return A ggplot2 object showing horizontal bars for top terms.
#'
#' @examples
#' \dontrun{
#' # Plot top words
#' corpus <- load_corpus("en_US")
#' unigrams <- tokenize_unigrams(corpus)
#' freq_table <- freq_unigrams(unigrams)
#' 
#' plot_top_terms(freq_table, top = 15)
#' 
#' # Plot top bigrams
#' bigrams <- tokenize_bigrams(corpus)
#' freq_bi <- freq_bigrams(bigrams)
#' plot_top_terms(freq_bi, top = 10)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal
#' @importFrom dplyr slice_head
#' @importFrom tidyr unite
plot_top_terms <- function(freq_tbl, top = 20, name_col = "word") {
  tbl <- freq_tbl
  if (!name_col %in% names(tbl)) {
    w_cols <- grep("^w\\d+$", names(tbl), value = TRUE)
    if (length(w_cols) == 0) stop("No term column found (word/term or w1..wn).")
    tbl <- tbl |>
      tidyr::unite(col = "term", dplyr::all_of(w_cols), sep = " ", remove = FALSE)
    name_col <- "term"
  }
  tbl |>
    dplyr::slice_head(n = top) |>
    ggplot2::ggplot(ggplot2::aes(x = reorder(.data[[name_col]], .data$n), y = .data$n)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Count", title = paste("Top", top, "terms")) +
    ggplot2::theme_minimal()
}

#' Plot Rank-Frequency Distribution (Zipf Plot)
#'
#' This function creates a log-log plot showing the relationship between
#' rank and frequency, which typically follows Zipf's law in natural language.
#'
#' @param freq_tbl A frequency table with relative frequency column.
#' @param top Integer. Number of top terms to include in plot. Default is 5000.
#' @param name_col Character. Name of column containing term names.
#'   Default is "word".
#' @param p_col Character. Name of column containing relative frequencies.
#'   Default is "p".
#'
#' @return A ggplot2 object showing log-log rank-frequency plot.
#'
#' @details
#' Zipf's law predicts that the frequency of a word is inversely proportional
#' to its rank. On a log-log plot, this appears as a straight line with
#' negative slope around -1.
#'
#' @examples
#' \dontrun{
#' # Plot Zipf distribution for words
#' corpus <- load_corpus("en_US")
#' unigrams <- tokenize_unigrams(corpus)
#' freq_table <- freq_unigrams(unigrams)
#' 
#' plot_rank_frequency(freq_table, top = 1000)
#' 
#' # Compare bigram distribution
#' bigrams <- tokenize_bigrams(corpus)
#' freq_bi <- freq_bigrams(bigrams)
#' plot_rank_frequency(freq_bi, top = 2000)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line scale_y_log10 scale_x_log10 labs theme_minimal
#' @importFrom dplyr arrange mutate slice_head
plot_rank_frequency <- function(freq_tbl, top = 5000, name_col = "word", p_col = "p") {
  tbl <- freq_tbl
  if (!name_col %in% names(tbl)) {
    w_cols <- grep("^w\\d+$", names(tbl), value = TRUE)
    if (length(w_cols) == 0) stop("No term column found (word/term or w1..wn).")
    tbl <- tbl |>
      tidyr::unite(col = "term", dplyr::all_of(w_cols), sep = " ", remove = FALSE)
    name_col <- "term"
  }
  tbl <- tbl |>
    dplyr::arrange(dplyr::desc(.data[[p_col]])) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::slice_head(n = top)
  
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$rank, y = .data[[p_col]])) +
    ggplot2::geom_line() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Rank (log scale)",
      y = "Frequency (log scale)",
      title = paste("Rank-frequency (top", top, ")")
    ) +
    ggplot2::theme_minimal()
}