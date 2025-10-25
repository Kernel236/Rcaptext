# ===============================================================================
#  PERFORMANCE ANALYSIS & REPORTING
# ===============================================================================
# 
# This module provides utilities for analyzing and visualizing model performance
# from accuracy@k evaluation results. Includes:
# - Formatted accuracy tables with percentages
# - Hit/miss breakdowns
# - Timing/latency summaries
# - Accuracy by source category
# - Visualization plots for accuracy, rank distribution, and latency

#' Format Accuracy Table with Percentages
#'
#' Converts raw accuracy values (0-1) to formatted percentage strings for
#' presentation in reports and tables.
#'
#' @param acc_tbl Tibble with columns \code{k} (integer) and \code{accuracy} (numeric 0-1).
#'   Typically from \code{\link{evaluate_accuracy_at_k}()$accuracy}.
#' @param digits Integer. Number of decimal places for percentage display (default: 1).
#'
#' @return Tibble with original columns plus \code{accuracy_pct} (character) containing
#'   formatted percentages like "34.1%".
#'
#' @examples
#' \dontrun{
#' results <- evaluate_accuracy_at_k(test_data, tri, bi, uni)
#' formatted <- format_accuracy_table(results$accuracy, digits = 2)
#' print(formatted)
#' #   k  accuracy  accuracy_pct
#' # 1 1  0.182     18.20%
#' # 2 3  0.341     34.10%
#' }
#'
#' @export
#' @importFrom dplyr mutate
#' @importFrom scales percent
#' @importFrom magrittr %>%
format_accuracy_table <- function(acc_tbl, digits = 1) {
  stopifnot(all(c("k","accuracy") %in% names(acc_tbl)))
  acc_tbl %>%
    dplyr::mutate(accuracy_pct = scales::percent(.data$accuracy, accuracy = 1, scale = 100)) %>%
    dplyr::mutate(accuracy_pct = format(round(.data$accuracy * 100, digits), nsmall = digits)) %>%
    dplyr::mutate(accuracy_pct = paste0(.data$accuracy_pct, "%"))
}

#' Build Comprehensive Performance Summary Tables
#'
#' Extracts and formats key performance metrics from evaluation results, including
#' accuracy@k, hit/miss counts, timing statistics, and optional per-source breakdown.
#'
#' @param eval_res List returned by \code{\link{evaluate_accuracy_at_k}}.
#'   Must contain \code{accuracy} and \code{per_case} components.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{accuracy}{Tibble. Formatted accuracy@k with percentage strings}
#'     \item{timing}{Tibble. Latency statistics (mean, p50, p95 in ms), or NULL if not measured}
#'     \item{hit_breakdown}{Tibble. Counts of top-1 hits, top-K hits, and total test cases}
#'     \item{accuracy_by_source}{Tibble. Accuracy@k broken down by source category
#'       (blogs/news/twitter), only if \code{source} column exists in test data}
#'   }
#'
#' @details
#' This function aggregates raw evaluation data into presentation-ready tables:
#' - **Hit breakdown**: Shows how many predictions were correct at top-1 vs anywhere in top-K
#' - **Timing**: Rounds latency metrics to 1 decimal place for readability
#' - **Source analysis**: Reveals if model performs differently on blogs vs news vs Twitter
#'
#' Useful for generating reports, dashboards, or quick performance summaries.
#'
#' @examples
#' \dontrun{
#' results <- evaluate_accuracy_at_k(test_data, tri, bi, uni, timeit = TRUE)
#' perf <- build_performance_tables(results)
#'
#' print(perf$accuracy)
#' print(perf$hit_breakdown)
#' print(perf$timing)
#' if (!is.null(perf$accuracy_by_source)) print(perf$accuracy_by_source)
#' }
#'
#' @seealso \code{\link{evaluate_accuracy_at_k}} for generating evaluation results,
#'   \code{\link{summarise_and_plot_eval}} for one-line reporting
#'
#' @export
#' @importFrom dplyr mutate group_by summarise select arrange desc tibble
#' @importFrom purrr map_dfr
#' @importFrom rlang sym
#' @importFrom magrittr %>%
build_performance_tables <- function(eval_res) {
  stopifnot(is.list(eval_res), "accuracy" %in% names(eval_res), "per_case" %in% names(eval_res))
  
  acc_tbl <- eval_res$accuracy %>% format_accuracy_table()
  
  # hit breakdown (quanti hit/miss alla top1/topK max)
  Kmax <- max(eval_res$accuracy$k)
  preds_cols <- paste0("pred", seq_len(Kmax))
  per <- eval_res$per_case
  
  # TRUE se target presente in almeno una delle top-K
  per$hit_any <- apply(per[preds_cols], 1, function(row) any(row == per$target[which(rownames(per) == rownames(as.data.frame(t(row))))]))
  per$hit_top1 <- per$pred1 == per$target
  
  hit_breakdown <- dplyr::tibble(
    metric = c("Top-1 hits", "Top-K hits", "Total cases"),
    value  = c(sum(per$hit_top1, na.rm = TRUE),
               sum(per$hit_any,  na.rm = TRUE),
               nrow(per))
  )
  
  # timing (se presente)
  timing_tbl <- NULL
  if (!is.null(eval_res$timing)) {
    timing_tbl <- eval_res$timing %>%
      dplyr::mutate(
        mean_ms = round(mean_ms, 1),
        p50_ms  = round(p50_ms,  1),
        p95_ms  = round(p95_ms,  1)
      )
  }
  
  # accuracy per source (se disponibile)
  acc_by_source <- NULL
  if ("source" %in% names(per)) {
    acc_by_source <- purrr::map_dfr(eval_res$accuracy$k, function(k) {
      colk <- paste0("pred", k)
      per %>%
        dplyr::mutate(hit_k = !!rlang::sym(colk) == target) %>%
        dplyr::group_by(source) %>%
        dplyr::summarise(accuracy = mean(hit_k, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(k = k) %>%
        dplyr::select(k, source, accuracy) %>%
        dplyr::arrange(k, dplyr::desc(accuracy)) %>%
        dplyr::mutate(accuracy_pct = paste0(round(accuracy * 100, 1), "%"))
    })
  }
  
  out <- list(
    accuracy = acc_tbl,
    timing   = timing_tbl,
    hit_breakdown = hit_breakdown
  )
  if (!is.null(acc_by_source)) out$accuracy_by_source <- acc_by_source
  out
}


# ===============================================================================
#  VISUALIZATION PLOTS
# ===============================================================================

#' Plot Accuracy@k as Bar Chart
#'
#' Creates a bar plot showing accuracy at different k values with percentage labels.
#'
#' @param acc_tbl Tibble with columns \code{k} and \code{accuracy} (0-1 scale).
#' @param title Character. Plot title (default: "Accuracy@k").
#'
#' @return A ggplot2 object. Use \code{print()} to display.
#'
#' @examples
#' \dontrun{
#' results <- evaluate_accuracy_at_k(test_data, tri, bi, uni)
#' p <- plot_accuracy_bars(results$accuracy, title = "Model Performance")
#' print(p)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_y_continuous labs theme_minimal
#' @importFrom scales percent_format
plot_accuracy_bars <- function(acc_tbl, title = "Accuracy@k") {
  stopifnot(all(c("k","accuracy") %in% names(acc_tbl)))
  ggplot2::ggplot(acc_tbl, ggplot2::aes(x = factor(.data$k), y = .data$accuracy)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$accuracy, accuracy = 1)),
                       vjust = -0.3, size = 3.5) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(x = "k", y = "Accuracy", title = title) +
    ggplot2::theme_minimal()
}

#' Plot Distribution of Hit Ranks
#'
#' Visualizes at which position (rank) the correct word appears in predictions.
#' Only shows cases where target was found (excludes NA/misses).
#'
#' @param per_case Tibble from \code{evaluate_accuracy_at_k()$per_case}.
#'   Must contain \code{rank_hit} column.
#' @param title Character. Plot title (default shows Italian, customize as needed).
#'
#' @return A ggplot2 object showing percentage of hits at each rank (1 = top-1, 2 = top-2, etc.).
#'
#' @details
#' This plot helps understand prediction quality:
#' - High bar at rank=1 → model often gets top prediction correct
#' - High bars at rank=2,3 → model needs to show multiple suggestions
#' - Excludes cases where target not in top-K (those are counted separately)
#'
#' @examples
#' \dontrun{
#' results <- evaluate_accuracy_at_k(test_data, tri, bi, uni, ks = c(1, 3, 5))
#' p <- plot_rank_hit_distribution(results$per_case)
#' print(p)
#' }
#'
#' @export
#' @importFrom dplyr filter count mutate
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_y_continuous labs theme_minimal
#' @importFrom scales percent_format percent
#' @importFrom magrittr %>%
plot_rank_hit_distribution <- function(per_case, title = "Distribuzione del rank della prima hit") {
  stopifnot("rank_hit" %in% names(per_case))
  tbl <- per_case %>%
    dplyr::filter(!is.na(.data$rank_hit)) %>%
    dplyr::count(.data$rank_hit) %>%
    dplyr::mutate(p = .data$n / sum(.data$n))
  ggplot2::ggplot(tbl, ggplot2::aes(x = factor(.data$rank_hit), y = .data$p)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(.data$p, accuracy = 1)),
                       vjust = -0.3, size = 3.5) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(x = "Rank della hit", y = "Percentuale su hit", title = title) +
    ggplot2::theme_minimal()
}

#' Plot Prediction Latency Summary
#'
#' Creates a bar chart showing mean, median (p50), and 95th percentile (p95) latency
#' in milliseconds. Useful for assessing real-time performance.
#'
#' @param timing_tbl Tibble from \code{evaluate_accuracy_at_k()$timing}.
#'   Must contain \code{mean_ms}, \code{p50_ms}, \code{p95_ms} columns.
#' @param title Character. Plot title (default shows Italian, customize as needed).
#'
#' @return A ggplot2 object. Returns error if timing_tbl is NULL/empty.
#'
#' @details
#' **Performance targets for keyboards:**
#' - Mean < 20ms: Excellent responsiveness
#' - P95 < 50ms: Acceptable for mobile keyboards
#' - P95 > 100ms: Noticeable lag, poor UX
#'
#' @examples
#' \dontrun{
#' results <- evaluate_accuracy_at_k(test_data, tri, bi, uni, timeit = TRUE)
#' if (!is.null(results$timing)) {
#'   p <- plot_latency_summary(results$timing)
#'   print(p)
#' }
#' }
#'
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs theme_minimal
plot_latency_summary <- function(timing_tbl, title = "Latency (ms): mean / p50 / p95") {
  if (is.null(timing_tbl) || nrow(timing_tbl) == 0) {
    stop("Timing table is NULL/empty. Run evaluate_accuracy_at_k(..., timeit=TRUE) first.")
  }
  tbl <- timing_tbl %>%
    tidyr::pivot_longer(cols = c(.data$mean_ms, .data$p50_ms, .data$p95_ms),
                        names_to = "stat", values_to = "ms")
  ggplot2::ggplot(tbl, ggplot2::aes(x = .data$stat, y = .data$ms)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(.data$ms,1), " ms")),
                       vjust = -0.3, size = 3.5) +
    ggplot2::labs(x = NULL, y = "Milliseconds", title = title) +
    ggplot2::theme_minimal()
}


# ===============================================================================
#  ONE-LINE REPORTING WRAPPER
# ===============================================================================

#' Comprehensive Performance Report with Plots
#'
#' One-line function to print performance tables and display visualizations.
#' Perfect for quick model evaluation during development.
#'
#' @param eval_res List returned by \code{\link{evaluate_accuracy_at_k}}.
#' @param plot_latency Logical. If TRUE and timing data available, plots latency chart (default: TRUE).
#'
#' @return Invisibly returns the performance tables list from \code{\link{build_performance_tables}}.
#'   Prints tables to console and displays plots.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Prints formatted accuracy@k table
#'   \item Prints timing statistics (if available)
#'   \item Prints hit/miss breakdown
#'   \item Prints per-source accuracy (if source data available)
#'   \item Displays accuracy@k bar chart
#'   \item Displays rank hit distribution
#'   \item Displays latency summary (if requested and available)
#' }
#'
#' Use this for rapid iteration: evaluate model → call this function → get instant insights.
#'
#' @examples
#' \dontrun{
#' # Quick evaluation workflow
#' results <- evaluate_accuracy_at_k(test_data, tri, bi, uni, 
#'                                    ks = c(1, 3, 5), timeit = TRUE)
#' 
#' # One line for complete report
#' perf <- summarise_and_plot_eval(results)
#' 
#' # Or suppress latency plot
#' perf <- summarise_and_plot_eval(results, plot_latency = FALSE)
#' 
#' # Access individual tables
#' print(perf$accuracy)
#' print(perf$hit_breakdown)
#' }
#'
#' @seealso \code{\link{evaluate_accuracy_at_k}} for generating evaluation results,
#'   \code{\link{build_performance_tables}} for table extraction
#'
#' @export
summarise_and_plot_eval <- function(eval_res, plot_latency = TRUE) {
  perf <- build_performance_tables(eval_res)
  
  # Stampa tabelle sintetiche
  cat("\n== Accuracy ==\n")
  print(perf$accuracy)
  if (!is.null(perf$timing)) {
    cat("\n== Timing (ms) ==\n")
    print(perf$timing)
  }
  cat("\n== Hit breakdown ==\n")
  print(perf$hit_breakdown)
  if (!is.null(perf$accuracy_by_source)) {
    cat("\n== Accuracy by source ==\n")
    print(perf$accuracy_by_source)
  }
  
  # Plot
  p1 <- plot_accuracy_bars(eval_res$accuracy, title = "Accuracy@k")
  print(p1)
  
  if ("rank_hit" %in% names(eval_res$per_case)) {
    p2 <- plot_rank_hit_distribution(eval_res$per_case)
    print(p2)
  }
  
  if (plot_latency && !is.null(eval_res$timing)) {
    p3 <- plot_latency_summary(eval_res$timing)
    print(p3)
  }
  
  invisible(perf)
}
