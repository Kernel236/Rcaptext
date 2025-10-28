#' Compute Log-Probability for a Single Token using Stupid Backoff
#'
#' Internal helper function that computes the log-probability of a target token
#' given a two-word context (w1, w2) using the Stupid Backoff algorithm.
#'
#' @param w1 Character. First context word.
#' @param w2 Character. Second context word.
#' @param target Character. The target word to compute probability for.
#' @param tri_pruned Data frame. Pruned trigram model with columns (w1, w2, w3, p_mle).
#' @param bi_pruned Data frame. Pruned bigram model with columns (w1, w2, p_mle).
#' @param uni_lookup Data frame. Unigram lookup table with columns (word, p).
#' @param alpha Numeric. Backoff penalty factor (default: 0.4).
#' @param eps Numeric. Smoothing value to avoid log(0) (default: 1e-9).
#'
#' @return Numeric. Natural log-probability (single value).
#'
#' @keywords internal
#' @importFrom dplyr filter pull
token_logprob_sb <- function(w1, w2, target,
                             tri_pruned, bi_pruned, uni_lookup,
                             alpha = 0.4, eps = 1e-9) {

  # Trigram: P(w3|w1,w2) if exists
  p_tri <- tri_pruned %>%
    dplyr::filter(.data$w1 == !!w1, .data$w2 == !!w2, .data$w3 == !!target) %>%
    dplyr::pull(.data$p_mle)

  if (length(p_tri) > 0) {
    return(log(p_tri + eps))
  }

  # Bigram backoff: alpha * P(w2->target)
  p_bi <- bi_pruned %>%
    dplyr::filter(.data$w1 == !!w2, .data$w2 == !!target) %>%
    dplyr::pull(.data$p_mle)

  if (length(p_bi) > 0) {
    return(log(alpha * p_bi + eps))
  }

  # Unigram backoff: alpha^2 * P(target)
  p_uni <- uni_lookup %>%
    dplyr::filter(.data$word == !!target) %>%
    dplyr::pull(.data$p)

  return(log(alpha^2 * (ifelse(length(p_uni) > 0, p_uni, 0)) + eps))
}

#' Evaluate Perplexity on Test Set using Stupid Backoff
#'
#' Computes perplexity metrics for a language model on a test set of trigram windows.
#' Perplexity measures how well the model predicts the test data (lower is better).
#'
#' @param test_windows Tibble. Test data with columns: w1, w2, target, and optionally
#'   input_text and source. Typically from \code{\link{make_test_trigrams}}.
#' @param tri_pruned Data frame. Pruned trigram model with columns (w1, w2, w3, p_mle).
#' @param bi_pruned Data frame. Pruned bigram model with columns (w1, w2, p_mle).
#' @param uni_lookup Data frame. Unigram lookup table with columns (word, p).
#' @param alpha Numeric. Backoff penalty factor (default: 0.4).
#' @param eps Numeric. Smoothing value to avoid log(0) (default: 1e-9).
#' @param by_source Logical. If TRUE and 'source' column exists, computes per-source
#'   perplexity in addition to overall (default: TRUE).
#' @param use_progress Logical. If TRUE, displays progress bar (default: TRUE).
#'
#' @return List with two components:
#'   \describe{
#'     \item{summary}{Tibble with columns: scope, N (token count), mean_logp,
#'       and perplexity. Contains overall metrics and per-source if requested.}
#'     \item{per_case}{Tibble with per-instance results: input_text, w1, w2,
#'       target, logp (log-probability), p (probability), and source (if present).}
#'   }
#'
#' @details
#' Perplexity is defined as exp(-mean_log_prob). Lower perplexity indicates better
#' model fit. Typical ranges:
#' \itemize{
#'   \item Excellent: < 50
#'   \item Good: 50-100
#'   \item Acceptable: 100-200
#'   \item Poor: > 200
#' }
#'
#' @examples
#' \dontrun{
#' # Evaluate perplexity on test set
#' ppl <- evaluate_perplexity(
#'   test_windows = test_data,
#'   tri_pruned = model$tri_pruned,
#'   bi_pruned = model$bi_pruned,
#'   uni_lookup = model$uni_lookup,
#'   alpha = 0.4
#' )
#' print(ppl$summary)
#' }
#'
#' @export
#' @importFrom dplyr mutate group_by summarise bind_rows
evaluate_perplexity <- function(test_windows,
                                tri_pruned, bi_pruned, uni_lookup,
                                alpha = 0.4,
                                eps = 1e-9,
                                by_source = TRUE,
                                use_progress = TRUE) {

  stopifnot(all(c("w1","w2","target") %in% names(test_windows)))

  do_compute <- function() {
    n <- nrow(test_windows)
    logp <- numeric(n)

    for (i in seq_len(n)) {
      if (isTRUE(use_progress) && requireNamespace("progressr", quietly = TRUE)) p()
      logp[i] <- token_logprob_sb(
        w1 = test_windows$w1[i],
        w2 = test_windows$w2[i],
        target = test_windows$target[i],
        tri_pruned = tri_pruned,
        bi_pruned  = bi_pruned,
        uni_lookup = uni_lookup,
        alpha = alpha,
        eps = eps
      )
    }

    per_case <- test_windows %>%
      dplyr::mutate(
        logp = logp,
        p    = exp(logp)
      )

    # Overall metrics
    N  <- nrow(per_case)
    ml <- mean(per_case$logp)
    ppl_overall <- exp(-ml)

    sum_tbl <- dplyr::tibble(
      scope = "overall",
      N = N,
      mean_logp = ml,
      perplexity = ppl_overall
    )

    # per-source (se richiesto)
    if (isTRUE(by_source) && "source" %in% names(per_case)) {
      bys <- per_case %>%
        dplyr::group_by(source) %>%
        dplyr::summarise(
          N = dplyr::n(),
          mean_logp = mean(logp),
          perplexity = exp(-mean_logp),
          .groups = "drop"
        ) %>%
        dplyr::mutate(scope = paste0("source:", source)) %>%
        dplyr::select(scope, N, mean_logp, perplexity)
      sum_tbl <- dplyr::bind_rows(sum_tbl, bys)
    }

    list(summary = sum_tbl, per_case = per_case)
  }

  if (isTRUE(use_progress) && requireNamespace("progressr", quietly = TRUE)) {
    progressr::with_progress({
      p <<- progressr::progressor(steps = nrow(test_windows))
      do_compute()
    })
  } else {
    do_compute()
  }
}


#' Merge Accuracy and Perplexity Tables for Model Comparison
#'
#' Helper function to combine accuracy@k and perplexity metrics for comparing
#' multiple language models.
#'
#' @param df_acc Tibble. Accuracy results with columns: model_id, k, accuracy.
#' @param df_ppl Tibble. Perplexity results with columns: model_id, scope, perplexity.
#' @param k_pick Integer. Which k value to extract from accuracy table (default: 1
#'   for top-1 accuracy).
#'
#' @return Tibble with columns: model_id, accuracy (at k_pick), perplexity (overall).
#'
#' @details
#' Use this to create a joint comparison table when evaluating multiple models
#' with different hyperparameters.
#'
#' @examples
#' \dontrun{
#' # Compare models
#' combined <- merge_acc_ppl(
#'   df_acc = accuracy_results,
#'   df_ppl = perplexity_results,
#'   k_pick = 1
#' )
#' print(combined)
#' }
#'
#' @export
#' @importFrom dplyr filter select inner_join
merge_acc_ppl <- function(df_acc, df_ppl, k_pick = 1) {
  acc_k <- df_acc %>%
    dplyr::filter(.data$k == !!k_pick) %>%
    dplyr::select(.data$model_id, .data$accuracy)

  ppl_overall <- df_ppl %>%
    dplyr::filter(.data$scope == "overall") %>%
    dplyr::select(.data$model_id, .data$perplexity)

  acc_k %>%
    dplyr::inner_join(ppl_overall, by = "model_id")
}
