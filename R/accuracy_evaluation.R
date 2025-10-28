#' Evaluate Language Model Accuracy@k with Performance Metrics
#'
#' Computes accuracy@k metrics for a next-word prediction model, measuring how often
#' the correct target word appears in the top-k predictions. This is the standard
#' evaluation metric for keyboard prediction and text suggestion systems.
#'
#' @param test_windows Tibble. Test data with at least \code{input_text} (context string)
#'   and \code{target} (ground truth next word). Typically output from
#'   \code{\link{make_test_trigrams}}. May include \code{source} for stratified analysis.
#' @param tri_pruned Tibble. Pruned trigram model with columns (w1, w2, w3, p_cond).
#' @param bi_pruned Tibble. Pruned bigram model with columns (w1, w2, p_cond).
#' @param uni_lookup Tibble. Unigram lookup table with columns (word, n, p).
#' @param alpha Numeric. Backoff coefficient for Stupid Backoff algorithm (default: 0.4).
#'   Controls penalty when backing off from trigram → bigram → unigram.
#' @param ks Integer vector. Values of k to evaluate, e.g., c(1, 3, 5) computes
#'   accuracy@1, accuracy@3, and accuracy@5 (default: c(1, 2, 3)).
#' @param timeit Logical. If TRUE, measures prediction latency statistics (default: TRUE).
#' @param timing_n Integer. Number of random test cases to time for performance
#'   profiling (default: 200). Useful for assessing real-time usability.
#' @param seed Integer. Random seed for reproducibility in timing sample (default: 123).
#' @param max_eval_cases Integer or NULL. Maximum number of test cases to evaluate
#'   (default: NULL = use all). For large test sets (e.g., 2M+ cases), setting this
#'   to 10K-50K dramatically speeds up evaluation with minimal loss in accuracy
#'   estimates. Samples randomly if provided.
#' @param use_progress Logical. If TRUE, displays progress bars using progressr
#'   and informative messages using cli (default: TRUE).
#'
#' @return A list with three components:
#'   \describe{
#'     \item{accuracy}{Tibble with columns \code{k} and \code{accuracy}. Shows
#'       proportion of test cases where target appears in top-k predictions.}
#'     \item{per_case}{Tibble with detailed per-instance results:
#'       \itemize{
#'         \item \code{input_text}: Input context string
#'         \item \code{target}: Ground truth next word
#'         \item \code{pred1, pred2, ..., predK}: Top-K predictions from model
#'         \item \code{rank_hit}: Position where target first appears (1-based), or NA if not in top-K
#'       }
#'       Useful for error analysis and understanding model failures.}
#'     \item{timing}{Tibble with latency statistics (only if \code{timeit=TRUE}):
#'       \itemize{
#'         \item \code{mean_ms}: Average prediction time in milliseconds
#'         \item \code{p50_ms}: Median latency (50th percentile)
#'         \item \code{p95_ms}: 95th percentile latency (tail latency)
#'         \item \code{n_calls}: Number of predictions timed
#'       }
#'       Critical for assessing real-time keyboard usability (target: <100ms).}
#'   }
#'
#' @details
#' **Accuracy@k Interpretation:**
#' - **Accuracy@1**: Strict metric - target must be THE top prediction (hardest)
#' - **Accuracy@3**: Practical for mobile keyboards showing 3 suggestions
#' - **Accuracy@5**: More lenient, suitable for desktop autocomplete
#'
#' **How It Works:**
#' \enumerate{
#'   \item For each test case, generates top-K predictions using \code{predict_next()}
#'   \item Checks if \code{target} appears anywhere in top-K predictions
#'   \item Aggregates hits across all test cases to compute accuracy@k
#'   \item Optionally measures prediction latency for performance profiling
#' }
#'
#' **Performance Benchmarks:**
#' - Mobile keyboards: <50ms per prediction (p95)
#' - Desktop autocomplete: <100ms acceptable
#' - Batch processing: latency less critical
#'
#' **Note:** Requires a helper function \code{get_pred_vec()} that wraps
#' \code{\link{predict_next}} to return a character vector of predictions.
#'
#' @examples
#' \dontrun{
#' # Create train/test split
#' splits <- split_corpus(corpus, prop_test = 0.1)
#'
#' # Build model
#' model <- build_model(splits$train, sample_prop = 0.1)
#'
#' # Generate test cases
#' test_data <- make_test_trigrams(splits$test, prop = 0.2)
#'
#' # Evaluate accuracy@k with timing
#' results <- evaluate_accuracy_at_k(
#'   test_windows = test_data,
#'   tri_pruned = model$tri_pruned,
#'   bi_pruned = model$bi_pruned,
#'   uni_lookup = model$uni_lookup,
#'   ks = c(1, 3, 5),
#'   timeit = TRUE
#' )
#'
#' # View aggregate accuracy
#' print(results$accuracy)
#' #   k  accuracy
#' # 1 1  0.182
#' # 2 3  0.341
#' # 3 5  0.428
#'
#' # Check latency
#' print(results$timing)
#' #   mean_ms  p50_ms  p95_ms  n_calls
#' # 1    12.3    10.5    28.4      200
#'
#' # Analyze failures
#' failures <- results$per_case %>%
#'   filter(is.na(rank_hit)) %>%
#'   head(10)
#' }
#'
#' @seealso
#' \code{\link{make_test_trigrams}} for generating test data,
#' \code{\link{predict_next}} for the underlying prediction function,
#' \code{\link{build_model}} for training the model
#'
#' @export
#' @importFrom dplyr bind_cols bind_rows select tibble
evaluate_accuracy_at_k <- function(
  test_windows,
  tri_pruned, bi_pruned, uni_lookup,
  alpha = 0.4,
  ks = c(1, 2, 3),
  timeit = TRUE,
  timing_n = 200,
  seed = 123,
  max_eval_cases = NULL,
  use_progress = TRUE
) {
  stopifnot(all(c("input_text", "target") %in% names(test_windows)))
  Kmax <- max(ks)

  # Wrapper for progress
  run_evaluation <- function() {
    # ---- Optional sampling to reduce evaluation time ----
    if (!is.null(max_eval_cases) && nrow(test_windows) > max_eval_cases) {
      if (use_progress) {
        cli::cli_h2("Evaluating model")
        cli::cli_alert_info("Sampling {max_eval_cases} of {nrow(test_windows)} test cases for faster evaluation")
      } else {
        message(">> Sampling ", max_eval_cases, " of ", nrow(test_windows), " test cases")
      }
      set.seed(seed)
      test_windows <- test_windows %>% dplyr::slice_sample(n = max_eval_cases)
    } else {
      if (use_progress) {
        cli::cli_h2("Evaluating model ({nrow(test_windows)} test cases)")
      }
    }
    
    if (use_progress) {
      cli::cli_alert_info("Pre-computing unigram fallback...")
    }
    
    # ---- PRE-COMPUTE unigram fallback ONCE and SORT by score (HUGE performance boost!) ----
    # OPTIMIZATION: Use base R for speed on large uni_lookup (51K rows)
    # Sort by p descending (already computed), multiply by alpha^2
    alpha_sq <- alpha^2
    
    # Convert to data.frame for faster base R operations
    uni_df <- as.data.frame(uni_lookup)
    uni_df <- uni_df[order(-uni_df$p), ]  # Sort by p descending (fast!)
    uni_df$score <- uni_df$p * alpha_sq
    uni_df$source <- "unigram"
    
    # Convert back to tibble with only needed columns
    uni_fallback <- dplyr::tibble(
      word = uni_df$word,
      score = uni_df$score,
      source = uni_df$source
    )
    
    rm(uni_df)  # Free memory
    gc(verbose = FALSE)
    
    if (use_progress) {
      cli::cli_alert_success("Unigram fallback ready ({nrow(uni_fallback)} words)")
    }
    
    # ---- Generate per-case predictions ----
    n_cases <- nrow(test_windows)
    
    # Setup progress with fixed steps (100 updates)
    if (use_progress) {
      cli::cli_alert_info("Generating predictions for {n_cases} test cases...")
      progress_steps <- 100  # 100 updates = 1% each
      p <- progressr::progressor(steps = progress_steps)
      progress_every <- ceiling(n_cases / progress_steps)
    }
    
    pred_list <- vector("list", n_cases)
    for (i in seq_len(n_cases)) {
      if (use_progress && i %% progress_every == 0) {
        p(sprintf("Predicting (%d%%)", round(100 * i / n_cases)))
      }
      x <- test_windows$input_text[i]
      preds <- get_pred_vec(x, tri_pruned, bi_pruned, uni_lookup, 
                           alpha = alpha, top_n = Kmax, uni_fallback = uni_fallback)
      # Pad with NA if model returns fewer than Kmax predictions
      if (length(preds) < Kmax) {
        preds <- c(preds, rep(NA_character_, Kmax - length(preds)))
      }
      pred_list[[i]] <- preds
    }

    # Build wide table with pred1, pred2, ..., predK columns
    pred_mat <- do.call(rbind, pred_list)
    colnames(pred_mat) <- paste0("pred", seq_len(Kmax))
    per_case <- dplyr::bind_cols(
      test_windows %>% dplyr::select(.data$input_text, .data$target),
      as.data.frame(pred_mat, stringsAsFactors = FALSE)
    )

    # Compute rank_hit: position of first match between target and predictions (1-based)
    per_case$rank_hit <- apply(per_case[paste0("pred", seq_len(Kmax))], 1, function(row) {
      hit_pos <- which(row == per_case$target[which(rownames(per_case) == rownames(as.data.frame(t(row))))])
      if (length(hit_pos) == 0) NA_integer_ else hit_pos[1]
    })

    # ---- Aggregate accuracy@k metrics ----
    acc_tbl <- lapply(ks, function(k) {
      # For each test case, check if target appears in top-k predictions
      hits_k <- mapply(function(target, ...) {
        preds_k <- c(...)
        any(preds_k[seq_len(k)] == target, na.rm = TRUE)
      }, per_case$target, split(per_case[paste0("pred", seq_len(Kmax))], seq_len(nrow(per_case))) )
      dplyr::tibble(k = k, accuracy = mean(hits_k, na.rm = TRUE))
    }) %>% dplyr::bind_rows()
    
    # Free memory: pred_list and pred_mat no longer needed
    rm(pred_list, pred_mat)
    gc(verbose = FALSE)

    # ---- Optional: Measure prediction latency ----
    timing_tbl <- NULL
    if (isTRUE(timeit)) {
      if (use_progress) {
        cli::cli_alert_info("Measuring prediction latency ({timing_n} samples)...")
      }
      
      set.seed(seed)
      # Sample subset of test cases for timing to avoid long runtimes
      idx <- sample.int(nrow(test_windows), size = min(timing_n, nrow(test_windows)))
      t_ms <- numeric(length(idx))
      
      # Setup progress for timing (20 steps)
      if (use_progress) {
        p_timing <- progressr::progressor(steps = 20)
        timing_every <- ceiling(length(idx) / 20)
      }
      
      for (j in seq_along(idx)) {
        if (use_progress && j %% timing_every == 0) {
          p_timing(sprintf("Timing (%d%%)", round(100 * j / length(idx))))
        }
        q <- test_windows$input_text[idx[j]]
        t0 <- proc.time()[["elapsed"]]
        invisible(get_pred_vec(q, tri_pruned, bi_pruned, uni_lookup, 
                              alpha = alpha, top_n = Kmax, uni_fallback = uni_fallback))
        t1 <- proc.time()[["elapsed"]]
        t_ms[j] <- (t1 - t0) * 1000  # Convert to milliseconds
      }
      
      timing_tbl <- dplyr::tibble(
        mean_ms = mean(t_ms),
        p50_ms  = stats::quantile(t_ms, 0.50, names = FALSE),
        p95_ms  = stats::quantile(t_ms, 0.95, names = FALSE),
        n_calls = length(t_ms)
      )
    }

    if (use_progress) {
      cli::cli_alert_success("Evaluation complete! Accuracy@{max(ks)}: {round(acc_tbl$accuracy[acc_tbl$k == max(ks)], 3)}")
    }

    return(list(
      accuracy = acc_tbl,
      per_case = per_case,
      timing   = timing_tbl
    ))
  }

  # Execute with or without progress
  if (use_progress) {
    progressr::with_progress(run_evaluation())
  } else {
    run_evaluation()
  }
}


#' Get Prediction Vector from Model (Helper for Accuracy Evaluation)
#'
#' Internal helper that wraps \code{\link{predict_next}} to return a simple
#' character vector of top-N predictions. Used by \code{\link{evaluate_accuracy_at_k}}
#' for batch evaluation.
#'
#' @param input_text Character. Input context string (e.g., "the quick").
#' @param tri_pruned Tibble. Pruned trigram model.
#' @param bi_pruned Tibble. Pruned bigram model.
#' @param uni_lookup Tibble. Unigram lookup table.
#' @param alpha Numeric. Backoff coefficient (default: 0.4).
#' @param top_n Integer. Number of predictions to return (default: 3).
#' @param uni_fallback Tibble or NULL. Pre-computed unigram fallback table
#'   for performance optimization (default: NULL).
#'
#' @return Character vector of length \code{top_n} with predicted words,
#'   or shorter if model has fewer predictions. Never returns NA values
#'   from the prediction itself (only padding happens externally).
#'
#' @keywords internal
#' @importFrom magrittr %>%
get_pred_vec <- function(input_text, tri_pruned, bi_pruned, uni_lookup, 
                         alpha = 0.4, top_n = 3, uni_fallback = NULL) {
  pred_df <- predict_next(
    input = input_text,  # Note: predict_next uses 'input' not 'input_text'
    tri_pruned = tri_pruned,
    bi_pruned = bi_pruned,
    uni_lookup = uni_lookup,
    alpha = alpha,
    top_k = top_n,
    uni_fallback = uni_fallback
  )
  
  if (nrow(pred_df) == 0) {
    return(character(0))
  }
  
  pred_df$word
}
