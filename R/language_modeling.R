#' Build conditional probabilities for bigrams
#'
#' Computes conditional probabilities P(w2 | w1) using Maximum Likelihood Estimation (MLE)
#' from bigram frequency data. This is essential for building language models that can
#' predict the next word given the previous word.
#'
#' @param freq_bi A data frame containing bigram frequencies with required columns:
#'   \itemize{
#'     \item \code{w1}: First word of the bigram (character)
#'     \item \code{w2}: Second word of the bigram (character) 
#'     \item \code{n}: Frequency count of the bigram (numeric)
#'   }
#'   Optional columns like \code{p} (probability) are preserved if present.
#'
#' @return A data frame identical to the input but with an additional column:
#'   \itemize{
#'     \item \code{p_cond}: Conditional probability P(w2 | w1) (numeric)
#'   }
#'   The conditional probability is calculated as n / sum(n) for each w1.
#'
#' @examples
#' # Create sample bigram data
#' bigrams <- data.frame(
#'   w1 = c("the", "the", "a", "a"),
#'   w2 = c("cat", "dog", "cat", "bird"),
#'   n = c(10, 5, 3, 2)
#' )
#' 
#' # Calculate conditional probabilities
#' result <- build_cond_bigram(bigrams)
#' print(result)
#' # Shows P(cat|the) = 10/15 = 0.67, P(dog|the) = 5/15 = 0.33, etc.
#'
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#' @export
build_cond_bigram <- function(freq_bi) {
  stopifnot(all(c("w1","w2","n") %in% names(freq_bi)))
  freq_bi %>%
    dplyr::group_by(w1) %>%
    dplyr::mutate(p_cond = n / sum(n)) %>%   # MLE: P(w2 | w1)
    dplyr::ungroup()
}

#' Build conditional probabilities for trigrams
#'
#' Computes conditional probabilities P(w3 | w1, w2) using Maximum Likelihood Estimation (MLE)
#' from trigram frequency data. This enables more sophisticated language models that can
#' predict the next word given the previous two words.
#'
#' @param freq_tri A data frame containing trigram frequencies with required columns:
#'   \itemize{
#'     \item \code{w1}: First word of the trigram (character)
#'     \item \code{w2}: Second word of the trigram (character)
#'     \item \code{w3}: Third word of the trigram (character)
#'     \item \code{n}: Frequency count of the trigram (numeric)
#'   }
#'   Optional columns like \code{p} (probability) are preserved if present.
#'
#' @return A data frame identical to the input but with an additional column:
#'   \itemize{
#'     \item \code{p_cond}: Conditional probability P(w3 | w1, w2) (numeric)
#'   }
#'   The conditional probability is calculated as n / sum(n) for each (w1, w2) pair.
#'
#' @examples
#' # Create sample trigram data
#' trigrams <- data.frame(
#'   w1 = c("the", "the", "a", "a"),
#'   w2 = c("big", "big", "small", "small"),
#'   w3 = c("cat", "dog", "cat", "bird"),
#'   n = c(8, 4, 2, 1)
#' )
#' 
#' # Calculate conditional probabilities
#' result <- build_cond_trigram(trigrams)
#' print(result)
#' # Shows P(cat|the,big) = 8/12 = 0.67, P(dog|the,big) = 4/12 = 0.33, etc.
#'
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#' @export
build_cond_trigram <- function(freq_tri) {
  stopifnot(all(c("w1","w2","w3","n") %in% names(freq_tri)))
  freq_tri %>%
    dplyr::group_by(w1, w2) %>%
    dplyr::mutate(p_cond = n / sum(n)) %>%   # MLE: P(w3 | w1, w2)
    dplyr::ungroup()
}

#' Build a complete pruned n-gram language model
#'
#' Creates a comprehensive language model from unigram, bigram, and trigram frequency data.
#' The model includes conditional probabilities, pruning by minimum count and top-N filtering,
#' and optional persistence to RDS files. This is the main function for creating production-ready
#' language models for text prediction tasks.
#'
#' @param freq_uni A data frame with unigram frequencies containing columns:
#'   \itemize{
#'     \item \code{word}: The word (character)
#'     \item \code{n}: Frequency count (numeric)
#'     \item \code{p}: Marginal probability (numeric)
#'   }
#' @param freq_bi A data frame with bigram frequencies containing columns:
#'   \itemize{
#'     \item \code{w1}: First word (character)
#'     \item \code{w2}: Second word (character)
#'     \item \code{n}: Frequency count (numeric)
#'     \item \code{p}: Probability (numeric, optional)
#'   }
#' @param freq_tri A data frame with trigram frequencies containing columns:
#'   \itemize{
#'     \item \code{w1}: First word (character)
#'     \item \code{w2}: Second word (character)
#'     \item \code{w3}: Third word (character)
#'     \item \code{n}: Frequency count (numeric)
#'   }
#' @param min_count_bi Minimum count threshold for bigrams (default: 2). 
#'   Bigrams with count < min_count_bi are removed to reduce noise.
#' @param min_count_tri Minimum count threshold for trigrams (default: 2).
#'   Trigrams with count < min_count_tri are removed to reduce noise.
#' @param topN_bi Maximum number of bigram continuations to keep per history word (default: 12).
#'   Only the top N most probable continuations are retained for each w1.
#' @param topN_tri Maximum number of trigram continuations to keep per history pair (default: 8).
#'   Only the top N most probable continuations are retained for each (w1, w2) pair.
#' @param save Logical. If TRUE, saves the model components as RDS files (default: FALSE).
#' @param out_dir Character. Output directory for RDS files if save = TRUE (default: "data/processed").
#'   Directory is created if it doesn't exist.
#'
#' @return A list containing the complete language model:
#'   \itemize{
#'     \item \code{uni_lookup}: Unigram lookup table with columns 'next' and 'p' for fallback predictions
#'     \item \code{bi_pruned}: Pruned bigram model with columns w1, w2, n, p_cond
#'     \item \code{tri_pruned}: Pruned trigram model with columns w1, w2, w3, n, p_cond  
#'     \item \code{meta}: Metadata including timestamp, parameters, and size statistics
#'   }
#'
#' @examples
#' \dontrun{
#' # Load frequency data (typically from freq_unigrams, freq_bigrams, freq_trigrams)
#' uni_freq <- freq_unigrams(tokens_uni)
#' bi_freq <- freq_bigrams(tokens_bi) 
#' tri_freq <- freq_trigrams(tokens_tri)
#' 
#' # Build a compact language model
#' lang_model <- build_pruned_lang_model(
#'   freq_uni = uni_freq,
#'   freq_bi = bi_freq, 
#'   freq_tri = tri_freq,
#'   min_count_bi = 3,     # Remove rare bigrams
#'   min_count_tri = 2,    # Remove rare trigrams  
#'   topN_bi = 10,         # Keep top 10 bigram continuations
#'   topN_tri = 5,         # Keep top 5 trigram continuations
#'   save = TRUE,          # Save to RDS files
#'   out_dir = "models/"   # Custom output directory
#' )
#' 
#' # Check model statistics
#' print(lang_model$meta$sizes)
#' 
#' # Use for prediction
#' predictions <- predict_next("I love", lang_model$tri_pruned, 
#'                            lang_model$bi_pruned, lang_model$uni_lookup)
#' }
#'
#' @importFrom magrittr %>%
#' @export
build_pruned_lang_model <- function(freq_uni, freq_bi, freq_tri,
                            min_count_bi = 2, min_count_tri = 2,
                            topN_bi = 12, topN_tri = 8,
                            save = FALSE, out_dir = "data/processed") {
  stopifnot(all(c("word","n","p") %in% names(freq_uni)))
  stopifnot(all(c("w1","w2","n")  %in% names(freq_bi)))
  stopifnot(all(c("w1","w2","w3","n") %in% names(freq_tri)))

  # 1) conditional probabilities
  bi_cond  <- build_cond_bigram(freq_bi)
  tri_cond <- build_cond_trigram(freq_tri)

  # 2) pruning by min count
  bi_min  <- prune_by_min_count(bi_cond,  min_count = min_count_bi)
  tri_min <- prune_by_min_count(tri_cond, min_count = min_count_tri)

  # 3) top-N per history
  bi_pruned <- prune_topN_per_history(bi_min,  history_cols = c("w1"),     target_col = p_cond, N = topN_bi)
  tri_pruned <- prune_topN_per_history(tri_min, history_cols = c("w1","w2"), target_col = p_cond, N = topN_tri)

  # 4) unigram lookup (fallback): next = word, p = marginal prob
  uni_lookup <- freq_uni
  names(uni_lookup)[names(uni_lookup) == "word"] <- "next"

  # meta info
  meta <- list(
    timestamp = Sys.time(),
    params = list(min_count_bi = min_count_bi, min_count_tri = min_count_tri,
                  topN_bi = topN_bi, topN_tri = topN_tri),
    sizes = list(
      uni = nrow(uni_lookup),
      bi_before  = nrow(bi_cond),  bi_after  = nrow(bi_pruned),
      tri_before = nrow(tri_cond), tri_after = nrow(tri_pruned)
    )
  )

  # 5) optional save
  if (isTRUE(save)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(uni_lookup, file.path(out_dir, "uni_lookup.rds"))
    saveRDS(bi_pruned,  file.path(out_dir, "bi_pruned.rds"))
    saveRDS(tri_pruned, file.path(out_dir, "tri_pruned.rds"))
    saveRDS(meta,       file.path(out_dir, "lang_meta.rds"))
  }

  list(
    uni_lookup = uni_lookup,
    bi_pruned  = bi_pruned,   # w1, w2, n, p_cond
    tri_pruned = tri_pruned,  # w1, w2, w3, n, p_cond
    meta       = meta
  )
}