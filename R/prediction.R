#' Extract last k tokens from input text
#'
#' Extracts and cleans the last k tokens from input text using the package's
#' text cleaning function. This is essential for preparing user input for
#' n-gram language models that need clean, tokenized context words for prediction.
#'
#' @param input Character string. The input text to extract tokens from.
#'   Can be a sentence, phrase, or any text string.
#' @param k Integer. Maximum number of tokens to extract from the end of the input
#'   (default: 2). This typically corresponds to the order of the language model:
#'   \itemize{
#'     \item k=1 for bigram models (need 1 previous word)  
#'     \item k=2 for trigram models (need 2 previous words)
#'   }
#'
#' @return Character vector of length <= k containing the last tokens from the
#'   cleaned input text. Tokens are:
#'   \itemize{
#'     \item Lowercased and cleaned using \code{clean_text()}
#'     \item Split on whitespace
#'     \item Empty strings removed
#'     \item Returned in original order (first element = earliest token)
#'   }
#'   Returns \code{character(0)} if no valid tokens are found.
#'
#' @examples
#' # Basic usage for trigram context (k=2)
#' extract_last_tokens("Hello world how are you?", k = 2)
#' # Returns: c("are", "you")
#' 
#' # Single token for bigram context (k=1) 
#' extract_last_tokens("The weather is nice", k = 1)
#' # Returns: "nice"
#' 
#' # Handle edge cases
#' extract_last_tokens("Single", k = 2)
#' # Returns: "single" (less than k tokens available)
#' 
#' extract_last_tokens("", k = 2)
#' # Returns: character(0) (no tokens)
#' 
#' extract_last_tokens("   Multiple    spaces   between   words   ", k = 3)
#' # Returns: c("between", "words") (after cleaning)
#'
#' @export
extract_last_tokens <- function(input, k = 2) {
  clean <- clean_text(input)
  toks  <- unlist(strsplit(clean, "\\s+"))
  toks  <- toks[toks != ""]
  if (length(toks) == 0) character(0) else tail(toks, k)
}

#' Predict next word using Stupid Backoff algorithm
#'
#' Predicts the most likely next words given input text using a Stupid Backoff
#' language model. The algorithm tries trigram predictions first, then backs off
#' to bigram and unigram models with decreasing confidence scores. This provides
#' robust predictions even when higher-order n-grams are not available.
#'
#' @param input Character string. The input text to predict continuations for.
#'   Will be cleaned and tokenized automatically using \code{extract_last_tokens}.
#' @param tri_pruned Data frame. Trigram model containing columns:
#'   \itemize{
#'     \item \code{w1}: First word of trigram (character)
#'     \item \code{w2}: Second word of trigram (character) 
#'     \item \code{w3}: Third word of trigram (character)
#'     \item \code{p_cond}: Conditional probability P(w3 | w1, w2) (numeric)
#'   }
#' @param bi_pruned Data frame. Bigram model containing columns:
#'   \itemize{
#'     \item \code{w1}: First word of bigram (character)
#'     \item \code{w2}: Second word of bigram (character)
#'     \item \code{p_cond}: Conditional probability P(w2 | w1) (numeric) 
#'   }
#' @param uni_lookup Data frame. Unigram model containing columns:
#'   \itemize{
#'     \item \code{word}: Word (character)
#'     \item \code{n}: Frequency count (numeric)
#'     \item \code{p}: Marginal probability (numeric)
#'   }
#'   Can also be a pre-computed unigram fallback table (see \code{uni_fallback} parameter).
#' @param alpha Numeric. Backoff penalty factor (default: 0.4). Applied as:
#'   \itemize{
#'     \item Trigram scores: original p_cond (no penalty)
#'     \item Bigram scores: alpha * p_cond  
#'     \item Unigram scores: alpha^2 * p
#'   }
#'   Lower values give stronger preference to higher-order n-grams.
#' @param top_k Integer. Number of predictions to return (default: 3).
#'   Returns the top_k highest-scoring word predictions.
#' @param uni_fallback Data frame or NULL. Pre-computed unigram fallback table with
#'   columns (word, score, source). If provided, skips recomputing alpha^2 * p for
#'   every prediction. Significantly speeds up batch predictions. If NULL (default),
#'   computes on-the-fly from \code{uni_lookup}.
#'
#' @return Data frame with top_k predicted words containing columns:
#'   \itemize{
#'     \item \code{word}: Predicted next word (character)
#'     \item \code{score}: Prediction confidence score (numeric, 0-1 range)
#'     \item \code{source}: Model source - "trigram", "bigram", or "unigram" (character)
#'   }
#'   Ordered by decreasing score (most confident predictions first).
#'
#' @details 
#' The Stupid Backoff algorithm works as follows:
#' \enumerate{
#'   \item Extract last 1-2 words from input using \code{extract_last_tokens}
#'   \item Try trigram model: P(w3 | w1, w2) if 2 context words available
#'   \item Try bigram model: alpha * P(w2 | w1) using last context word
#'   \item Try unigram model: alpha^2 * P(w) as final fallback
#'   \item Combine all candidates, keeping best score per word
#'   \item Return top_k highest-scoring predictions
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming you have trained models from build_pruned_lang_model()
#' lang_model <- build_pruned_lang_model(freq_uni, freq_bi, freq_tri)
#' 
#' # Predict next word after "I love"
#' predictions <- predict_next(
#'   input = "I love",
#'   tri_pruned = lang_model$tri_pruned,
#'   bi_pruned = lang_model$bi_pruned, 
#'   uni_lookup = lang_model$uni_lookup,
#'   alpha = 0.4,
#'   top_k = 5
#' )
#' 
#' print(predictions)
#' #   word score   source
#' # 1  you  0.35  trigram
#' # 2   it  0.28   bigram  
#' # 3 this  0.15 unigram
#' 
#' # Handle empty/short input
#' empty_pred <- predict_next("", lang_model$tri_pruned, 
#'                           lang_model$bi_pruned, lang_model$uni_lookup)
#' # Returns top unigrams with alpha^2 penalty
#' 
#' # Single word input (only bigram + unigram possible)
#' single_pred <- predict_next("hello", lang_model$tri_pruned,
#'                            lang_model$bi_pruned, lang_model$uni_lookup) 
#' }
#'
#' @importFrom dplyr filter transmute bind_rows group_by summarise arrange slice_head desc
#' @importFrom magrittr %>%
#' @export
predict_next <- function(input, tri_pruned, bi_pruned, uni_lookup,
                         alpha = 0.4, top_k = 3, uni_fallback = NULL) {
  # Pre-compute unigram fallback if not provided (happens once per call)
  if (is.null(uni_fallback)) {
    uni_fallback <- uni_lookup %>%
      dplyr::transmute(word = .data$word, score = (alpha^2) * .data$p, source = "unigram")
  }
  
  # last tokens (cleaned)
  last2 <- extract_last_tokens(input, k = 2)
  # fallback: if empty input, return top unigrams
  if (length(last2) == 0) {
    return(
      uni_fallback %>%
        dplyr::arrange(dplyr::desc(.data$score)) %>%
        dplyr::slice_head(n = top_k)
    )
  }

  # prepare last1 and (w1, w2)
  last1 <- tail(last2, 1)
  w1 <- if (length(last2) == 2) last2[1] else NA_character_
  w2 <- if (length(last2) == 2) last2[2] else NA_character_

  # 1) TRIGRAM candidates (exact two-word history), if available
  tri_cand <- NULL
  if (!is.na(w1) && !is.na(w2)) {
    tri_cand <- tri_pruned %>%
      dplyr::filter(.data$w1 == !!w1, .data$w2 == !!w2) %>%
      dplyr::transmute(word = .data$w3, score = .data$p_cond, source = "trigram")
  }

  # 2) BIGRAM candidates (one-word history) with backoff alpha
  bi_cand <- bi_pruned %>%
    dplyr::filter(.data$w1 == !!last1) %>%
    dplyr::transmute(word = .data$w2, score = alpha * .data$p_cond, source = "bigram")

  # 3) UNIGRAM fallback - only take top candidates (already sorted in uni_fallback)
  # This avoids bind_rows with 50K+ rows every time!
  
  # 4) Combine tri + bi first
  all_cands <- dplyr::bind_rows(tri_cand, bi_cand)
  
  # Only add unigrams if needed
  if (nrow(all_cands) == 0) {
    # No tri/bi matches: return top-k unigrams directly
    return(uni_fallback %>% dplyr::slice_head(n = top_k))
  } else if (nrow(all_cands) < top_k) {
    # Need unigrams to reach top_k: add enough from pre-sorted uni_fallback
    # Take more than needed to account for potential duplicates
    uni_needed <- top_k - nrow(all_cands) + 50  # +50 buffer for duplicates
    uni_cand <- uni_fallback %>% dplyr::slice_head(n = uni_needed)
    all_cands <- dplyr::bind_rows(all_cands, uni_cand)
  } else {
    # Have enough candidates, but unigrams might still rank higher
    # Only add top unigrams that could compete
    uni_cand <- uni_fallback %>% dplyr::slice_head(n = top_k * 2)
    all_cands <- dplyr::bind_rows(all_cands, uni_cand)
  }
  
  # Deduplicate and return top-k
  all_cands %>%
    dplyr::group_by(.data$word) %>%
    dplyr::summarise(score = max(.data$score), source = .data$source[which.max(.data$score)], .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$score)) %>%
    dplyr::slice_head(n = top_k)
}

#' Predict next word using Interpolation + IDF Reranking
#'
#' Predicts the most likely next words using linear interpolation of n-gram probabilities
#' combined with IDF-based reranking for improved prediction quality. This algorithm
#' combines trigram, bigram, and unigram probabilities using weighted interpolation,
#' then reranks candidates using inverse document frequency to favor more informative words.
#'
#' @param input Character string. The input text to predict continuations for.
#' @param tri_pruned Data frame. Trigram model with columns: w1, w2, w3, p_cond
#' @param bi_pruned Data frame. Bigram model with columns: w1, w2, p_cond  
#' @param uni_lookup Data frame. Unigram model with columns: word, p
#' @param lambda Numeric vector of length 3. Interpolation weights for [trigram, bigram, unigram]
#'   (default: c(0.7, 0.25, 0.05)). Must sum to 1.0.
#' @param beta Numeric. IDF reranking exponent (default: 0.7). Higher values give more weight to rare words.
#' @param top_k Integer. Number of predictions to return (default: 3).
#' @param top_uni Integer. Maximum unigram candidates to consider (default: 500).
#' @param penalize_stop Logical. Whether to penalize stopwords in certain contexts (default: TRUE).
#' @param idf_lookup Data frame or NULL. Pre-computed IDF lookup table with columns (word, idf).
#'   If NULL, will be computed from uni_lookup.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{word}: Predicted next word
#'     \item \code{p}: Interpolated probability 
#'     \item \code{score}: IDF-reranked score
#'     \item \code{source}: Highest contributing n-gram level
#'   }
#'
#' @examples
#' \dontrun{
#' # Using default interpolation weights
#' predictions <- predict_interpolated(
#'   input = "I would like",
#'   tri_pruned = tri_model,
#'   bi_pruned = bi_model,
#'   uni_lookup = uni_model,
#'   top_k = 5
#' )
#' 
#' # Custom interpolation weights
#' predictions <- predict_interpolated(
#'   input = "Hello world",
#'   tri_pruned = tri_model,
#'   bi_pruned = bi_model, 
#'   uni_lookup = uni_model,
#'   lambda = c(0.6, 0.3, 0.1),
#'   beta = 0.8,
#'   top_k = 3
#' )
#' }
#'
#' @export
predict_interpolated <- function(input, tri_pruned, bi_pruned, uni_lookup,
                                lambda = c(0.7, 0.25, 0.05), beta = 0.7, top_k = 3,
                                top_uni = 500, penalize_stop = TRUE, idf_lookup = NULL) {
  
  # Validate lambda weights
  if (abs(sum(lambda) - 1.0) > 1e-6) {
    lambda <- lambda / sum(lambda)  # Normalize
    warning("Lambda weights normalized to sum to 1.0")
  }
  
  λ3 <- lambda[1]; λ2 <- lambda[2]; λ1 <- lambda[3]
  
  # Pre-compute IDF lookup if not provided
  if (is.null(idf_lookup)) {
    idf_lookup <- uni_lookup %>%
      dplyr::mutate(idf = -log(.data$p + 1e-12)) %>%
      dplyr::select(.data$word, .data$idf)
  }
  
  # Stopword set for contextual penalization
  stop_set <- c("the","and","of","to","a","in","that","it","is","for","on","as",
                "with","at","this","be","are","was","i","you","he","she","they","we")
  
  # Clean and tokenize input
  clean <- clean_text(input)
  toks <- unlist(strsplit(clean, "\\s+"))
  toks <- toks[toks != ""]
  
  if (length(toks) == 0) {
    # Empty input: return top unigrams with IDF reranking
    return(
      uni_lookup %>%
        dplyr::slice_max(.data$p, n = top_k) %>%
        dplyr::left_join(idf_lookup, by = "word") %>%
        dplyr::mutate(
          idf = dplyr::coalesce(.data$idf, median(idf_lookup$idf, na.rm = TRUE)),
          score = .data$p * (.data$idf ^ beta),
          source = "unigram"
        ) %>%
        dplyr::select(.data$word, .data$p, .data$score, .data$source) %>%
        dplyr::arrange(dplyr::desc(.data$score)) %>%
        dplyr::slice_head(n = top_k)
    )
  }
  
  # Extract context
  w2 <- tolower(tail(toks, 1))
  w1 <- if (length(toks) >= 2) tolower(toks[length(toks)-1]) else "<s>"
  
  # Generate candidate set efficiently
  tri_cand <- tri_pruned %>% 
    dplyr::filter(.data$w1 == !!w1, .data$w2 == !!w2) %>% 
    dplyr::transmute(word = .data$w3)
  
  bi_cand <- bi_pruned %>% 
    dplyr::filter(.data$w1 == !!w2) %>% 
    dplyr::transmute(word = .data$w2)
  
  uni_cand <- uni_lookup %>% 
    dplyr::slice_max(.data$p, n = top_uni) %>% 
    dplyr::transmute(word = .data$word)
  
  candidates <- dplyr::bind_rows(tri_cand, bi_cand, uni_cand) %>%
    dplyr::distinct()
  
  if (nrow(candidates) == 0) {
    candidates <- uni_lookup %>% 
      dplyr::transmute(word = .data$word) %>% 
      dplyr::slice_head(n = top_uni)
  }
  
  # Vectorized probability computation
  tri_probs <- tri_pruned %>%
    dplyr::filter(.data$w1 == !!w1, .data$w2 == !!w2) %>%
    dplyr::select(.data$w3, tri_p = .data$p_cond)
  
  bi_probs <- bi_pruned %>%
    dplyr::filter(.data$w1 == !!w2) %>%
    dplyr::select(word = .data$w2, bi_p = .data$p_cond)
  
  uni_probs <- uni_lookup %>%
    dplyr::select(.data$word, uni_p = .data$p)
  
  # Join probabilities and compute interpolated score
  scored <- candidates %>%
    dplyr::left_join(tri_probs, by = c("word" = "w3")) %>%
    dplyr::left_join(bi_probs, by = "word") %>%
    dplyr::left_join(uni_probs, by = "word") %>%
    dplyr::mutate(
      tri_p = dplyr::coalesce(.data$tri_p, 0),
      bi_p = dplyr::coalesce(.data$bi_p, 0),
      uni_p = dplyr::coalesce(.data$uni_p, 0),
      p = λ3 * .data$tri_p + λ2 * .data$bi_p + λ1 * .data$uni_p,
      source = dplyr::case_when(
        .data$tri_p > 0 ~ "trigram",
        .data$bi_p > 0 ~ "bigram", 
        .data$uni_p > 0 ~ "unigram",
        TRUE ~ "unknown"
      )
    ) %>%
    dplyr::filter(.data$p > 0) %>%
    dplyr::left_join(idf_lookup, by = "word") %>%
    dplyr::mutate(
      idf = dplyr::coalesce(.data$idf, median(idf_lookup$idf, na.rm = TRUE)),
      score = .data$p * (.data$idf ^ beta)
    )
  
  # Apply contextual penalties if enabled
  if (penalize_stop && length(toks) > 0) {
    last_word <- tail(toks, 1)
    # Penalize stopwords after sentence-ending punctuation
    if (length(last_word) && stringr::str_detect(last_word, "[.!?]$")) {
      scored <- scored %>%
        dplyr::mutate(score = ifelse(.data$word %in% stop_set, .data$score * 0.7, .data$score))
    }
    # Don't penalize articles after prepositions
    if (length(toks) && tolower(last_word) %in% c("of","to","in","for")) {
      scored <- scored %>%
        dplyr::mutate(score = ifelse(.data$word %in% c("the","a","an"), .data$p * (.data$idf ^ beta), .data$score))
    }
  }
  
  # Return top-k results
  scored %>%
    dplyr::select(.data$word, .data$p, .data$score, .data$source) %>%
    dplyr::arrange(dplyr::desc(.data$score)) %>%
    dplyr::slice_head(n = top_k)
}