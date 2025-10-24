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
#'     \item \code{next}: Word (character)
#'     \item \code{p}: Marginal probability (numeric)
#'   }
#' @param alpha Numeric. Backoff penalty factor (default: 0.4). Applied as:
#'   \itemize{
#'     \item Trigram scores: original p_cond (no penalty)
#'     \item Bigram scores: alpha * p_cond  
#'     \item Unigram scores: alpha^2 * p
#'   }
#'   Lower values give stronger preference to higher-order n-grams.
#' @param top_k Integer. Number of predictions to return (default: 3).
#'   Returns the top_k highest-scoring word predictions.
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
                         alpha = 0.4, top_k = 3) {
  # last tokens (cleaned)
  last2 <- extract_last_tokens(input, k = 2)
  # fallback: if empty input, return top unigrams
  if (length(last2) == 0) {
    return(
      uni_lookup %>%
        dplyr::transmute(word = next, score = (alpha^2) * p, source = "unigram") %>%
        dplyr::arrange(dplyr::desc(score)) %>%
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
      dplyr::filter(w1 == !!w1, w2 == !!w2) %>%
      dplyr::transmute(word = w3, score = p_cond, source = "trigram")
  }

  # 2) BIGRAM candidates (one-word history) with backoff alpha
  bi_cand <- bi_pruned %>%
    dplyr::filter(w1 == !!last1) %>%
    dplyr::transmute(word = w2, score = alpha * p_cond, source = "bigram")

  # 3) UNIGRAM fallback with alpha^2
  uni_cand <- uni_lookup %>%
    dplyr::transmute(word = next, score = (alpha^2) * p, source = "unigram")

  # 4) Combine, deduplicate by best score, rank and top-k
  dplyr::bind_rows(tri_cand, bi_cand, uni_cand) %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(score = max(score), source = source[which.max(score)], .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    dplyr::slice_head(n = top_k)
}