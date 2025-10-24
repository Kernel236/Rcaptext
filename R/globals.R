# Global variables to avoid R CMD check notes
# These are used in dplyr/tidyverse NSE (Non-Standard Evaluation)

utils::globalVariables(c(
  "word",
  "ng", 
  "n",
  "p",
  "source",
  "text",
  "rank",
  "cum_p",
  "is_short",
  "stop_words",
  "w1", "w2", "w3", "w4", "w5",  # for n-grams up to 5-grams
  ".data",
  # Language modeling variables
  "p_cond",     # conditional probability
  "score",      # prediction score
  # ML pipeline variables (make_test_trigrams.R)
  ".row_id",    # row identifier for train/test split
  ".text",      # temporary text column in test case generation
  ".wc",        # word count column in test case generation
  "input_text", # input context for test cases
  "target",     # target word for test cases
  # Accuracy evaluation variables (model_accuracy@k.R)
  "rank_hit"    # hit rank in accuracy evaluation
))