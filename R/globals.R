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
  "score"       # prediction score
))