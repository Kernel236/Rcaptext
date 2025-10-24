# Rcaptext �

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R%20CMD%20check-probably%20works-brightgreen)](https://github.com/Kernel236/rcaptext)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Made with ❤️](https://img.shields.io/badge/Made%20with-❤️-red.svg)](https://github.com/Kernel236)
<!-- badges: end -->

> *"Yet another text analysis package, because apparently the world needed one more"* 🤷‍♂️

A modest collection of R functions for text analysis, born from the ashes of my capstone project. Probably only useful to me, but maybe someone else will find something good in it.

## What it does (more or less) 🎯

- **Loads text corpora** - specifically SwiftKey ones, but works with others too
- **Cleans text** - removes URLs, emojis, and other junk you don't need
- **Tokenizes** - unigrams, bigrams, trigrams... the usual stuff
- **Counts frequencies** - because counting matters
- **Builds language models** - n-gram models with conditional probabilities
- **Predicts text** - next word prediction with Stupid Backoff
- **Makes pretty plots** - to impress supervisors and committees
- **Parallel processing** ⚡ - multi-core file loading for faster corpus import

## Installation 🚀

```r
# From GitHub (the only place it lives)
# install.packages("devtools")  # if you don't have it already
devtools::install_github("Kernel236/Rcaptext")
```

## Quick Start 🏃‍♂️

```r
library(rcaptext)

# 1. Load a corpus (assuming you have SwiftKey files)
corpus <- load_corpus("en_US", base_dir = "data/raw")

# 2. Take a sample (because processing everything is slow)
sample_data <- sample_corpus(corpus, prop = 0.05, seed = 42)

# 3. Clean the text (goodbye junk)
sample_data$text <- clean_text(sample_data$text)

# 4. Tokenize
unigrams <- tokenize_unigrams(sample_data)
bigrams <- tokenize_bigrams(sample_data)
trigrams <- tokenize_trigrams(sample_data)

# 5. Count and analyze
freq_uni <- freq_unigrams(unigrams)
freq_bi <- freq_bigrams(bigrams)
freq_tri <- freq_trigrams(trigrams)

# 6. Build a language model for text prediction
lang_model <- build_pruned_lang_model(
  freq_uni = freq_uni,
  freq_bi = freq_bi,
  freq_tri = freq_tri,
  min_count_bi = 2,
  min_count_tri = 2,
  topN_bi = 12,
  topN_tri = 8
)

# 7. Predict next word
predictions <- predict_next(
  input = "I love",
  tri_pruned = lang_model$tri_pruned,
  bi_pruned = lang_model$bi_pruned,
  uni_lookup = lang_model$uni_lookup,
  top_k = 5
)

# 8. Make plots (the fun part)
plot_top_terms(freq_uni, top = 15)
plot_cumulative_coverage(freq_uni)
```

## How Language Modeling Works 🧠

The package implements a complete **n-gram language modeling** pipeline with **Stupid Backoff** for text prediction:

### 1. **Build Conditional Probabilities**
```r
# P(w2 | w1) - probability of w2 given w1
bi_cond <- build_cond_bigram(freq_bi)

# P(w3 | w1, w2) - probability of w3 given w1, w2
tri_cond <- build_cond_trigram(freq_tri)
```

### 2. **Prune the Models**
```r
# Remove rare n-grams (min count threshold)
bi_pruned <- prune_by_min_count(bi_cond, min_count = 2)
tri_pruned <- prune_by_min_count(tri_cond, min_count = 2)

# Keep only top-N most probable continuations per history
bi_pruned <- prune_topN_per_history(bi_pruned, history_cols = "w1", 
                                     target_col = p_cond, N = 12)
tri_pruned <- prune_topN_per_history(tri_pruned, history_cols = c("w1", "w2"),
                                      target_col = p_cond, N = 8)
```

### 3. **Predict with Stupid Backoff**
The algorithm tries models in order of specificity:
1. **Trigram**: If `(w1, w2)` context available → use `P(w3 | w1, w2)`
2. **Bigram**: If only `w1` context → use `α × P(w2 | w1)` (with backoff penalty)
3. **Unigram**: No context → use `α² × P(w)` (fallback to word frequency)

```r
# Predict next word after "I love"
predict_next("I love", tri_pruned, bi_pruned, uni_lookup, 
             alpha = 0.4, top_k = 5)
#   word  score    source
#   you   0.35    trigram
#   it    0.28    bigram
#   the   0.15    unigram
```

The `alpha` parameter (default 0.4) penalizes lower-order models, giving preference to higher-order n-grams when available.

## Package Structure 📦

```
rcaptext/
├── R/
│   ├── corpus.R            # Load & sample corpora
│   ├── text_cleaning.R     # Clean & filter text
│   ├── tokenization.R      # Create tokens
│   ├── frequency.R         # Count stuff
│   ├── coverage.R          # Coverage analysis
│   ├── language_modeling.R # Build n-gram models 🆕
│   ├── pruning.R           # Prune models 🆕
│   ├── prediction.R        # Text prediction 🆕
│   └── visualization.R     # Pretty plots
└──  man/                    # Documentation (auto-generated)
```

## Main Functions 🔧

### Corpus Management
- `load_corpus()` - Load the famous SwiftKey files (with parallel processing)
- `sample_corpus()` - Take a representative chunk

### Text Cleaning
- `clean_text()` - Normalize text (goodbye emojis 👋)
- `flag_non_english()` - Find foreign words using hunspell
- `filter_non_english_unigrams()` - Filter multilingual chaos

### Tokenization
- `tokenize_unigrams()` - Single words
- `tokenize_bigrams()` - Word pairs
- `tokenize_trigrams()` - Word triplets
- `tokenize_ngrams()` - Generic n-grams (flexible n)

### Frequency Analysis
- `freq_unigrams()` - Count single words
- `freq_bigrams()` - Count word pairs
- `freq_trigrams()` - Count word triplets
- `freq_ngrams()` - Generic n-gram frequencies
- `coverage_from_freq()` - Coverage analysis for vocabulary

### Language Modeling 🆕
- `build_cond_bigram()` - Compute P(w2 | w1) conditional probabilities with MLE
- `build_cond_trigram()` - Compute P(w3 | w1, w2) conditional probabilities with MLE
- `build_pruned_lang_model()` - Build complete pruned n-gram language model
- `prune_by_min_count()` - Remove rare n-grams by frequency threshold
- `prune_topN_per_history()` - Keep only top-N continuations per context

### Text Prediction 🆕
- `extract_last_tokens()` - Extract context words from user input
- `predict_next()` - Predict next word with Stupid Backoff algorithm

### Visualization
- `plot_top_terms()` - Bar plot of most frequent terms
- `plot_cumulative_coverage()` - Cumulative coverage curves
- `plot_rank_frequency()` - Zipf's law visualization (log-log plots)

## Requirements �

- R >= 3.6.0
- A pinch of patience
- Text data to analyze
- Coffee (optional but recommended)

### Dependencies

The package relies on giants like:
- `dplyr` & `tidyr` - For data manipulation
- `tidytext` - For tokenization
- `ggplot2` - For plots
- `stringr` - For string operations
- `hunspell` - For spell checking
- `magrittr` - For pipe operator (`%>%`)
- `parallel` - For multi-core file loading
- `readr` - For efficient file reading
- `here` - For path management
- `tibble` - For modern data frames

## Limitations 🚧

- Mainly tested on English corpora
- Optimized for SwiftKey datasets
- Documentation written in a hurry
- Might have bugs (feedback welcome!)
- I guarantee nothing 🤷‍♂️

## Performance ⚡

The package uses **parallel processing** strategically for I/O-bound operations:

- **`load_corpus()`** - Multi-core file reading (reads 3 files simultaneously)

By default, `load_corpus()` uses `detectCores() - 6` cores to leave plenty of resources for the system and other applications. You can control this with the `n_cores` parameter:

```r
# Use specific number of cores
corpus <- load_corpus("en_US", n_cores = 2)

# Force sequential processing (1 core)
corpus <- load_corpus("en_US", n_cores = 1)

# Auto-detect (default: all cores - 6)
corpus <- load_corpus("en_US")
```

**Note**: All text processing, tokenization, and frequency computation functions are fully sequential. This design choice ensures:
- ✅ Lower memory consumption
- ✅ Better performance for typical workloads
- ✅ No parallelization overhead
- ✅ Simpler debugging

## Contributing 🤝

PRs welcome, issues appreciated, suggestions valued. It's a personal project but if it can help others, all the better!

## License 📄

MIT License - do whatever you want, but don't blame me if something goes wrong.

## Acknowledgments 🙏

- Coursera Data Science Specialization for the inspiration
- Stack Overflow for the solutions
- Coffee for everything else

---
