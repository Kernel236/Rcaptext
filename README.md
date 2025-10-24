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

## Installation 🚀

```r
# From GitHub (the only place it lives)
# install.packages("devtools")  # if you don't have it already
devtools::install_github("Kernel236/rcaptext")
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
- `load_corpus()` - Load the famous SwiftKey files
- `sample_corpus()` - Take a representative chunk

### Text Cleaning
- `clean_text()` - Normalize text (goodbye emojis 👋)
- `flag_non_english()` - Find foreign words
- `filter_non_english_unigrams()` - Filter multilingual chaos

### Tokenization
- `tokenize_unigrams()` - Single words
- `tokenize_bigrams()` - Word pairs
- `tokenize_trigrams()` - Word triplets
- `tokenize_ngrams()` - Generic n-grams

### Frequency Analysis
- `freq_unigrams()` - Count words
- `freq_ngrams()` - Count n-grams
- `coverage_from_freq()` - Coverage analysis

### Language Modeling 🆕
- `build_cond_bigram()` - Compute P(w2 | w1) conditional probabilities
- `build_cond_trigram()` - Compute P(w3 | w1, w2) conditional probabilities
- `build_pruned_lang_model()` - Build complete pruned n-gram language model
- `prune_by_min_count()` - Remove rare n-grams
- `prune_topN_per_history()` - Keep only top-N continuations per context

### Text Prediction 🆕
- `extract_last_tokens()` - Extract context words from input
- `predict_next()` - Predict next word with Stupid Backoff algorithm

### Visualization
- `plot_top_terms()` - The most popular ones
- `plot_cumulative_coverage()` - Coverage curves
- `plot_rank_frequency()` - Zipf plots (for nerds)

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
- `stringr` - For strings
- `hunspell` - For spell checking
- `magrittr` - For pipe operator
- `parallel` - For faster loading (8 cores)

## Limitations 🚧

- Mainly tested on English corpora
- Optimized for SwiftKey datasets
- Documentation written in a hurry
- Might have bugs (feedback welcome!)
- I guarantee nothing 🤷‍♂️

## Contributing 🤝

PRs welcome, issues appreciated, suggestions valued. It's a personal project but if it can help others, all the better!

## License 📄

MIT License - do whatever you want, but don't blame me if something goes wrong.

## Acknowledgments 🙏

- Coursera Data Science Specialization for the inspiration
- Stack Overflow for the solutions
- Coffee for everything else

---
