#' Load SwiftKey Text Corpus
#'
#' This function loads the 3 main text files (blogs, news, twitter)
#' for the selected language (default: en_US). It checks for file existence,
#' reads the content safely, normalizes encoding to UTF-8, and returns
#' a tidy tibble with one row per text line.
#'
#' @param lang Character. Language code prefix (e.g. "en_US", "de_DE"). Default is "en_US".
#' @param base_dir Character. Directory where raw data are stored. Default is "data/raw".
#' @param n_cores Integer. Number of CPU cores for parallel file reading.
#'   Default is NULL (uses parallel::detectCores() - 6 to preserve system resources).
#'   Set to 1 for sequential processing.
#'
#' @return A tibble with columns: 
#'   \describe{
#'     \item{source}{Character. Source type: "blogs", "news", or "twitter"}
#'     \item{text}{Character. The actual text content from each line}
#'   }
#'
#' @details
#' The function expects files to follow the standard SwiftKey naming convention:
#' \code{[lang].[source].txt} (e.g., "en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt").
#' 
#' Text encoding is normalized to UTF-8 to handle various input encodings safely.
#' Uses parallel processing to read the 3 files simultaneously, with explicit
#' garbage collection after reading to free memory immediately.
#'
#' @examples
#' \dontrun{
#' # Load English corpus
#' corpus <- load_corpus("en_US")
#' head(corpus)
#' 
#' # Load German corpus from custom directory
#' corpus_de <- load_corpus("de_DE", base_dir = "my_data")
#' }
#'
#' @export
#' @importFrom here here
#' @importFrom readr read_lines
#' @importFrom tibble tibble
#' @importFrom parallel makeCluster clusterEvalQ parLapply stopCluster detectCores
load_corpus <- function(lang = "en_US", base_dir = "data/raw", n_cores = NULL) {
  
  # Build file paths (standard SwiftKey filenames)
  f_blogs   <- here::here(base_dir, sprintf("%s.blogs.txt",   lang))
  f_news    <- here::here(base_dir, sprintf("%s.news.txt",    lang))
  f_twitter <- here::here(base_dir, sprintf("%s.twitter.txt", lang))
  
  # Collect paths in a named vector
  paths <- c(blogs = f_blogs, news = f_news, twitter = f_twitter)
  
  # --- Safety check: all files must exist ---
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(sprintf(
      "Missing files:\n- %s\nCheck 'base_dir' and file names (e.g., en_US.blogs.txt).",
      paste(missing, collapse = "\n- ")
    ))
  }
  
  # --- Informative messages for console log ---
  message(sprintf("• Loading corpus language: %s", lang))
  message(sprintf("  - blogs:   %s", f_blogs))
  message(sprintf("  - news:    %s", f_news))
  message(sprintf("  - twitter: %s", f_twitter))
  
  # Determine number of cores (conservative to avoid RAM saturation)
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 6)
  }
  message(sprintf("• Setting up parallel processing with %d cores...", n_cores))
  
  # --- Setup parallel processing ---
  cl <- parallel::makeCluster(n_cores)
  on.exit({
    parallel::stopCluster(cl)
    gc()  # Force garbage collection to free memory
  })
  
  # Load required packages on cluster nodes
  parallel::clusterEvalQ(cl, library(readr))
  
  # --- Read files in parallel ---
  message("• Reading files in parallel...")
  file_data <- parallel::parLapply(cl, paths, function(file_path) {
    content <- readr::read_lines(file_path, progress = FALSE)
    # Normalize encoding to UTF-8
    iconv(content, from = "", to = "UTF-8", sub = "byte")
  })
  
  # --- Extract results ---
  blogs   <- file_data[[1]]
  news    <- file_data[[2]]
  twitter <- file_data[[3]]
  
  # Free memory immediately after extraction
  rm(file_data)
  gc()
  
  # --- Combine all sources into a single tibble ---
  corpus <- tibble::tibble(
    source = c(rep("blogs",   length(blogs)),
               rep("news",    length(news)),
               rep("twitter", length(twitter))),
    text = c(blogs, news, twitter)
  )
  
  # Return tidy corpus
  return(corpus)
}

#' Sample Text Corpus by Source
#'
#' This function creates a random sample from the corpus, either by proportion
#' or by absolute number of lines per source (blogs/news/twitter).
#'
#' @param corpus A tibble with columns \code{source} and \code{text}, typically
#'   from \code{\link{load_corpus}}.
#' @param prop Numeric. Proportion to sample within each source (e.g., 0.05 for 5%).
#'   Use either \code{prop} OR \code{n_per_source}, not both.
#' @param n_per_source Integer. Number of lines to sample within each source
#'   (e.g., 50000). Use either \code{prop} OR \code{n_per_source}, not both.
#' @param seed Integer. Random seed for reproducibility. Default is 123.
#' @param min_chars Integer. Drop lines with fewer than this many characters 
#'   before sampling. Default is 1.
#'
#' @return A tibble with the same structure as input (\code{source}, \code{text}), 
#'   but sampled according to specified parameters.
#'
#' @details
#' The function filters out NA values and very short lines before sampling.
#' Sampling is performed within each source group to maintain representativeness
#' across different text types.
#'
#' @examples
#' \dontrun{
#' # Sample 5% from each source
#' small_corpus <- sample_corpus(corpus, prop = 0.05, seed = 123)
#' 
#' # Sample exactly 20,000 lines from each source
#' medium_corpus <- sample_corpus(corpus, n_per_source = 20000, seed = 42)
#' 
#' # Sample with minimum character filter
#' clean_sample <- sample_corpus(corpus, prop = 0.1, min_chars = 10, seed = 456)
#' }
#'
#' @export
#' @importFrom dplyr filter group_by slice_sample ungroup
sample_corpus <- function(corpus, prop = NULL, n_per_source = NULL, seed = 123, min_chars = 1) {
  stopifnot("source" %in% names(corpus), "text" %in% names(corpus))
  if (is.null(prop) == is.null(n_per_source)) {
    stop("Provide exactly one of `prop` or `n_per_source`.")
  }
  set.seed(seed)
  
  out <- corpus |>
    dplyr::filter(!is.na(text), nchar(text) >= min_chars) |>
    dplyr::group_by(source)
  
  out <- if (!is.null(prop)) {
    dplyr::slice_sample(out, prop = prop)
  } else {
    dplyr::slice_sample(out, n = n_per_source)
  }
  
  dplyr::ungroup(out)
}