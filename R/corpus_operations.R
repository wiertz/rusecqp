#' Get information on existing Corpus Workbench corpus
#'
#' \code{get_corpus} retrieves corpus information from an existing CWB corpus.
#' \code{corpus_name} is converted to uppercase.
#'
#' @param corpus_name string of CWB corpus name.
#' @return list with class attribute "rusecqp_corpus" containing  corpus information.
#' @examples \dontrun{get_corpus("MY_CORPUS")}
#' @export
get_corpus <- function(corpus_name) {
  corpus_name <- toupper(corpus_name)
  corpora <- list_corpora()
  stopifnot(corpus_name %in% corpora)
  corpus <- list()
  corpus$name <- corpus_name
  corpus$sattribs <- rcqp::cqi_attributes(corpus_name, "s")
  corpus$pattribs <- cp_get_pattribs(corpus_name)
  corpus$token_count <- cp_count_token(corpus_name)
  corpus$cpos <- list(c(0, corpus$token_count - 1))
  class(corpus) <- append(class(corpus), "rusecqp_corpus")
  corpus
}

#' Get corpus token positions restricted by structural attribute values
#'
#' \code{subset_corpus} retrieves cpos values for specified s-attribute values
#' in corpus. Currently, corpus subsets are only useful for calculating ngrams.
#' Other functions, such as query frequency list, provide their own parameters
#' for s-attribute restrictions.
#'
#' @param corpus corpus created with \code{get_corpus}.
#' @param sattr character specifying s-attribute.
#' @param sattr_values vector of s-attribute values.
#' @return list of class rusecqp_subcorpus with info on corpus name, subset
#'   positions, number of tokens.
#' @examples \dontrun{subset_corpus, "text_year", c(2015, 2016)}
#'
#' @export
subset_corpus <- function(corpus, sattr, sattr_values) {
  sattr_string <- paste0(corpus$name, ".", sattr)
  corpus_sattr <- .sattr_regions(sattr, corpus)
  if(!is.null(sattr_values)) {
    corpus_sattr <- corpus_sattr[CATEGORY %in% sattr_values]
  }
  #cat("Calculating", pattr, "frequencies for s-attribute", sattr, "with values", unique(corpus_sattr$VALUE))
  positions <- lapply(corpus_sattr$ID, function(x) {
    from_to_pos <- rcqp::cqi_struc2cpos(sattr_string, x)
  })
  subcorpus <- list()
  class(subcorpus) <- append(class(subcorpus), "rusecqp_subcorpus")
  subcorpus$name <- corpus$name
  subcorpus$cpos <- positions
  subcorpus$token_count <- sum(vapply(positions, function(p) {
    p[2] - p[1] + 1
  }, 0))
  subcorpus
}

#' Calculate token frequency list
#'
#' \code{frequency_list} calculates token (e.g. word or lemma) frequency list
#' for the specified corpus. If a structural attribute and attribute values are
#' provided, frequencies are restricted to the subset of the corpus.
#'
#' @param corpus corpus created with \code{get_corpus}.
#' @param pattr string specifying positional attribute (e.g. word or lemma).
#' @param sattr character specifying s-attribute.
#' @param sattr_values vector of s-attribute values.

#' @return data.table with tokens, absolute, and relative frequencies.
#' @examples \dontrun{frequency_list, pattr = "lemma", sattr = "text_year", sattr_values = c(2015, 2016)}
#'
#' @export
frequency_list <- function(corpus, pattr = "word", sattr = NULL, sattr_values = NULL) {
  if(is.null(sattr)) {
    all_tokens <- 0:(corpus$token_count - 1)
    cat("Calculating", pattr, "frequencies for whole corpus")
    .tk_pos2freqdist(all_tokens, corpus, pattr)
  } else {
    sattr_string <- paste0(corpus$name, ".", sattr)
    corpus_sattr <- .sattr_regions(sattr, corpus)
    if(!is.null(sattr_values)) {
      corpus_sattr <- corpus_sattr[CATEGORY %in% sattr_values]
    }
    cat("Calculating", pattr, "frequencies for s-attribute", sattr, "with values", sattr_values)
    positions <- lapply(corpus_sattr$ID, function(x) {
      category_range <- rcqp::cqi_struc2cpos(sattr_string, x)
      category_positions <- .tk_range2pos(pos)
    })
    positions <- unique(unlist(positions))
    r <- .tk_pos2freqdist(positions, corpus, pattr)
  }
  r[]
}

#' Calcualte ngrams
#'
#' \code{ngrams} calculates ngrams for a corpus or subcorpus. Ngram calculation
#' uses a lot of memory and may fail for large corpora or subcorpora or causing
#' the system to hang. On a machine with 8 GB of available memory, corpora of
#' about 50 Mio token can be processed.
#'
#' @param corpus corpus created with \code{get_corpus}.
#' @param ngram_length numeric. Length of ngrams.
#' @param min_count numeric. Minimum frequency of ngrams to return.
#' @param pattr character. Positional attribute to use for ngram calculation.
#' @param ignore_punct logical. If TRUE, punctuation is ignored in ngram calculation.
#'
#' @return data.table with ngrams and frequencies.
#' @examples \dontrun{trigrams <- ngrams(my_corpus, 3, min_count = 10)}
#'
#' @export
ngrams <- function(corpus, ngram_length, min_count, pattr = "word", ignore_punct = T) {
  pattr_string <- paste0(corpus$name, ".", pattr)
  ng <- rbindlist(
    lapply(corpus$cpos, function(x) {
      .id_ngrams(x[1], x[2], ignore_punct, pattr_string, ngram_length)
    })
  )
  id_cols <- paste0("V", 1:ngram_length)
  ng <- ng[, .(N = sum(N)), by = id_cols]
  ng[, NGRAM := apply(
    ng, 1, function(r) {
      stringr::str_cc(rcqp::cqi_id2str(pattr_string, r[1:ngram_length]), collapse = " ")
    }
  )]
  ng[, (id_cols) := NULL][order(-N)][N >= min_count]
}
