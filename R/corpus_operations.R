#' List available corpora
#'
#' Lists corpora available through the Corpus Workbench. Make sure to set the
#' correct path to the corpus workbench registry directory before loading rcqp
#' or rusecqp (\code{Sys.setenv(CORPUS_REGISTRY = path_to_reg)}).
#'
#' @examples \dontrun{list_corpora()}
#' @export
list_corpora <- function() {
  corpus_list <- rcqp::cqi_list_corpora()
  corpus_list <- corpus_list[!stringr::str_detect(corpus_list, "__FREQ")]
  corpus_list[order(corpus_list)]
}

#' Get information on existing Corpus Workbench corpus
#'
#' \code{get_corpus} retrieves corpus information from an existing CWB corpus.
#'
#' @param corpus_name character of CWB corpus name (will be converted to uppercase).
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
  corpus_sattr <- sattr_regions(sattr, corpus)
  if(!is.null(sattr_values)) {
    corpus_sattr <- corpus_sattr[CATEGORY %in% sattr_values]
  }
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
#' for the specified corpus or corpus subset.
#'
#' @param corpus corpus created with \code{get_corpus} or corpus subset created
#'   with \code{subset_corpus}.
#' @param pattr string specifying positional attribute (e.g. word or lemma).

#' @return data.table with tokens, absolute, and relative frequencies.
#' @examples \dontrun{frequency_list, pattr = "lemma", sattr = "text_year", sattr_values = c(2015, 2016)}
#'
#' @export
frequency_list <- function(corpus, pattr = "word") {
  pattr_string <- paste0(corpus$name, ".", pattr)
  positions <- unlist(lapply(corpus$cpos, function(p) seq(p[1], p[2])))
  token_fd <- tk_pos2freq(positions, corpus, pattr)
  token_fd[]
}

#' Calcualte ngrams
#'
#' \code{ngrams} calculates ngrams for a corpus or subcorpus. Ngram calculation
#' uses a lot of memory and may fail for large corpora or subcorpora or causing
#' the system to hang. For computational efficiency, results are not cleaned and
#' may contain some artifacts (i.e. ngrams with less than the specified token
#' count).
#'
#' @param corpus corpus created with \code{get_corpus}.
#' @param ngram_length numeric. Length of ngrams.
#' @param pattr character. Positional attribute to use for ngram calculation.
#' @param ignore_punct logical. If TRUE, punctuation is ignored in ngram
#'   calculation.
#'
#' @return data.table with ngrams and frequencies.
#' @examples \dontrun{trigrams <- ngrams(my_corpus, 3, min_count = 10)}
#'
#' @export
ngrams <- function(corpus, ngram_length, pattr = "word", ignore_punct = T) {
  pattr_string <- paste0(corpus$name, ".", pattr)
  dt_columns <- paste0("V", 1:ngram_length)
  if(ignore_punct) {
    punct_tokens <- c(".", "!", "?", ",", ";", ":", '"', "(", ")", "-")
    punct_ids <- rcqp::cqi_str2id(pattr_string, punct_tokens)
  }
  ng <- rbindlist(
    lapply(corpus$cpos, function(p) {
      ids <- data.table(rcqp::cqi_cpos2id(pattr_string, p[1]:p[2]))
      if(ignore_punct) ids <- ids[!(V1 %in% punct_ids)]
      ids[, (dt_columns) := shift(V1, n = 1:ngram_length, type = "lead")]
      ids[, .N, by = dt_columns]
    })
  )
  ng <- ng[, .(N = sum(N)), by = dt_columns]
  ng[, NGRAM := apply(
    ng, 1, function(r) {
      stringr::str_c(rcqp::cqi_id2str(pattr_string, r[1:ngram_length]), collapse = " ")
    }
  )]
  ng[, (dt_columns) := NULL][order(-N)]
}
