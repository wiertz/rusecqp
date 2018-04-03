#' Query corpus
#'
#' \code{query_corpus} performs a cqp_query and returns a list of position
#' values. If s_attr and s_attr_values are provided, restricts query to
#' structural attributes with given values.
#'
#' @param corpus corpus created with \code{get_corpus}.
#' @param cqp_query character with cqp query string.
#' @param sattr character specifying s-attribute.
#' @param sattr_values vector of s-attribute values.
#' @return list of class query_result with match and matchend position values.
#'
#' @examples \dontrun{query_corpus(my_corpus, '[word = "crisis"]')}
#'
#' @export
query_corpus <- function(cqp_corpus, cqp_query, sattr = NULL, sattr_values = NULL) {
  stopifnot(inherits(cqp_corpus, "rusecqp_corpus"))
  if(is.character(sattr) && !is.null(sattr_values)) {
    cqp_query <- paste0(cqp_query,
                               " :: match.", sattr, "='",
                               paste0(sattr_values),
                               "'")
  }
  result <- lapply(cqp_query, function(q) {
    print(paste0("performing query ", q, " on corpus ", cqp_corpus$name))
    rcqp::cqi_query(cqp_corpus$name, "RusecqpQuery", q)
    cqp_result <- rcqp::cqi_dump_subcorpus(paste0(cqp_corpus$name, ":RusecqpQuery"))
    cqp_result <- tk_range2pos(cqp_result[,1], cqp_result[,2])
    rcqp::cqi_drop_subcorpus(paste0(cqp_corpus$name, ":RusecqpQuery"))
    cqp_result
  })
  result <- unlist(result, recursive = F)
  query_result <- list(match = result, corpus = cqp_corpus)
  class(query_result) <- append(class(query_result), "query_result")
  query_result
}

#' Frequency breakdown of query results
#'
#' \code{q_frequency_breakdown} returns a frequency list of query results.
#'
#' @param query_result list of class query result returned from
#'   \code{query_corpus}.
#' @param pattr character of positional attribute (e.g. word)
#' @return data.table of frequencies.
#'
#' @examples \dontrun{q_frequency_breakdown(my_query_result, pattr = "lemma")}
#'
#' @export
q_frequency_breakdown  <- function(query_result, pattr = "word") {
  stopifnot(inherits(query_result, "query_result"))
  token_ids <- tk_pos2id(query_result$match, pattr, query_result$corpus)
  strings <- tk_id2str(token_ids, pattr, query_result$corpus)
  strings <- lapply(strings, stringr::str_c, sep = " ")
  strings_table <- data.table(QUERY_RESULT = unlist(strings))
  strings_table <- strings_table[, .(N = .N), by = QUERY_RESULT]
  strings_table <- strings_table[, PERCENT := N / length(query_result$match) * 100]
  strings_table[order(-N)]
}

#' Frequency distribution of query results
#'
#' \code{q_frequency_breakdown} returns number of query matches in each
#' category of the structural attribute provided.
#'
#' @param query_result list of class query result returned from
#'   \code{query_corpus}.
#' @param sattr character of structural attribute (e.g. "text_year")
#' @return frequency distribution as data.table of frequencies.
#'
#' @examples \dontrun{q_distribution(my_query_result, "text_year")}
#'
#' @export
q_distribution <- function(query_result, sattr) {
  force(query_result)
  stopifnot(inherits(query_result, "query_result"))
  stopifnot(sattr %in% query_result$corpus$sattribs)

  sattr_categories <- sattr_categories(sattr, query_result$corpus)
  result_categories <- lapply(query_result$match, function(x) {
    tk_pos2sattr(x[1], query_result$corpus, sattr)
  })
  distrib <- data.table(CATEGORY = unlist(result_categories))
  distrib <- distrib[, .(N = .N), by = CATEGORY][order(CATEGORY)]
  distrib <- distrib[sattr_categories, on = "CATEGORY"]
  setnames(distrib, c("N", "i.N"), c("HITS", "N"))
  distrib[is.na(HITS), HITS := 0]
  distrib[, FREQ := HITS / N]
  setcolorder(distrib, c("CATEGORY", "N", "HITS", "FREQ"))
  distrib[]
}

#' Collocations to query match
#'
#' \code{q_collocations} returns collocators of a query match. If query match is
#' longer than one token, the context is build around the entire length of the
#' query match. E.g. if \code{context = 3}, three tokens before the first token
#' of the match and three tokens after the last token of the match are taken
#' into account.
#'
#' @param query_result list of class query result returned from
#'   \code{query_corpus}.
#' @param context integer vector of length one: number of tokens around the
#'   query match. integer vector of length two: number of tokens to the left and
#'   right. character: structural attribute (e.g. "s").
#' @param pattr character specifying positional attribute (e.g. "word").
#' @param include_match logical indicating wether the matched tokens themselves
#'   should be considered in the analysis.
#' @return data.table of collocators and frequencies.
#'
#' @examples \dontrun{q_collocations(my_query_result, context = 3)}
#'   \dontrun{q_collocations(my_query_result, context = c(2, 0))}
#'   \dontrun{q_collocations(my_query_result, context = "s")}
#'
#'
q_collocations <- function(query_result, context, pattr = "word", exclude_match = T) {
  match_positions <- unlist(query_result$match)
  match_positions_count <- length(match_positions)
  if(is.numeric(context)) {
    left_context <- context[1]
    right_context <- ifelse(length(context) == 2, context[2], context[1])
    positions <- unlist(
      lapply(query_result$match, function(x) {
      (x[1] - left_context):(x[length(x)] + right_context)
      })
    )
  } else if(context %in% query_result$corpus$sattribs) {
    sattrib_string <- paste0(query_result$corpus$name, ".", context)
    left_boundaries <- rcqp::cqi_cpos2lbound(sattrib_string, match_positions)
    right_boundaries <- rcqp::cqi_cpos2rbound(sattrib_string, match_positions)
    positions <- unique(unlist(Map(seq, left_boundaries, right_boundaries)))
  } else {
    stop("Invalid context. Provide numeric window size or character of structural attribute.")
  }
  if(exclude_match) positions <- positions[!(positions %in% match_positions)]
  collocations <- tk_pos2freq(positions, query_result$corpus, pattr = pattr)
  collocations[, R := N / length(positions)]
  corpus_flist <- frequency_list(query_result$corpus, pattr = pattr)
  collocation_stats <- keywords(collocations, corpus_flist, A_is_subset = F, min_A = 1, min_B = 1)
  names(collocation_stats) <- c("TOKEN", "N.COLLOC", "N.CORPUS", "R.COLLOC", "R.CORPUS", "LOGLIK", "LOGRAT")
  collocation_stats[order(-LOGLIK)]
}
