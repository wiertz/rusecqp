
# corpus tools
list_corpora <- function() {
  corpus_list <- rcqp::cqi_list_corpora()
  corpus_list <- corpus_list[!stringr::str_detect(corpus_list, "__FREQ")]
  corpus_list[order(corpus_list)]
}
cp_count_token <- function(cqp_corpus) {
  p_attr <- rcqp::cqi_attributes(cqp_corpus, "p")[[1]]
  cs <- rcqp::cqi_attribute_size(paste0(cqp_corpus, ".", p_attr))
}

cp_get_sattribs <- function(cqp_corpus) {
  s_attribs <- rcqp::cqi_attributes(cqp_corpus, "s")
  s_attribs_size <- vapply(s_attribs, function(a) {
    rcqp::cqi_attribute_size(paste0(cqp_corpus, ".", a))
  }, 0L)
  data.table(s_attribute = names(s_attribs_size), n_regions = s_attribs_size)
}
cp_get_pattribs <- function(cqp_corpus) {
  rcqp::cqi_attributes(cqp_corpus, "p")
}

# s-attribute tools
.sattr_categories <- function(sattr, corpus) {
  regions <- .sattr_regions(sattr, corpus)
  regions <- regions[, sum(N), by = CATEGORY][order(CATEGORY)]
  setnames(regions, "V1", "N")
  regions
}

.sattr_regions <- function(sattr, corpus) {
  sattr_string <- paste0(corpus$name, ".", sattr)
  n_regions <- rcqp::cqi_attribute_size(sattr_string)
  region_ids <- 0:(n_regions - 1)
  region_value <- as.character(rcqp::cqi_struc2str(sattr_string, region_ids))
  region_size <- vapply(region_ids, function(r) {
    from_to_pos <- rcqp::cqi_struc2cpos(sattr_string, r)
    n <- as.integer(from_to_pos[2] - from_to_pos[1] + 1)
  }, 0L)
  data.table(ID = region_ids, CATEGORY = region_value, N = region_size)
}

# ngram tools
.id_ngrams <- function(pos_from, pos_to, ignore_punct, pattr_string, ngram_length) {
  punct <- c(".", "!", "?", ",", ";", ":", '"', "(", ")", "-")
  punct_id <- rcqp::cqi_str2id(pattr_string, punct)
  id_cols <- paste0("V", 1:ngram_length)
  ids <- data.table(rcqp::cqi_cpos2id(pattr_string, pos_from:pos_to))
  if(ignore_punct) ids <- ids[!(V1 %in% punct_id)]
  ids[, (id_cols) := shift(V1, n = 1:ngram_length, type = "lead")]
  ids[, .N, by = id_cols]
}

# token tools
.tk_range2pos <- function(begin_pos, end_pos) {
  ## Takes two vectors with begin/end positions of equal length and returns all positions
  stopifnot(length(begin_pos) == length(end_pos))
  ranges <- Map(seq, begin_pos, end_pos)
}

.tk_pos2id <- function(tk_positions, pattr, cqp_corpus) {
  ## Takes any kind of list containing token positions as integers and recursively
  ## replaces all integers by token ids based on corpus pattr
  cp_pattr <- paste0(cqp_corpus$name, ".", pattr)
  ids <- rapply(tk_positions, function(pos) rcqp::cqi_cpos2id(cp_pattr, pos),
                how = "replace",
                classes = "integer")
}

.tk_id2str <- function(tk_ids, pattr, cqp_corpus) {
  ## Takes any kind of list containing token ids as integers and recursively
  ## replaces all integers by token string based on corpus pattr
  cp_pattr <- paste0(cqp_corpus$name, ".", pattr)
  tk_str <- rapply(tk_ids, function(idx) rcqp::cqi_id2str(cp_pattr, idx),
                   how = "replace",
                   classes = "integer")
  tk_str <- rapply(tk_str, function(s) paste(s, collapse = " "),
                   how = "replace",
                   classes = "character")
}

.tk_pos2sattr <- function(tk_pos, cqp_corpus, sattr) {
  cp_sattr <- paste0(cqp_corpus$name, ".", sattr)
  sattr_id <- rcqp::cqi_cpos2struc(cp_sattr, tk_pos)
  sattr_value <- rcqp::cqi_struc2str(cp_sattr, sattr_id)
}

.tk_pos2freqdist <- function(tk_pos, cqp_corpus, pattr) {
  pattr_string <- paste0(cqp_corpus$name, ".", pattr)
  d <- data.table(token = rcqp::cqi_cpos2id(pattr_string, tk_pos))
  d <- d[, .(N = .N), by = token][order(-N)]
  d[, R := N / (cqp_corpus$token_count)]
  d <- d[, token := rcqp::cqi_id2str(pattr_string, token)]
}
