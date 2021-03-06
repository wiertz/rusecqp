% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus_operations.R
\name{subset_corpus}
\alias{subset_corpus}
\title{Get corpus token positions restricted by structural attribute values}
\usage{
subset_corpus(cqp_corpus, sattr, sattr_values)
}
\arguments{
\item{sattr}{character specifying s-attribute.}

\item{sattr_values}{vector of s-attribute values.}

\item{corpus}{corpus created with \code{get_corpus}.}
}
\value{
list of class rusecqp_subcorpus with info on corpus name, subset
  positions, number of tokens.
}
\description{
\code{subset_corpus} retrieves cpos values for specified s-attribute values
in corpus. Currently, corpus subsets are only useful for calculating ngrams.
Other functions, such as query frequency list, provide their own parameters
for s-attribute restrictions.
}
\examples{
\dontrun{subset_corpus, "text_year", c(2015, 2016)}

}
