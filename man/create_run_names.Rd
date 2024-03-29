% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_names_and_ids.R
\name{create_run_names}
\alias{create_run_names}
\title{Creates run names}
\usage{
create_run_names(
  normal_tissue,
  analyte,
  file_prefix,
  shorten = T,
  shorten_to = NA
)
}
\arguments{
\item{normal_tissue}{Boolean vector indicating whether tissue was normal or tumor}

\item{analyte}{Character vector of analytes containing DNA, RNA, Methylation or Protein}

\item{file_prefix}{Unique file identifiers}

\item{shorten}{Boolean whether or not to reduce file_prefix to minimum unique length, defaults TRUE. If shorten_to is also set, that value overrides the "minimum unique length" determination}

\item{shorten_to}{Integer value indicating how many characters from the end of each file_prefix should be used. Defaults to NA in which case if shorten is true the minimum length is used for file_prefix where all values remain unique.}
}
\value{
Vector of run names
}
\description{
Combines normal_tissue, analyte and file_prefix values into Run_Names
of format {a|n}{d|r|m|p}{unique part of file_prefix} where {a|n} refers
to normal or abnormal(tumor) tissue and {d|r|m|p} refers to analyte of sample.
}
