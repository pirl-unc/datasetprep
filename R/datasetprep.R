#' datasetprep: Basic Helpers for Dataset Prep
#'
#' Methods to make life easier when preparing datasets for analysis.
#' 
#' @import magrittr
#' @importFrom dplyr mutate filter slice
#' @importFrom stringr str_split str_to_title
#' @importFrom readr read_tsv
#' @importFrom housekeeping "%ni%"
#' 
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

utils::globalVariables(c("readme_path"))