# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert_clm_case
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Takes an input_string and puts it in title format with underscores.
#' 
#' @param input_string 
#' 
#' @return Returns dataframe with updated and added *_Rx and *_Tx columns
#' 
#' @export
convert_clm_case <- function(input_strings) {
  sapply(input_strings, function(input_string) {
    words <- tolower(unlist(strsplit(input_string, "_")))
    words <- sapply(words, function(word) {
      paste0(toupper(substring(word, 1, 1)), substring(word, 2))
    })
    paste(words, collapse = "_")
  }, USE.NAMES = FALSE)
}