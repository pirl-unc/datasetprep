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
convert_clm_case <- function(input_string) {
  # Convert the input string to lower case and split by '_'
  words <- tolower(unlist(strsplit(input_string, "_")))
  # Capitalize the first letter of each word
  words <- sapply(words, function(word) {
    paste0(toupper(substring(word, 1, 1)), substring(word, 2))
  })
  # Combine the words back into snake case
  result <- paste(words, collapse = "_")
  return(result)
}