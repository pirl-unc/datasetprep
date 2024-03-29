# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create_run_names
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Creates run names
#' 
#' @description 
#' Combines normal_tissue, analyte and file_prefix values into Run_Names
#' of format {a|n}{d|r|m|p}{unique part of file_prefix} where {a|n} refers
#' to normal or abnormal(tumor) tissue and {d|r|m|p} refers to analyte of sample.
#' 
#' @param normal_tissue Boolean vector indicating whether tissue was normal or tumor
#' @param analyte Character vector of analytes containing DNA, RNA, Methylation or Protein
#' @param file_prefix Unique file identifiers
#' @param shorten Boolean whether or not to reduce file_prefix to minimum unique length, defaults TRUE. If shorten_to is also set, that value overrides the "minimum unique length" determination
#' @param shorten_to Integer value indicating how many characters from the end of each file_prefix should be used. Defaults to NA in which case if shorten is true the minimum length is used for file_prefix where all values remain unique.
#' 
#' @return Vector of run names
#' 
#' @export
create_run_names <- function (normal_tissue, analyte, file_prefix, shorten = T, shorten_to = NA) 
{
  num_observations = length(normal_tissue)
  if (length(analyte) != num_observations | length(file_prefix) != 
      num_observations) {
    warning("Warning: for create_run_names to work properly, all parameters must be of the same length.")
    return()
  }
  max_nchar = max(nchar(file_prefix))
  if (length(unique(nchar(file_prefix))) > 1) {
    print("Normalizing file_prefix to same length.")
    file_prefix %<>% stringr::str_pad(width = max_nchar, 
                                      side = "left", pad = "_")
  }
  substrate_lut = c("d", "r", "m", "p")
  names(substrate_lut) = c("DNA", "RNA", "Methylation", "Protein")
  type_lut = c("a", "n")
  names(type_lut) = c("FALSE", "TRUE")
  s_index = 1
  e_index = max_nchar
  if (shorten) {
    if( !is.na(shorten_to) ){
      if( !all( nchar(file_prefix) >= shorten_to ) ) stop("Cannot shorten file_prefix values as requested because some are already shorter than shorten_to.")
      s_index <- e_index - shorten_to + 1
    }else{
      for (x in ((e_index) - 1):1) {
        if (length(unique(substr(file_prefix, x, e_index))) == 
            length(file_prefix)) {
          s_index = x
          break
        }
      }
    }
  }
  return(paste0(type_lut[as.character(normal_tissue)], substrate_lut[analyte], 
                "-", substr(file_prefix, s_index, e_index)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create_run_ids
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Creates run ids
#' 
#' @description 
#' Concatenates patient_ids with run_names.
#' 
#' @param patient_ids Vector of patient_ids to use
#' @param run_names Vector of run_names to use
#' 
#' @return Returns a vector of run_ids
#' 
#' @export
create_run_ids = function( patient_ids, run_names ){
  return(paste0(patient_ids, '-', run_names))
}