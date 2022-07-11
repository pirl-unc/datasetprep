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
#' 
#' @return Vector of run names
#' 
#' @export
create_run_names = function(normal_tissue, analyte, file_prefix){
  num_observations = length(normal_tissue)
  if( length(analyte) != num_observations | length(file_prefix) != num_observations ){
    warning("Warning: for create_run_names to work properly, all parameters must be of the same length.")
    return()
  }
  if( length(unique(nchar(file_prefix))) > 1 ){
    warning("Warning: for create_run_names to work properly, all file_prefix's must be of the same length.")
    return()
  }
  substrate_lut = c("d","r","m","p")
  names(substrate_lut) = c("DNA", "RNA", "Methylation", "Protein")
  
  type_lut = c("a","n")
  names(type_lut) = c("FALSE", "TRUE")
  s_index = 1
  e_index = nchar(file_prefix[1])
  for( x in ((e_index)-1):1 ){
    if( length(unique(substr(file_prefix, x, e_index))) == length(file_prefix) ){
      s_index = x
      break
    }
  }
  return(paste0( type_lut[as.character(normal_tissue)], substrate_lut[analyte],  "-", substr(file_prefix, s_index, e_index)))
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