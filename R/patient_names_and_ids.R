# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format_patient_names
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Formats patient names
#' 
#' @description 
#' Data's original patient identifiers are often more complex than we want. 
#' This method takes those names ( within a limited subset of formats ) 
#' and reformats them as "p001" where the number portion is unique across patients.
#' 
#' @param old_names Vector of patient names to format
#' @param reduce_size Boolean to indicate whether numeric portion of ids should be reduced to shortest length representing unique values
#' 
#' @return Returns the updated patient names vector
#' 
#' @section Limitations:
#' \itemize{
#'   \item Only works properly if old names have unique numerical identifiers in them
#'   i.e. Patient1, Patient2, etc.
#'   \item Only works if old names have a single numerical element within them
#'   i.e wouldn't work with identifiers like MD-27-AR-0215
#' }
#' 
#' @export
format_patient_names = function( old_names, reduce_size=FALSE ){
  num_unique = length(unique(old_names))
  #remove any non numeric characters from before and after numeric characters
  if(all(grepl("^[^0-9]+[0-9]+$|^[0-9]+$", old_names)) & length(unique(old_names)) == length(unique(gsub("[^0-9]", "", old_names)))) new_names = gsub( "[^0-9]", "", old_names )
  else new_names = old_names
  max_nchar = max(nchar(new_names))
  #str_pad is vectorized so no need for sapply
  if(any(grepl("^[^0-9]", new_names))) new_names %<>% stringr::str_pad(width=max_nchar, side="right", pad="0")
  else new_names %<>% stringr::str_pad(width=max_nchar, side="left", pad="0")
  if(reduce_size){
    #find shortest number that is unique for all values
    e_index = max_nchar
    s_index = 1
    unique_names = length(unique(new_names))
    for( ind in (e_index-1):s_index ){
       if( unique_names == length(unique(substr(new_names, ind, e_index)))){
        s_index = ind
        break
      }
    }
    new_names = substr(new_names, s_index, e_index)
  }
  if(!all(grepl("^[p]",new_names))) new_names = paste0( "p", new_names )
  if(num_unique != length(unique(new_names))){
    warning("Failed to preserve unique names. Names must be formatted manually.")
    return(old_names)
  }
  return(new_names)
}

#d = format_patient_names(c("p1", "p1a2", "p3", "p1"), FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create_patient_ids
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Creates patient ids
#' 
#' @description 
#' Concatenates dataset with patient_names. 
#' 
#' @param dataset Name of dataset to use with these ids
#' @param patient_names Vector of patient names to use in creation of ids
#' @return Returns a vector of patient_ids
#' 
#' @export
create_patient_ids = function( dataset, patient_names ){
  return(paste0(dataset, "-", patient_names))
}