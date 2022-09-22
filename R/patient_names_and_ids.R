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
#' @param min_chars Integer minimum number of characters to keep in numeric portion of id's ( final lengths will be length(prefix)+min_char because prefix is prepended to numeric portion )
#' @param prefix Character value to prepend onto numeric portions of reformatted ids - defaults to "p"
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
format_patient_names = function( old_names, reduce_size=FALSE, min_chars=1, prefix="p" ){
  bad_format_indices <- which(grepl("^[^0-9]+[0-9]+$|^[0-9]+$", old_names) == FALSE )
  if( length(bad_format_indices) > 0 ) stop( paste("Patient names like", old_names[bad_format_indices[1]], "can not be reformatted with this method. Accepted formats are all numeric OR one or more non-numeric characters followed by one or more numeric characters." ) )
  #remove any leading numeric characters so long as same uniqueness will be preserved
  if(length(unique(old_names)) == length(unique(gsub("[^0-9]", "", old_names)))){
    old_names = gsub( "[^0-9]", "", old_names )
  }else{
    warning("The numeric portions of all names must be unique to be reformatted with this method - returning original patient names." )
    return(old_names)
  } 
  #pad names with leading "0's" to make them all the same length
  new_width <- max(c(nchar(old_names), min_chars))
  old_names %<>% stringr::str_pad(width=new_width, side="left", pad="0")
  #if we want only the fewest characters that maintain uniqueness
  if(reduce_size){
    #find shortest number that is unique for all values
    e_index = nchar(old_names[1])
    unique_names = length(unique(old_names))
    for( ind in (e_index-max(1, min_chars-1)):1 ){
      new_names <- substr(old_names, ind, e_index)
       if( unique_names == length(unique(new_names))){
        old_names = new_names
        break
      }
    }
  }
  #put a "p" ( or whatever was sent as prefix ) in front of the numeric characters which should all be the same length now
  old_names = paste0( prefix, old_names )
  return(old_names)
}

######### format_patient_names method tests follow #########
#test for non-formattable patient names
#format_patient_names(c("p1", "p1a2", "p3", "p1"), FALSE)
#test for non-unique numeric portions
#format_patient_names(c("p1", "p12", "p3", "p1"), FALSE)
#test for formattable names of various lengths with no reducing
#format_patient_names(c("p1", "p12", "p3", "p1"), FALSE)
#test for formattable names of various lengths with reducing
#format_patient_names(c("p10", "p12", "p3", "p1"), FALSE, min_chars=3)
#format_patient_names(c("p10", "p12", "p3", "p1"), TRUE, min_chars=3)
#format_patient_names(c("dda490", "qv1240", "p3510", "p1276"), TRUE, min_chars=3)

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
#' 
#' @return Returns a vector of patient_ids
#' 
#' @export
create_patient_ids = function( dataset, patient_names ){
  return(paste0(dataset, "-", patient_names))
}