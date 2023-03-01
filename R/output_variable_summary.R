# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# output_variable_summary
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Outputs summary of variables in data.frame
#' 
#' @description 
#' In order to confirm data being generated through the prep phase, 
#' it can be helpful to output a summary of variables in a data.frame. 
#' This method takes a dataframe and does just that, outputting the summary to the console.
#' 
#' @param dat Data.frame from which to output summary
#' @param column_names Vector of column names to summarize. Defaults to all colnames
#' @param max_length Integer of maximum unique values to display for a given column
#' @param title Optional string to display as label for data being output
#' 
#' @export
output_variable_summary = function(dat=NULL, column_names=NULL, max_length=10, title=NULL){
  #validate and set default values for parameters
  if( is.null(dat)){
    warning( "ERROR: output_variable_summary requires a data.frame parameter to enumerate")
    return;
  }
  if(is.null(column_names)) column_names = colnames(dat)
  if(!is.null(title)){
    cat(paste("Variable Summary for", title,"\n"))
  	cat("------------------------------------------\n\n")
  }
  for(this_name in column_names){
  	cat(paste0(this_name,"\n"))
    if(!(this_name %in% colnames(dat))){
      cat("Variable does not exist in data.\n\n")
      next
    }
    #only output summaries for variables with less than max_length values
    if(length(unique(dat[[this_name]])) <= max_length){
      my_summary = summary(factor(dat[[this_name]]))
      my_out = ""
      for (findex in 1:length(my_summary)){
        my_out = paste0(my_out, names(my_summary)[findex], ":", my_summary[findex], "; ")
      }
      my_out = gsub("; $", "", my_out)
      my_out = gsub("TRUE", "T", my_out)
      my_out = gsub("FALSE", "F", my_out)
      my_out = gsub("Complete Response", "CR", my_out)
      my_out = gsub("Partial Response", "PR", my_out)
      my_out = gsub("Stable Disease", "SD", my_out)
      my_out = gsub("Progressive Disease", "PD", my_out)
     cat(paste0(my_out),"\n")
    }else{
     cat(paste0("more than ", max_length, " values\n"))
     cat(paste0("example value: ", dat[[this_name]][!is.na(dat[[this_name]])][1],"\n"))
    }
   cat("\n")
  }
}
