# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format_sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Formats sex values
#' 
#' @description 
#' Converts data's original sex characters to standard Female / Male.
#' 
#' @param sex Vector of sex values
#' @param female_char Character used in sex to represent Female, defaults to F
#' @param male_char Character used in sex to represent Male, defaults to M
#' 
#' @return Returns the updated sex vector
#' 
#' @export
format_sex = function(sex, female_char = "F", male_char = "M"){
  sex[sex == female_char] = "Female"
  sex[sex == male_char] = "Male"
  sex = stringr::str_to_sentence(sex)
  return(sex)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set_race_fields
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Formats race values and sets individual race fields
#' 
#' @description 
#' Takes data's original race values and cleans them up for consistency, 
#' converts unknown races to Other, 
#' and sets the individual boolean race fields ( Caucasian, Asian, etc. ) based on updated Race.
#' 
#' @param dat Dataframe containing race values
#' @param column_name Name of column containing race values, defaults to Race
#' 
#' @return Returns dat with updated and added columns
#' 
#' @export
set_race_fields = function(dat, column_name="Race"){
  dat$Race = stringr::str_to_title(dat[[column_name]])
  dat$Race[dat$Race %like% "White"] = "Caucasian"
  dat$Race[dat$Race %like% "Black"] = "African"
  options = c("Caucasian", "Asian", "African", "Nat_American", "Pac_Islander", "Other")
  others = levels(factor(dat$Race[dat$Race %ni% options]))
  if( length(others) > 0 ){
    warning("Warning :: following Race values converted to 'Other':: ", paste(others, collapse=", "))
    dat$Race[dat$Race %ni% options] = "Other"  
  }
  options = options[1:5]
  for(x in options){
    dat[[x]] = dat$Race == x
  }
  return(dat)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set_response_data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Formats response data and sets associated fields
#' 
#' @description 
#' This method standardizes the Best Response data to full names in CamelCase 
#' and then sets fields that are dependent on those values ( Progression, 
#' Clinical_Benefit and Responder ).
#' 
#' @param dat Dataframe containing response values
#' @param response_col Name of column containing Response values
#' 
#' @section Limitations:
#' \itemize{
#'   \item Only works for known set of responses ( PD, SD, PR, CR )
#'}
#' 
#' @return Returns dat with updated and added columns
#' 
#' @export
set_response_data = function( dat, response_col = "Response" ){
  #Normalize abbreviated responses, if there are any, while also normalizing full name values if those already exist
  response_data = stringr::str_to_sentence(dat[[response_col]])
  dat$Response = response_data
  if(any(response_data %in% c("Pd", "Sd", "Pr", "Cr"))){
    dat$Response = NA
    dat$Response[response_data == "Pd"] = "Progressive Disease"
    dat$Response[response_data == "Sd"] = "Stable Disease"
    dat$Response[response_data == "Pr"] = "Partial Response"
    dat$Response[response_data == "Cr"] = "Complete Response"
  }
  
  dat$Progression = NA
  dat$Progression= dat$Response == "Progressive Disease"
  dat$Clinical_Benefit = !dat$Progression
  
  dat$Responder[dat$Response %in% c("Complete Response", "Partial Response")] = TRUE
  dat$Responder[dat$Response %in% c("Stable Disease", "Progressive Disease")] = FALSE
  
  return(dat)
}