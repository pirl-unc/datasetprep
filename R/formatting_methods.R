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
#' @param column_name Name of column containing race values, defaults to Race, if NA intializes Race as all NA
#' @param na_to_lc Boolean dictating whether to set any na's to Likely Caucasian
#' 
#' @return Returns dat with updated and added columns
#' 
#' @export
set_race_fields = function(dat, column_name="Race", na_to_lc=FALSE){
  dat$Race = ifelse(is.na(column_name) | column_name %ni% names(dat), NA, str_to_title(dat[[column_name]]))
  #override quirk in str_to_title where _ is not a word separator ...
  dat$Race[ dat$Race == "Pac_islander" ] = "Pac_Islander"
  dat$Race[ dat$Race == "Nat_american" ] = "Nat_American"
  dat$Race[dat$Race %like% "White"] = "Caucasian"
  #convert Black and African-American to African
  dat$Race[dat$Race %like% "Black" | dat$Race %like% "African"] = "African"
  options = c(NA, "Caucasian", "Asian", "African", "Nat_American", "Pac_Islander", "Other")
  others = levels(factor(dat$Race[dat$Race %ni% options]))
  if( length(others) > 0 ){
    warning("Warning :: following Race values converted to 'Other':: ", paste(others, collapse=", "))
    dat$Race[dat$Race %ni% options] = "Other"  
  }
  options = options[-c(1,length(options))]
  # We want specific race fields to be TRUE if matches Race field value
  #       FALSE if we know they can't be the given Race ( because they are a different known race, excluding Other )
  #       Likely_Caucasian where not a non-caucasian known race or where Other and there are no known Caucasians 
  #           AND na_to_lc is TRUE
  #       NA where data is NA or called out as Other
  for(x in options){
    if( nrow(filter(dat, Race == x )) > 0 )
      dat[[x]] <- dat$Race == x
    else
      dat[[x]][ dat$Race %in% options ] = FALSE
  }
  dat$Likely_Caucasian = dat$Race %in% options[c(1,NA)]
  if( !na_to_lc ) dat$Likely_Caucasian[ is.na(dat$Race)] = NA
  dat$Race[ dat$Race == "Other" ] = NA
  return(dat)
}

#set_race_fields(data.frame(Race=c("White", "White", "African-American", "Caucasian", "Black", "African", "Other", NA, NA, "Other", "Pac_Islander")), na_to_lc=FALSE)
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
  response_data = stringr::str_to_title(dat[[response_col]])
  dat$Response = response_data
  if(any(response_data %in% c("Pd", "Sd", "Pr", "Cr"))){
    dat$Response[response_data == "Pd"] = "Progressive Disease"
    dat$Response[response_data == "Sd"] = "Stable Disease"
    dat$Response[response_data == "Pr"] = "Partial Response"
    dat$Response[response_data == "Cr"] = "Complete Response"
  }
  
  dat$Progression = NA
  dat$Progression= dat$Response == "Progressive Disease"
  dat$Clinical_Benefit = !dat$Progression
  
  dat$Responder = NA
  dat$Responder[dat$Response %in% c("Complete Response", "Partial Response")] = TRUE
  dat$Responder[dat$Response %in% c("Stable Disease", "Progressive Disease")] = FALSE
  
  return(dat)
}
#set_response_data(data.frame(pid=c(1:4), Response=c("Pd", "PD", "Progressive disease", "Progressive Disease")))