# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format_sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Formats sex values
#' 
#' @description 
#' Converts data's original sex characters to standard Female / Male.
#' 
#' @param sex Vector of sex values
#' @param female_chars Characters in sex to replace with Female, defaults include "female", "F", and "f"
#' @param male_chars Characters in sex to replace with Male, defaults include "male", "M", and "m"
#' @param female_replacement Value to replace female_chars with, defaults to "Female"
#' @param male_replacement Value to replace male_chars with, defaults to "Male"
#' 
#' @return Returns the updated sex vector
#' 
#' @export
format_sex = function(sex, female_chars = c( "female", "F", "f", "FEMALE" ), male_chars = c( "male", "M", "m", "MALE" ), female_replacement = "Female", male_replacement= "Male" ){
  prior_na_count <- sum(is.na(sex))
  #create look up table
  lut <- c( rep(c("Female", "Male"), times=c(length(female_chars)+1, length(male_chars)+1) ))
  names(lut) <- c(female_chars,"Female", male_chars,"Male")
  #return values as looked up in lut
  sex <- mapply( function(s) return(lut[s]), as.character(sex), USE.NAMES = F )
  if( sum(is.na(sex)) > prior_na_count ) warning( paste("Converted", (sum(is.na(sex)) - prior_na_count), " values not found in male_chars or female_chars to NA.") )
  return( sex )
}

#format_sex( c("M", "F", "F", "m", "male", "female", "f", NA, "", "Min", "Female", "Male"), male_chars = c("male","M","m","Min",""))
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
  if(is.na(column_name) | column_name %ni% names(dat)) dat$Race = NA
  else dat$Race = stringr::str_to_title(dat[[column_name]])
  #override quirk in str_to_title where _ is not a word separator ...
  dat$Race[ dat$Race == "Pac_islander" | dat$Race %like% "Pacific"] = "Pac_Islander"
  dat$Race[ dat$Race == "Nat_american" ] = "Nat_American"
  dat$Race[dat$Race %like% "White"] = "Caucasian"
  #convert Black and African-American to African
  dat$Race[dat$Race %like% "Black" | dat$Race %like% "African"] = "African"
  #convert Unknowns to NA
  dat$Race[dat$Race %like% "Unknown"] = NA
  options = c(NA, "Caucasian", "Asian", "African", "Nat_American", "Pac_Islander", "Other")
  others = levels(factor(dat$Race[dat$Race %ni% options]))
  if( length(others) > 0 ){
    warning("The following Race values converted to 'Other':: ", paste(others, collapse=", "))
    dat$Race[dat$Race %ni% options] = "Other"  
  }
  options = options[-c(1,length(options))]
  # We want specific race fields to be TRUE if matches Race field value
  #       FALSE if we know they can't be the given Race ( because they are a different known race, excluding Other )
  #       Likely_Caucasian where not a non-caucasian known race or where Other and there are no known Caucasians 
  #           AND na_to_lc is TRUE
  #       NA where data is NA or called out as Other
  for(x in options){
    if( length(dat[dat$Race %like% x, "Race"]) > 0 ){
      dat[[x]] <- dat$Race == x
    }else{
      dat[[x]][ dat$Race %in% options ] = FALSE
    }
  }
  dat$Likely_Caucasian = dat$Race %in% options[c(1,NA)]
  if( !na_to_lc ) dat$Likely_Caucasian[ is.na(dat$Race)] = NA
#  dat$Race[ dat$Race == "Other" ] = NA
  output_summary(dat$Race, "Race")
  cat("Also added/modified columns: ", options, "and Likely_Caucasian", sep=", ")
  return(dat)
}

#tdf <- data.frame(Race=c(" ", "White", "White", "African-American", "Caucasian", "Black", "African", "Other", "Unknown", NA, "Other", "Pac_Islander"))
#rf2 = set_race_fields(tdf, "Race", na_to_lc=TRUE)

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
#' 
set_response_data = function( dat, response_col = "Response" ){
  #Normalize abbreviated responses, if there are any, while also normalizing full name values if those already exist
  response_data = stringr::str_to_title(dat[[response_col]])
  dat$Response = response_data
  #clever approach but doesn't work if any entries are already full words b/c they don't match the lut
  #fm_response_lut = c(Pd="Progressive Disease", Sd="Stable Disease", Pr="Partial Response", Cr="Complete Response")  
  #dat$Response[dat] = fm_response_lut[dat$Response]
  if(any(response_data %in% c("Pd", "Sd", "Pr", "Cr"))){
    dat$Response[response_data == "Pd"] = "Progressive Disease"
    dat$Response[response_data == "Sd"] = "Stable Disease"
    dat$Response[response_data == "Pr"] = "Partial Response"
    dat$Response[response_data == "Cr"] = "Complete Response"
  }
  #convert remaining values to NA
  dat$Response[dat$Response %ni% c("Progressive Disease", "Stable Disease", "Partial Response", "Complete Response")] = NA
  #set additional values
  dat$Progression = NA
  dat$Progression= dat$Response == "Progressive Disease"
  dat$Clinical_Benefit = !dat$Progression
  
  dat$Responder = NA
  dat$Responder[dat$Response %in% c("Complete Response", "Partial Response")] = TRUE
  dat$Responder[dat$Response %in% c("Stable Disease", "Progressive Disease")] = FALSE
  output_summary(dat$Response, "Response")
  cat("Also added/modified columns: Responder, Progression and Clinical Benefit")
  return(dat)
}
# dat = data.frame(Response=c("PD","SD","Pr","Pd","NE",NA,"CR","CR"))
# set_response_data(dat)
# set_response_data(data.frame(pid=c(1:6), Response=c("Pd", "PD", "Summin Else", "NE", "Progressive Disease", NA)))

output_summary = function(vec, varname){
  s = summary(factor(vec))
  cat("Final ",varname," values:", "\n")
  for( n in names(s) ){
    cat(n, ": ", s[n], "\n")
  }
}