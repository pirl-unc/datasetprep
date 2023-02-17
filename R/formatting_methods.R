# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format_sex
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Formats sex values
#' 
#' @description 
#' Converts data's original sex characters to standard Female / Male.
#' 
#' @param sex Vector of sex values
#' @param female_values String values in sex to replace with Female, defaults include "female", "F", and "f"
#' @param male_values String values in sex to replace with Male, defaults include "male", "M", and "m"
#' @param female_replacement Value to replace female_chars with, defaults to "Female"
#' @param male_replacement Value to replace male_chars with, defaults to "Male"
#' 
#' @return Returns the updated sex vector
#' 
#' @export
format_sex = function(
	sex, 
	female_values = c( "female", "f", 'w', "woman", "girl", 'g'), 
	male_values = c( "male", "m", "man", "boy", 'b'), 
	female_replacement = "Female", 
	male_replacement= "Male" 
){
	sex %<>% tolower() %>% trimws()
	sex[sex %in% female_values] = female_replacement
	sex[sex %in% male_values] = male_replacement
  unmatched_strings = unique(sex)
  unmatched_strings = unmatched_strings[!is.na(unmatched_strings)]
  unmatched_strings = unmatched_strings[unmatched_strings %ni% c(female_replacement, male_replacement)]
  if (length(unmatched_strings) > 0) cat("Could not match biological sex values: ", paste0(unmatched_strings, collapse = ", "), "\n")
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
#' @param input_clm Name of column containing race values, defaults to Race, if NA intializes Race as all NA
#' @param na_to_lc Boolean dictating whether to set any na's to Likely Caucasian
#' 
#' @return Returns dat with updated and added columns
#' 
#' @export
set_race_fields = function (dat, input_clm = "Race", na_to_lc = FALSE) 
{
	if (is.na(input_clm) | input_clm %ni% names(dat)) {
		warning(paste0("set_race_fields input_clm (", input_clm, ") does not exist in dat."))
		dat$Race = NA
		
	} #else { - we still want to add these other fields, even if there is no race data - just start from race = NA
		dat$Race %<>% gsub("_", " ", ., fixed = T)
		dat$Race %<>% trimws
		dat$Race = stringr::str_to_title(dat[[input_clm]])
		dat$Race[grepl("^Pac", dat$Race) | dat$Race %like% "Pacific"] = "Pacific Islander"
		dat$Race[grepl("^Nat", dat$Race)] = "Native American"
		dat$Race[grepl("^Cauc", dat$Race)] = "Caucasian"
		dat$Race[dat$Race %like% "European"] = "Caucasian"
		dat$Race[dat$Race %like% "White"] = "Caucasian"
		dat$Race[dat$Race %like% "Black" | dat$Race %like% "African" | grepl("^Afr", dat$Race)] = "African"
		dat$Race[dat$Race %like% "Unknown" | grepl("^Un", dat$Race)] = NA
		options = c(NA, "Caucasian", "Asian", "African", "Native American", 
								"Pacific Islander", "Other")
		others = levels(factor(dat$Race[dat$Race %ni% options]))
		if (length(others) > 0) {
			warning("The following Race values converted to 'Other': ", 
							paste(others, collapse = ", "))
			dat$Race[dat$Race %ni% options] = "Other"
		}
		options = options[!is.na(options)]
		for (x in options) {
			if (nrow(dplyr::filter(dat, Race == x)) > 0) {
				dat[[gsub(" ", "_", x)]] <- dat$Race == x
			} else {
				dat[[gsub(" ", "_", x)]][dat$Race %in% options] = FALSE
			}
		}
		
		cat("Added/modified columns: ", paste0(options, collapse = ", "), "\n")
#	}
	dat$Likely_Caucasian = dat$Race == "Caucasian"
	
	if (na_to_lc) {
		dat$Likely_Caucasian[is.na(dat$Race)] = TRUE
		cat("Assumed NA's were Likely_Caucasian\n")
	}
	output_summary(dat$Race, "Race")
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
#' 
set_response_data = function( dat, response_col = "Response" ){
  #Normalize abbreviated responses, if there are any, while also normalizing full name values if those already exist
  response_data = toupper(dat[[response_col]]) %>% trimws()
  dat$Response = response_data
 
  dat$Response[response_data %in% c("PD", "PROGRESSIVE DISEASE")] = "Progressive Disease"
  dat$Response[response_data %in% c("SD", "STABLE DISEASE")] = "Stable Disease"
  dat$Response[response_data %in% c("PR", "PARTIAL RESPONSE")] = "Partial Response"
  dat$Response[response_data %in% c("CR", "COMPLETE RESPONSE")] = "Complete Response"
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# output_summary
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Outputs summary of values
#' 
#' @param vec Vector to summarize
#' @param varname Name of values

#' 
#' @return outputs values
#' 
#' @export
#' 
output_summary = function(vec, varname){
  s = summary(factor(vec))
  cat(paste0("Final ",varname," values:", "\n"))
  for( n in names(s) ){
    cat(paste0("  ", n, ": ", s[n], "\n"))
  }
}