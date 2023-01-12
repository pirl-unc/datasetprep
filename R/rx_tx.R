# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# converge_aliases
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Takes an input vector and replaces aliases with preferred values
#'
#' @description
#' This method replaces aliases in an input vector using a user-defined
#' lookup table.
#'
#' @param input_vector Character vector with aliases to be replaced
#' @param input_vector_sep Character separating multi-element values of input vector
#' @param output_vector_sep Character to separate returned multi-element values
#' @param lut_path Path to .tsv with alias and preferred name columns
#' @param alias_clms Vector of column names containing aliases in lut
#' @param preferred_name_clm Name of column from which to pull preferred names to replace aliases
#' @param alias_sep Character separating multiple aliases found within a single alias_clm
#' @param include_missing_terms Boolean to return unchanged elements that are not found in the alias_clms
#' @param default_empty_value Value to use where output vector element is empty or NA
#'
#' @return Returns a vector of same length as input_vector with aliases replaced by preferred values
#'
#' @export
#'
converge_aliases <- function(
    input_vector,
    input_vector_sep = "+",
    output_vector_sep = " + ",
    lut_path=system.file("rx_table", "rx_table.tsv", package="datasetprep"),
    alias_clms=c("Full_Name", "Name_Aliases"),
    preferred_name_clm=c("Preferred_Name"),
    alias_sep=",",
    include_missing_terms = FALSE,
    default_empty_value = NA
){

  #load the lut_path into a data frame
  if (!file.exists(lut_path)) stop( "No file exists at lut_path ( ", lut_path, " )." )
  lut_df <- read.csv(lut_path, sep="\t",na.strings = "")

  if ( !all(alias_clms %in% names(lut_df)) ) stop( "Not all alias_clms exist in lookup table provided.")

  full_lut <- c()
  preferred_names_vec <- lut_df[[ preferred_name_clm ]] %>% trimws()
  if ( any( is.na(preferred_names_vec) | preferred_names_vec == "" ) ) cat( "It would be advisable to review the file at lut_path. One or more entries in the preferred_name_clm are NA or empty.\n\n" )
  # create a single lut with all aliases from alias_clms provided pointing towards preferred_clm values
  for ( clm in alias_clms ) {
    alias_vec <- lut_df[[ clm ]]
    for ( alias_index in seq_along(alias_vec) ) {
      # skip NA or empty string aliases
      if ( is.na(alias_vec[ alias_index ]) | trimws(alias_vec[ alias_index ]) == "" )  next
      # split aliases into individual items based on alias_sep provided
      these_aliases <- strsplit( alias_vec[ alias_index ], alias_sep, fixed=T )[[1]] %>% unlist()
#      print(these_aliases)
      # add these aliases ( trimmed and to lower case ) as names of LUT pointing towards preferred_name
      add_aliases <- c( rep(preferred_names_vec[ alias_index ], length(these_aliases)) )
      names(add_aliases) <- these_aliases %>% trimws() %>% tolower()
      full_lut %<>% c( add_aliases )
    }
  }
  # now add any elements that are in the preferred_name_clm but not already in the lut from alias_clms
  missing_pns <- preferred_names_vec %>% tolower() %>% {!(. %in% names(full_lut))}
  if ( sum(missing_pns) ) {
    # there are preferred_names not yet in the lut
    missing_vec <- preferred_names_vec[ missing_pns ]
#    print(paste("Adding preferred_names ", paste(missing_vec, collapse=","), " to lut."))
    names(missing_vec) <- missing_vec %>% tolower()
    full_lut %<>% c( missing_vec )
  }

  # warn if we have duplicate aliases
  if ( sum(duplicated(names(full_lut))) ) {
    warning( paste("More than 1 entry exists in alias_clms for ", paste0(unique(names(full_lut)[ duplicated(names(full_lut)) ]), collapse=", "), ". Results may not be as expected.") )
  }

  # sanity check for aliases that contain the input_vector_sep as they can't possibly be matched by input values ( which will already have the input_vector_sep character removed ).
  input_sep_matches <- grepl( input_vector_sep, names(full_lut), fixed=T )
  if( any(input_sep_matches) ){
    num_matches <- sum(input_sep_matches)
    matching_aliases <- names(full_lut)[input_sep_matches]
    warning( "The input_vector_sep ( ", input_vector_sep, " ) is found in ", num_matches, " of the aliases ( ", paste(matching_aliases, collapse=","), " ). These aliases will never match an input value.\n\n")
  }

  missing_vals <- c()
  # iterate on input vector, looking up each individual drug delimited by input_sep and replacing with preferred names
  output_vector <- sapply( input_vector, function( lookup_vals ){
    lookup_vals %<>% {strsplit(., input_vector_sep, fixed=T)[[1]]} %>% trimws()
    # exit if value is NA or ""
    if( all( is.na(lookup_vals) | lookup_vals %in% c("", "NA") ) ) return( default_empty_value )
    # lookup replacement values, intially keeping those that are not found ( will be NA's )
    replace_vals <- full_lut[ lookup_vals %>% tolower()  ] # %>% .[complete.cases(.)]
    # get any of the input values not found in lookup table
    not_found <- lookup_vals[ !complete.cases(replace_vals) ]
    # reduce replacements to only those found in lookup table
    replace_vals %<>% {.[complete.cases(.)]}
    # if there were any value not found, add them to the list for reporting at the end and replace them in the return vector if we are including_missing_terms
    if ( length(not_found)) {
      missing_vals <<- c(missing_vals, not_found)
      if( include_missing_terms ) replace_vals %<>% c(not_found)
    }
    # now return default empty value if no replacements were found ( an we are not including_missing_terms )
    if ( !length( replace_vals ) ) return( default_empty_value )
    return( paste( replace_vals, collapse=output_vector_sep) )
  }, USE.NAMES = F)
  missing_vals %<>% unique()
  if ( length(missing_vals ) ) cat( "The following values from input_vec could not be found in the lookup table: ", paste( missing_vals, collapse=", "), "\n\n")

  return( output_vector )
}

######### TESTING DATA. ###############
# out <- converge_aliases( test_drug_vec, "+", " + ", drug_path, include_missing_terms=T )
#
# full_name_drugs <- c("Interferon", "cisplatin", "Ipilimumab")
# preferred_name_drugs <- c("Ipi", "Ribociclib", "atezo")
# alias_name_drugs <- c("Paclitaxel", "Temodar", "Temo", "Mk-3475", "Tykerb and Tyverb")
# misc_name_tests <- c("NA", NA, "   ", "", "PEMBRO", "PEMBRO  + Ipilimumab")
# missing_name_tests <- c("Rando drug", "DXP + Ipini")
# test_drug_vec <- c( full_name_drugs, preferred_name_drugs, alias_name_drugs, misc_name_tests, missing_name_tests )


lookup_properties <- function(
    input_vector,
    input_vector_sep = "+",
    property_lut_path=system.file("rx_table", "rx_table.tsv", package="datasetprep"),
    name_clm = "Preferred_Name",
    property_clm = "Properties",
    copy_clms = c("ICI_Pathway", "ICI_Target"),
    property_sep = ",",
    all_properties = c("ICI", "Non_ICI", "IS", "aPD1", "aCTLA4", "aVEGF", "aBRAF", "aMAPK", "Chemo", "Steroid"),
    no_info_itemized_value = "None",
    no_info_boolean_value = FALSE,
    no_info_copy_value = NA,
    itemized_clm_suffix = "_Rx",
    boolean_clm_suffix = "_Tx",
    skip_itemized_clms = FALSE,
    skip_boolean_clms = FALSE
) {

  if (!file.exists(property_lut_path)) stop( "No file exists at property_lut_path ( ", property_lut_path, " )." )
  lut_df <- read.csv(property_lut_path, sep="\t", na.strings = "")

  if ( !all(c(name_clm, property_clm) %in% names(lut_df)) ) stop( "The property_clm and/or name_clm does not exist in the lookup table provided.")

  output_df <- data.frame( original_values=input_vector )
  for ( prop in all_properties ){
    if (!skip_itemized_clms) output_df[[ paste0(prop, itemized_clm_suffix) ]] <- no_info_itemized_value
    if (!skip_boolean_clms) output_df[[ paste0(prop, boolean_clm_suffix) ]] <- no_info_boolean_value
  }

  if ( all( is.na(copy_clms) ) ) copy_clms <- c()
  if ( length(copy_clms) & !all( copy_clms %in% names(lut_df)) ) stop( "At least one of the copy_clms does not exist in the lookup table provided.")
  for ( clm_to_copy in copy_clms ){
    output_df[[ clm_to_copy ]] <- no_info_copy_value
  }

  names_vec <- lut_df[[ name_clm ]]
  props_vec <- lut_df[[ property_clm ]]
  elements_not_found <- c()
  for ( input_index in seq_along(input_vector) ) {
    these_elements <- strsplit( input_vector[ input_index ], input_vector_sep, fixed=T)[[1]] %>% trimws() %>% .[complete.cases(.)]
    if ( length(these_elements) == 0 | all( these_elements == "" ) ) next

    for ( element in these_elements ){
      my_props <- props_vec[ which( names_vec == element ) ]
      if( length(my_props) == 0 ) next
      # keep only those that are in the all_properties vector
      my_props %<>% {strsplit(., property_sep, fixed=T )[[1]]} %>% trimws()
      my_props %<>% {.[ . %in% all_properties ]}
      for ( prop in my_props ){
        if (!skip_itemized_clms){
          my_prop_clm <- paste0(prop, itemized_clm_suffix)
          if ( output_df[input_index, my_prop_clm] == no_info_itemized_value ) output_df[input_index, my_prop_clm] <- element
          else output_df[input_index, my_prop_clm] %<>% paste(element, sep="+")
        }
        if (!skip_boolean_clms) {
          output_df[input_index, paste0(prop, boolean_clm_suffix)] <- TRUE
        }
      }
    }
    for( clm_to_copy in copy_clms ){
      vals_to_copy <- lut_df[ lut_df[[ name_clm ]] %in% these_elements, clm_to_copy ] %>% unique() %>% {.[complete.cases(.)]}
      if( length(vals_to_copy) ) output_df[input_index, clm_to_copy] <- paste(vals_to_copy, collapse=",")
    }
  }
  return(output_df)
}

lur <- lookup_properties(c("DXP + SOX"), property_lut_path = drug_path)


# add comments so I can understand my own code - remove embedded loops if possible
# add output of elements not found
# what do we do with the combined PD1 / CTLA4 field?

# generally working - need to test:

# * full set of drugs with at least one from each class
# * various combinations to verify that two or more drugs work
# * confirm what happens with NA, "", and unknown drug names
# * changing values for all params

# test both methods with an actual dataset by updating the dataset_prep.R for one

