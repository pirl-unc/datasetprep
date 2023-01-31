# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# converge_aliases
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @keywords internal
#' 
#' @title Takes an input vector and replaces aliases with preferred values
#'
#' @description
#' This method replaces aliases in an input vector using a user-defined
#' lookup table.
#'
#' @param input_vector Character vector with aliases to be replaced
#' @param input_vector_sep Character separating multi-name values of input vector
#' @param output_vector_sep Character to separate returned multi-name values
#' @param lut_path Path to .tsv with alias and preferred name columns
#' @param alias_clms Vector of column names containing aliases in lut
#' @param preferred_name_clm Name of column from which to pull preferred names to replace aliases
#' @param alias_sep Character separating multiple aliases found within a single alias_clm
#' @param include_missing_terms Boolean to return unchanged names that are not found in the alias_clms
#' @param default_empty_value Value to use where output vector name is empty or NA
#'
#' @return Returns a vector of same length as input_vector with aliases replaced by preferred values
#'
#' @export
#'
converge_aliases <- function(
    input_vector,
    input_vector_sep = "+",
    output_vector_sep = "+",
    lut_path,
    alias_clms="Alias",
    preferred_name_clm="Name",
    alias_sep=",",
    include_missing_terms = FALSE,
    default_empty_value = NA
){

  #load the lut_path into a data frame
  if (!file.exists(lut_path)) stop( "No file exists at lut_path ( ", lut_path, " )." )
  lut_df <- read.csv(lut_path, sep="\t", na.strings = "")

  if ( !all(alias_clms %in% names(lut_df)) )
  {
    missing_clms <- alias_clms[ which(!(alias_clms %in% names(lut_df)))]
    stop( "Not all alias_clms ( ", paste(missing_clms, collapse=",") , " ) exist in lookup table provided.")
  }

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
  # now add any names that are in the preferred_name_clm but not already in the lut from alias_clms
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
    warning( "The input_vector_sep ( ", input_vector_sep, " ) is found in ", num_matches, " of the aliases ( ", paste(matching_aliases, collapse=","), " ). These aliases will never match an input value.")
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# converge_drug_aliases
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Takes an input vector of drug names/combinations and replaces 
#' aliases with preferred names
#'
#' @description
#' This method replaces aliases in an input vector using a user-defined
#' drug table.
#'
#' @param input_vector Character vector with drug aliases to be replaced
#' @param include_missing_terms Boolean to keep unchanged names that are not found in the alias_clms
#' @param drug_lut_path Path to .tsv drug table
#' @param default_empty_value Value to use where output vector name is empty or NA
#'
#' @return Returns a vector of same length as input_vector with drug aliases replaced by preferred names
#'
#' @export
#'
converge_drug_aliases <- function(
    input_vector,
    drug_lut_path=system.file("rx_table", "rx_table.tsv", package="datasetprep"),
    include_missing_terms = FALSE,
    default_empty_value = NA
){
  
  ## DEFAULTS TO USE ###
  # There shouldn't be any call to change these but if necessary, keep both for backwards compatibility?
  #   * add functionality in base converge_aliases, to handle an input vector for the separators
  #
  #
  input_vector_sep = "+"
  output_vector_sep = " + "
  alias_clms=c("Full_Name", "Name_Aliases")
  preferred_name_clm=c("Preferred_Name")
  alias_sep=","

  #call the base function with argument values and defaults
  return_df <- converge_aliases(
    input_vector,
    input_vector_sep = input_vector_sep,
    output_vector_sep = output_vector_sep,
    lut_path = drug_lut_path,
    alias_clms = alias_clms,
    preferred_name_clm = preferred_name_clm,
    alias_sep = alias_sep,
    include_missing_terms = include_missing_terms, 
    default_empty_value = default_empty_value
    )
  
  # anything extra we'd like to do here? maybe output some stats to the user?
  
  # return the df
  return( return_df )
}

######### TESTING DATA. ###############
# 
# library(magrittr)
# 
# drug_lut_path <- file.path("~/vincent_lab/packages/datasetprep/inst/rx_table/rx_table.tsv")
# full_name_drugs <- c("Interferon", "cisplatin", "Ipilimumab", "Atezo")
# preferred_name_drugs <- c("Ipi", "Ribociclib", "atezo")
# alias_name_drugs <- c("Paclitaxel", "Temodar", "Temo", "Mk-3475", "Tykerb and Tyverb")
# each_category_drugs <- c("BiocheMO", "Trametinib", "Dabrafenib", "Ramucrimab", "Atezo", "Herceptin", "Ipi")
# misc_name_and_combination_drugs <- c("NA", NA, "   ", "", "PEMBRO", "PEMBRO  + Ipilimumab", "DXP+SOX+ Yervoy")
# missing_name_drugs <- c("Rando drug", "DXP + Ipini")
# test_drug_vec <- c( full_name_drugs, preferred_name_drugs, alias_name_drugs, each_category_drugs, misc_name_and_combination_drugs, missing_name_drugs )
# out <- converge_drug_aliases( test_drug_vec, lut_path <- drug_lut_path)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lookup_properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#'
#' @keywords internal
#'
#' @title Finds properties for names in input vector using user-defined lookup 
#' table
#'
#' @description
#' This method looks up names in user-defined lookup table, takes properties 
#' found in the associated property_clm and expands them into a return 
#' data.frame with itemized and boolean columns for each.
#'
#' @param input_vector Character vector with names to be looked up
#' @param property_lut_path Path to .tsv with name and property columns
#' @param name_clm Column containing names in lut
#' @param property_clm Column containing properties in lut
#' @param copy_clms Some columns in the lookup table can have values that can be copied over.  For example, For 'Pembro' we'd want to find pembro on the list and then grab the info for ICI_Pathway (PD1) and pass that info along to the sample data. Optional vector of columns containing values to be copied directly
#' @param all_properties Vector of property names to look for in property_clm and include in return df
#' @param input_vector_sep Character separating multi-name values of input vector
#' @param property_sep Character separating multi-property values in property_clm
#' @param output_property_sep Character to separate multi-name values returned in itemized columns
#' @param output_copy_clm_sep Character separating copy column values from multi-name inputs
#' @param itemized_clm_suffix Characters appended to each property to form itemized column names in return df
#' @param boolean_clm_suffix Characters appended to each property to form boolean column names in return df
#' @param no_info_itemized_value Value to return for itemized columns where the given property is not found
#' @param no_info_boolean_value Value to return for boolean columns where the given property is not found
#' @param no_info_copy_value Value to return for copied columns where value is empty/NA
#' @param skip_itemized_clms Boolean to include itemized columns in return df
#' @param skip_boolean_clms Boolean to include boolean columns in return df
#' @param return_input Boolean whether to include the input vector as the first column of the return df
#'
#' @return Returns a dataframe with original input vector and itemized, 
#' boolean and copied values as requested through input parameters.
#'
#' @export
#' 
lookup_properties <- function(
    input_vector,
    property_lut_path,
    name_clm = "Name",
    property_clm = "Properties",
    copy_clms = NA,
    all_properties = c(),
    input_vector_sep = "+",
    property_sep = ",",
    output_property_sep = "+",
    output_copy_clm_sep = ",",
    itemized_clm_suffix = "_Itemized",
    boolean_clm_suffix = "_Boolean",
    no_info_itemized_value = NA_character_,
    no_info_boolean_value = FALSE,
    no_info_copy_value = NA,
    skip_itemized_clms = FALSE,
    skip_boolean_clms = FALSE,
    return_input=TRUE
) {
  # load lut
  if (!file.exists(property_lut_path)) stop( "No file exists at property_lut_path ( ", property_lut_path, " )." )
  lut_df <- read.csv(property_lut_path, sep="\t", na.strings = "")
  # check columns exist for name, properties and copy clms
  if ( !all(c(name_clm, property_clm) %in% names(lut_df)) ) stop( "The property_clm and/or name_clm does not exist in the lookup table provided.")
  if ( all( is.na(copy_clms) ) ) copy_clms <- c()
  if ( length(copy_clms) & !all( copy_clms %in% names(lut_df)) ) stop( "At least one of the copy_clms does not exist in the lookup table provided.")

  # create return dataframe with input values as first column
  output_df <- data.frame( input_values=input_vector )
  # initialize property columns in output_df based on all_properties and itemized / boolean parameters
  for ( prop in all_properties ) {
    if (is.na(prop) | prop == "") next
    if (!skip_itemized_clms) output_df[[ paste0(prop, itemized_clm_suffix) ]] <- no_info_itemized_value
    if (!skip_boolean_clms) output_df[[ paste0(prop, boolean_clm_suffix) ]] <- no_info_boolean_value
  }
  # initialize copy columns in output_df based on copy_clms. if copy_clms is NA, init as empty vector to simplify later code
  for ( clm in copy_clms ) {
    output_df[[ clm ]] <- no_info_copy_value
  }
  # create simple name to property lut to simplify lookup later
  props_lut <- lut_df[[ property_clm ]]
  names(props_lut) <- lut_df[[ name_clm ]]
  # create vector to hold names we don't find for reporting
  missing_names_vec <- c() 
  
  # for all of the names in the input_vector ( separated by input_sep ):
  # * find properties in lut ( note any names that aren't found ), and subset for all_properties
  # * update itemized and boolean for properties found in columns at this index of output_df
  # * if copy_clms exist, combine the values for all names into the matching column of output_df
  
  for ( input_index in seq_along(input_vector) ) {
    # split vector of names at this index of input_vector, remove white space around each and drop NA's
    these_names <- strsplit( input_vector[ input_index ], input_vector_sep, fixed=T)[[1]] %>% trimws() %>% .[complete.cases(.)]
    if ( length(these_names) == 0 | all( these_names == "" ) ) next # there is nothing to see here ... move on ( empty input_vector rows have already been initialized with default property values in output_df )
    # add any names not found in the names_vec to the missing_names_vec
    missing_names_vec %<>% c(these_names[ which( !(these_names %in% names(props_lut)) ) ])
    for ( name in these_names ){
      my_props <- props_lut[ name ]
      if( length(my_props) == 0 ) next
      # separate these properties by property_sep, remove whitespace and then keep only those that are in the all_properties vector
      my_props %<>% {strsplit(., property_sep, fixed=T )[[1]]} %>% trimws()
      my_props %<>% {.[ . %in% all_properties ]}
      # for each property update the matching itemized and boolean columns ( if requested in parameters )
      for ( prop in my_props ){
        if (!skip_itemized_clms){
          my_prop_clm <- paste0(prop, itemized_clm_suffix)
          current_prop_val <- output_df[input_index, my_prop_clm]
#          cat(my_prop_clm, " :: ", current_prop_val, " :: ", is.na(current_prop_val), " :: ", identical(current_prop_val, no_info_itemized_value), "\n")
          if ( is.na(current_prop_val) | identical(current_prop_val, no_info_itemized_value) ) output_df[input_index, my_prop_clm] <- name
          else output_df[input_index, my_prop_clm] %<>% paste(name, sep=output_property_sep)
        }
        if (!skip_boolean_clms) {
          output_df[input_index, paste0(prop, boolean_clm_suffix)] <- TRUE
        }
      }
    }
    # for any copy_clms, update the output_df at this index with combined values from lut_df for all names
    for( clm_to_copy in copy_clms ){
      vals_to_copy <- lut_df[ which( names(props_lut) %in% these_names), clm_to_copy ] %>% unique() %>% {.[complete.cases(.)]}
      if( length(vals_to_copy) ) output_df[input_index, clm_to_copy] <- paste(vals_to_copy, collapse=output_copy_clm_sep)
    }
  }
  
  missing_names_vec %<>% .[ complete.cases(.) ] %>% unique()
  if( length(missing_names_vec) ) cat("The following names in the input_vec were not found in the lookup table: ", paste(missing_names_vec), "\n")

  # remove the first column ( a copy of the input_vec ) from the return df if requested
  if( !return_input ){
    output_df[-1]
  }
  
  return(output_df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lookup_drug_properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Looks up properties for drug combinations in input vector using user-defined lookup table
#'
#' @description
#' This method calls base lookup_properties method with appropriate values for package drug table
#' and returns the resulting df with aCTLA4_PD1_Tx column added
#'
#' @param input_vector Character vector with drug names to be looked up
#' @param property_lut_path Path to .tsv with drug names and properties
#' @param copy_clms Optional vector of columns containing values to be copied directly
#' @param all_properties Vector of property names to look for in property_clm and include in return df
#' @param return_input Boolean whether to include the input vector as the first column of the return df
#'
#' @return Returns a dataframe with original input vector and itemized, 
#' boolean and copied properties as requested through input parameters.
#'
#' @export
#'
lookup_drug_properties <- function(
    input_vector,
    property_lut_path=system.file("rx_table", "rx_table.tsv", package="datasetprep"),
    copy_clms = c("ICI_Pathway", "ICI_Target"),
    all_properties = c("ICI", "Non_ICI", "IS", "aPD1", "aCTLA4", "aVEGF", "aBRAF", "aMAPK", "Chemo", "Steroid"),
    return_input = FALSE
){
  
  name_clm = "Preferred_Name"
  property_clm = "Properties"
  input_vector_sep = "+"
  property_sep = ","
  output_property_sep = "+"
  output_copy_clm_sep = ","
  lut_path=system.file("rx_table", "rx_table.tsv", package="datasetprep")
  itemized_clm_suffix = "_Rx"
  boolean_clm_suffix = "_Tx"
  no_info_itemized_value = "None"
  no_info_boolean_value = FALSE
  no_info_copy_value = "None"
  skip_itemized_clms = FALSE
  skip_boolean_clms = FALSE

  props_df <- lookup_properties(
    input_vector, 
    property_lut_path=property_lut_path, 
    name_clm=name_clm, 
    property_clm = property_clm, 
    copy_clms=copy_clms,
    all_properties=all_properties, 
    itemized_clm_suffix=itemized_clm_suffix, 
    boolean_clm_suffix=boolean_clm_suffix, 
    no_info_itemized_value = no_info_itemized_value, 
    no_info_boolean_value=no_info_boolean_value,
    no_info_copy_value = no_info_copy_value, 
    skip_itemized_clms=skip_itemized_clms, 
    skip_boolean_clms=skip_boolean_clms,
    return_input=return_input 
    )
  
  # add the aCTLA4_aPD1_Tx column if boolean_clms are requested and all_properties includes both aCTLA4 and aPD1
  if(!skip_boolean_clms && all(c("aPD1", "aCTLA4") %in% all_properties)){
    props_df$aCTLA4_aPD1_Tx <- ( props_df$aCTLA4_Tx & props_df$aPD1_Tx )
  }
  
  return( props_df )
  
}

########### TESTING DATA ###########
#
# assumes out is a list of valid drug Preferred_Names like output from converge_drug_aliases()
# lookup_drug_properties( out, property_lut_path = drug_lut_path)
#
#######################

