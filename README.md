The purpose of this package is to add tools that make life easier when performing dataset prep.  

## Install
Restart R Session  
In R:
``` r
devtools::install_github("Benjamin-Vincent-Lab/datasetprep")
```

Or for a specific version:
``` r
devtools::install_github("Benjamin-Vincent-Lab/datasetprep", ref = "0.3.2")
```

Use the package documentation for help:
``` r
??datasetprep
```

##Configuration methods

###init_paths
Sets up the post processing directory and prepares temp library folder if necessary.

###create_input_paths
Uses the RAW_DATA_DIR and the parameters passed in to create a full path to the input file being requested.

## Standardizing column names

### format_sex
Converts data's original sex characters to standard Female / Male.

### set_race_fields
Takes data's original race values and cleans them up for consistency, converts unknown races to Other, and sets the individual boolean race fields ( Caucasian, Asian, etc. ) based on updated Race.

## Variable cleanup and setting of related values
### set_response_data
This method standardizes the Best Response data to full names in CamelCase and then sets fields that are dependent on those values ( Progression, Clinical_Benefit and Responder ).

### format_patient_names
Data's original patient identifiers are often more complex than we want. This method takes those names ( within a limited subset of formats ) and reformats them as "p001" where the number portion is unique across patients.

### create_run_names  
Combines normal_tissue, analyte and file_prefix values into Run_Names of format {a|n}{d|r|m|p}{unique part of file_prefix}.

## Simple concatenations

### create_patient_ids  
Concatenates dataset with patient_names. 

### create_run_ids
Concatenates patient_ids with run_names.

## Complex concatenations

### find_somatic_workflow_matches
Typically somatic workflow matches are when a given patient has samples of tumor data in RNA and DNA form along with normal tissue in DNA form. This method determines for which patients this is true.


