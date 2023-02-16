The purpose of this package is to add tools that make life easier when performing dataset prep.  

## Install
Restart R Session  
In R:
``` r
devtools::install_github("Benjamin-Vincent-Lab/datasetprep")
```

Or for a specific version:
``` r
devtools::install_github("Benjamin-Vincent-Lab/datasetprep", ref = "0.4.2")
```

Use the package documentation for help:
``` r
??datasetprep
```

##Configuration methods

###get_run_only_columns
Returns character vector of column names for data only relevant to a sample or sequencing run as opposed to a patient.

###get_patient_only_columns
Returns character vector of column name relevant to the patient and should be true of all samples coming from that patient

###get_all_columns
Returns character vector concatenation of get_patient_columns and get_run_only_columns

##Initializaton methods

###init_paths
Sets up the post processing directory and prepares temp library folder if necessary.

## Standardizing column names

### format_sex
Converts data's original sex characters to standard Female / Male.

### set_race_fields
Takes data's original race values and cleans them up for consistency, converts unknown races to Other, and sets the individual boolean race fields ( Caucasian, Asian, etc. ) based on updated Race.

### set_response_data
This method standardizes the Best Response data to full names in CamelCase and then sets fields that are dependent on those values ( Progression, Clinical_Benefit and Responder ).

## Creating and formatting patient names and ids

### format_patient_names
Data's original patient identifiers are often more complex than we want. This method takes those names ( within a limited subset of formats ) and reformats them as "p001" where the number portion is unique across patients.

### create_patient_ids  
Concatenates dataset with patient_names. 

## Creating and formatting run names and ids

### create_run_names  
Combines normal_tissue, analyte and file_prefix values into Run_Names of format {a|n}{d|r|m|p}{unique part of file_prefix}.

### create_run_ids
Concatenates patient_ids with run_names.

## Somatic workflows

### find_somatic_workflow_matches
Typically somatic workflow matches are when a given patient has samples of tumor data in RNA and DNA form along with normal tissue in DNA form. This method determines for which patients this is true.

## Drug lookup

### set_rx_tx
DEPRICATED :: Takes a data.frame with a Drug column and returns formatted variables based on the drug names
### converge_drug_aliases
Replaces drug name aliases with preferred name values
### lookup_drug_properties
Looks up properties for drugs and sets itemized and boolean fields with appropriate values

## Helpers

### output_variable_summary
Outputs summary of variables in data.frame
