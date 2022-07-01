The purpose of this package is to add tools that make life easier when performing dataset prep.  

##Setup and debugging methods

### a
This method allows user to add annotations to a dataset prep file. Annotations are output into configured readme_path ( if one exists ) and output to console.

### configure_readme
This method sets up the readme file based on the dataset name and output_dir provided.

### output_variable_summary
In order to confirm data being generated through the prep phase, it can be helpful to output a summary of variables in a data.frame. This method takes a dataframe and does just that, outputting the summary to the console.

## Basic normalization

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


