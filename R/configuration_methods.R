# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# configure_output_columns
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Depricated method to define columns for output data - use get_run_only_columns, get_patient_only_columns, get_all_columns instead
#' 
#' @description 
#' Method to set RUN_ONLY_COLUMNS and PATIENT_ONLY_COLUMNS to be output to final data.
#'  
#' @export
#' 
configure_output_columns = function(){
  stop("Depricated - please use get_run_only_columns, get_patient_columns and get_all_columns instead.")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get_run_only_columns
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Returns character vector of column names for data only relevant to a 
#' sample or sequencing run as opposed to a patient.
#'  
#' @return Character vector of column names
#'
#' @export
#' 
get_run_only_columns = function(){
  return( c(
    'Run_ID', 'Sequencing_Method', 'Analyte', 'Biopsy_Site', 
    'Specific_Biopsy_Site', 'Metastatic_Biopsy', 'Sample_Treated', 'Timepoint', 
    'Sample_Type', 'Normal', 'Primary_Tumor', 'Read_Length', 'Batch_ID', 
    'Sequencer', 'Sequencer_ID', 'Sample_Prep', 'FFPE', 'Likely_FFPE', 
    'Storage', 'Storage_Method', 'File_Prefix', 'Run_Name', 
    'Pre_On_Treatment_Match', 'Somatic_Workflow_Match', 'Center'
    ) )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get_patient_columns
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Returns character vector of column name relevant to the patient and 
#' should be true of all samples coming from that patient
#' 
#' @description 
#' Method to return static list of columns relevant to patient level data.
#'  
#' @return Character vector of column names
#'
#' @export
#' 
get_patient_columns = function(){
  return( c(
    'Patient_ID', 'TCGA_Tissue', 'TCGA_Subtype', 'Cancer_Tissue', 'Tissue_Subtype',
    'M_Stage', 'Metastasized', 'Clinical_Stage', 'Response', 'Responder', 'Progression', 'Clinical_Benefit',
    'OS_e', 'OS_d', 'PFS_e', 'PFS_d', 'Age', 'Sex',
    'Race', 'Caucasian', 'Asian', 'African', 'Native_American', 'Pacific_Islander', 'Likely_Caucasian',
    'ICI_Rx', 'aPD1_Rx', 'aCTLA4_Rx', 'ICI_Pathway', 'ICI_Target',
    'Non_ICI_Rx', 'NeoICI_Rx',  'ICI_Tx', 'aPD1_Tx', 'aCTLA4_Tx', 'aCTLA4_aPD1_Tx',
    'Non_ICI_Tx', 'NeoICI_Tx',
    'Prior_Rx', 'Prior_ICI_Rx', 'Prior_Tx', 'Prior_ICI_Tx', 'Prior_aCTLA4_Tx', 'Prior_aMAPK_Tx',
    'Subsq_Rx', 'Subsq_ICI_Rx', 'Subsq_Tx', 'Subsq_ICI_Tx', 'Subsq_aCTLA4_aPD1_Tx', 'Subsq_aCTLA4_Tx',
    'Subsq_aMAPK_Tx',
    'Patient_Name', 'Dataset'
  ) )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get_all_columns
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Returns character vector concatenation of get_patient_columns and get_run_only_columns
#' 
#' @description 
#' Method to return character vector combining of get_patient_columns and
#' get_run_only_columns
#'  
#' @return Character vector of get_patient_columns and get_run_only_columns
#'
#' @export
#' 
get_all_columns = function(){
  return( c( datasetprep::get_run_only_columns(), datasetprep::get_patient_columns() ) )
}
