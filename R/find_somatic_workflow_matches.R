# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# find_somatic_workflow_matches
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Sets field equal to patient name where data exists for somatic workflow
#' 
#' @description 
#' Typically somatic workflow matches are when a given patient has samples of 
#' tumor data in RNA and DNA form along with normal tissue in DNA form. 
#' This method determines for which patients this is true.
#' 
#' @param dat Data.frame from which to determine matches
#' 
#' @section Limitations:
#' \itemize{
#'   \item Requires that data.frame sent have columnes named Patient_Name, Normal,
#'   Analyte and Run_Name
#'}
#' 
#' @return Vector of matches
#' 
#' @export
find_somatic_workflow_matches = function( dat ){
  if(any(c("Patient_Name", "Normal", "Analyte", "Run_Name") %ni% names(dat))){
    warning("Warning: find_somatic_workflow_matches requires data with variables 'Patient_Name', 'Normal', 'Analyte', and 'Run_Name'")
    return()
    
  }
  Somatic_Workflow_Match = dat$Patient_Name
  a("For Somatic_Workflow_Match, set to Patient_Name where patient has RNA and DNA samples from Tumor tissue AND a DNA sample from Normal tissue.")
  for (patient_name in unique(dat$Patient_Name)){
    subdat = dat[dat$Patient_Name == patient_name,]
    ar_runs = subdat[!subdat$Normal & subdat$Analyte == "RNA", c("Run_Name")]
    ad_runs = subdat[!subdat$Normal & subdat$Analyte == "DNA", c("Run_Name")]
    nd_runs = subdat[subdat$Normal & subdat$Analyte == "DNA", c("Run_Name")]
    if(length(ar_runs) == 0 | length(ad_runs) == 0 | length(nd_runs) == 0){
      Somatic_Workflow_Match[dat$Run_Name %in% c(ar_runs, ad_runs, nd_runs)] = NA
    }
  }
  return(Somatic_Workflow_Match)
}