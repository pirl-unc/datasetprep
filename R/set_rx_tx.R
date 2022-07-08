# library("readr")
# library("dplyr")
# library("stringr")

#test_df = data.frame(Patient_ID=c(1:4), Drug=c("Nivolumab", "Vemurafenib + Atezolizumab + Dabrafenib", "nivo + Binimetinib + MK3475", "nivo+Binimetinib"))#, "Pembro", "Ecorafenib", "Vinblastine","Ramucrimab", "Atezolizumab", "Atezolizumab + Pembro"))

#drug_path = paste(find_folder_along_path(housekeeping::get_script_dir_path(include_file_name = F), "inst"), "rx_list", "rx_list.tsv", sep="/")
drug_path = system.file(file.path("rx_list", "rx_list.tsv"), package = "datasetprep")

drug_list = read_tsv(drug_path, show_col_types=F)
#Fill in preferred_name fields where the preferred name is the same as the full generic name
drug_list$preferred_name[is.na(drug_list$preferred_name)] = drug_list$full_name[is.na(drug_list$preferred_name)]
#Add columns for is_PD1 and is_CTLA4
drug_list %<>% dplyr::mutate(is_PD1=ici_pathway %in% c("PD1", "PD-L1"), is_CTLA4=ici_pathway == "CTLA4")

#define look up table for tsv file names to data variable names
classes = c("is_PD1", "is_CTLA4", "is_aVEGF", "is_aBRAF", "is_aMAPK", "is_chemo")
names(classes) = c("aPD1", "aCTLA4", "aVEGF", "aBRAF", "aMAPK", "Chemo")

#convert all NA's to FALSE in drug class fields
drug_list$is_ici_inhibitor[is.na(drug_list$is_ici_inhibitor)] = FALSE
for(dci in 1:length(classes)){
  drug_list[[classes[dci]]][is.na(drug_list[[classes[dci]]])] = FALSE
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set_rx_tx
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Takes a drug list and returns fomatted variables
#' 
#' @description 
#' This method looks up drugs by name, normalizes them and sets the various 
#' pathway fields ( i.e. ICI_Rx, ICI_Tx, aPD1_Rx, aPD1_Tx ).
#' 
#' @param rx_list Dataframe containing " + " separated Drugs
#' 
#' @section Limitations:
#' \itemize{
#'   \item Does not work for prior or subsq fields.
#'   \item Does not set NeoICI_Tx
#'   \item Does not handle non-Rx treatments like Surgery, Radiation, etc.
#'}
#' 
#' @return Returns rx_list with updated and added columns
#' 
#' @export
set_rx_tx = function( rx_list ){
  #normalize drug names, replace full names with preferred names and normalized case
  for( rxi in 1:nrow(drug_list) ){
    rx_list$Drug %<>% gsub( drug_list$preferred_name[rxi], drug_list$preferred_name[rxi], ., ignore.case=TRUE ) %>%
                      gsub( drug_list$full_name[rxi], drug_list$preferred_name[rxi], ., ignore.case=TRUE ) 
  }

  # initialize all fields to None, Unknown or NA
  rx_list$ICI_Pathway = "None" #whats the target pathway
  rx_list$ICI_Target = "None" #whats the target receptor
  rx_list$ICI_Rx = "Unknown"
  rx_list$ICI_Tx = NA
  rx_list$Non_ICI_Rx = "Unknown"
  rx_list$Non_ICI_Tx = NA
  rx_list[,paste(names(classes),"Rx", sep="_")] = "Unknown"
  rx_list[,paste(names(classes),"Tx", sep="_")] = NA
  rx_list$aCTLA4_aPD1_Tx = NA

  #iterate over drugs in patient records
  for( di in 1:nrow(rx_list) ){
    #if drug field is NA or NULL, continue to next record
    if(is.na(rx_list$Drug[di]) | is.null(rx_list$Drug[di])) next()
    #if drug field is anything other than NA or NULL, assume we know the information for this patient
    #so update the _Rx fields to "None" and _Tx fields to FALSE
    rx_list$ICI_Rx[di] <- "None"
    rx_list$ICI_Tx[di] <- FALSE
    rx_list[di,paste(names(classes),"Rx",sep="_")] <- "None"
    rx_list[di,paste(names(classes),"Tx",sep="_")] <- FALSE
    rx_list$aCTLA4_aPD1_Tx = FALSE
    rx_list$Non_ICI_Rx[di] = "None"
    rx_list$Non_ICI_Tx[di] = FALSE
   #split on + to get all individual drugs
   nvec <- sort(str_split(rx_list$Drug[di], "[ ]{0,1}\\+[ ]{0,1}")[[1]])
   #iterate on individual drug names
   for( dr in nvec ){
     #get just the first observation for drug in question
     dr_data <- drug_list %>% filter(preferred_name == dr) %>% slice(1)
     #handle case of drug not in list
     if(nrow(dr_data) == 0){
       warning("Ignoring unknown drug named :: ", dr)
       next()
     }
     #handle ici_inhibitors first
     if( dr_data$is_ici_inhibitor ){
       rx_list$ICI_Rx[di] <- ifelse(rx_list$ICI_Rx[di] == "None", dr_data$preferred_name, paste(rx_list$ICI_Rx[di],dr_data$preferred_name, sep=" + "))
       rx_list$ICI_Tx[di] <- TRUE       
       rx_list$ICI_Pathway[di] <- ifelse(dr_data$is_CTLA4, "CTLA4", "PD1")
       rx_list$ICI_Target[di] <- dr_data$ici_pathway
     }else{
       rx_list$Non_ICI_Rx[di] <- ifelse(rx_list$Non_ICI_Rx[di] == "None", dr_data$preferred_name, paste(rx_list$Non_ICI_Rx[di], dr_data$preferred_name, sep=" + "))
       rx_list$Non_ICI_Tx[di] <- TRUE
     }
     #handle drug classes using the classes look up table ( since drug_list column names aren't the same as rx_list columns )
     for( dcli in 1:length(classes) ){
       rx_col <- paste(names(classes[dcli]),"Rx", sep="_")
       tx_col <- paste(names(classes[dcli]), "Tx", sep="_")
       #if this drug matches the current class, update the respective Rx and Tx fields
       if(dr_data[[classes[dcli]]]){
         rx_list[[rx_col]][di] <- ifelse(rx_list[[rx_col]][di] == "None", dr_data$preferred_name, paste(rx_list[[rx_col]][di], dr_data$preferred_name, sep=" + "))
         rx_list[[tx_col]][di] <- TRUE
       }
     }
   }
   #handle rare case where there were both a PD1 and a CTLA4 inhibitor used
   rx_list$aCTLA4_aPD1_Tx[di] <- rx_list$aCTLA4_Tx[di] & rx_list$aPD1_Tx[di]
  }
  # 
  return(rx_list)
}
  
#x = set_rx_tx(test_df)
default_unknown_Rx = "Unknown"
default_unknown_Tx = NA
default_known_Rx = "None"
default_known_Tx = F
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# init_prior_and_subsq_rx_tx
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Initializes prior and subsq Rx and Tx fields
#' 
#' @description 
#' This method sets the initial values for the various Rx and Tx fields for 
#' Prior and Subsq treatments
#' 
#' @param rx_list Dataframe to add columns to
#' @param prior_is_known Whether we want to initialize prior values as 
#' unknown ("Unknown", NA) or known ("None", FALSE). Defaults to FALSE ( unkonwn )
#' @param subsq_is_known Whether we want to initialize subsq values as 
#' unknown ("Unknown", NA) or known ("None", FALSE). Defaults to FALSE ( unkonwn )
#' 
#' @return Returns dataframe with columns
#' 
#' @export
init_prior_and_subsq_rx_tx = function(dat, prior_is_known=FALSE, subsq_is_known=FALSE){
  pr_Rx = ifelse( prior_is_known, default_known_Rx, default_unknown_Rx )
  pr_Tx = ifelse( prior_is_known, default_known_Tx, default_unknown_Tx )
  sq_Rx = ifelse( subsq_is_known, default_known_Rx, default_unknown_Rx )
  sq_Tx = ifelse( subsq_is_known, default_known_Tx, default_unknown_Tx )
  dat %<>% mutate(Prior_Rx=pr_Rx, 
                  Prior_ICI_Rx=pr_Rx,
                  Prior_Tx=pr_Tx,
                  Prior_ICI_Tx=pr_Tx,
                  Prior_aCTLA4_Tx=pr_Tx,
                  Prior_aMAPK_Tx=pr_Tx
                  )
  
  dat %<>% mutate(Subsq_Rx=sq_Rx,
                  Subsq_ICI_Rx=sq_Rx,
                  Subsq_Tx=sq_Tx,
                  Subsq_ICI_Tx=sq_Tx,
                  Subsq_aCTLA4_Tx=sq_Tx,
                  Subsq_aMAPK_Tx=sq_Tx,
                  Subsq_aCTLA4_aPD1_Tx=sq_Tx
                  )
  return(dat)
}

init_prior_and_subsq_rx_tx(data.frame(Patient_ID=c(1:6)), TRUE, TRUE)