# library("readr")
# library("dplyr")
# library("stringr")

test_df = data.frame(Patient_ID=c(1:2), Drug=c("ipi+pembro", "Vemurafenib + Atezolizumab + Dabrafenib"))#, "nivo + Binimetinib + ipi", "Binimetinib"))#, "Pembro", "Ecorafenib", "Vinblastine","Ramucrimab", "Atezolizumab", "Atezolizumab + Pembro"))

drug_path = paste(find_folder_along_path(housekeeping::get_script_dir_path(include_file_name = F), "inst"), "rx_list", "rx_list.tsv", sep="/")
drug_list = readr::read_tsv(drug_path, show_col_types=F)

#drug_list = readr::read_tsv(system.file(file.path("rx_list", "rx_list.tsv"), package = "datasetprep"), show_col_types=F)
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
#' @param dat Dataframe containing " + " separated Drugs as Drug variable
#' 
#' @section Limitations:
#' \itemize{
#'   \item Does not work for prior or subsq fields.
#'   \item Does not set NeoICI_Tx
#'   \item Does not handle non-Rx treatments like Surgery, Radiation, etc.
#'}
#' 
#' @return Returns dataframe with updated and added *_Rx and *_Tx columns
#' 
#' @export
set_rx_tx = function( dat ){
  #normalize drug names, replace full names with preferred names and normalized case
  for( rxi in 1:nrow(drug_list) ){
    dat$Drug %<>% gsub( drug_list$preferred_name[rxi], drug_list$preferred_name[rxi], ., ignore.case=TRUE ) %>%
                      gsub( drug_list$full_name[rxi], drug_list$preferred_name[rxi], ., ignore.case=TRUE ) 
  }

  # initialize all fields to None, Unknown or NA
  dat$ICI_Pathway = "None" #whats the target pathway
  dat$ICI_Target = "None" #whats the target receptor
  dat$ICI_Rx = "Unknown"
  dat$ICI_Tx = NA
  dat$Non_ICI_Rx = "Unknown"
  dat$Non_ICI_Tx = NA
  dat[,paste(names(classes),"Rx", sep="_")] = "Unknown"
  dat[,paste(names(classes),"Tx", sep="_")] = NA
  dat$aCTLA4_aPD1_Tx = NA

  #iterate over drugs in patient records
  for( di in 1:nrow(dat) ){
#    print(paste("For patient ", di))
    #if drug field is NA or NULL, continue to next record
    if(is.na(dat$Drug[di]) | is.null(dat$Drug[di])) next()
    #if drug field is anything other than NA or NULL, assume we know the information for this patient
    #so update the _Rx fields to "None" and _Tx fields to FALSE
    dat$ICI_Rx[di] <- "None"
    dat$ICI_Tx[di] <- FALSE
    dat[di,paste(names(classes),"Rx",sep="_")] <- "None"
    dat[di,paste(names(classes),"Tx",sep="_")] <- FALSE
    dat$aCTLA4_aPD1_Tx[di] = FALSE
    dat$Non_ICI_Rx[di] = "None"
    dat$Non_ICI_Tx[di] = FALSE
   #split on + to get all individual drugs
   nvec <- sort(stringr::str_split(dat$Drug[di], "[ ]{0,1}\\+[ ]{0,1}")[[1]])
   #iterate on individual drug names
   ici_p="None"
   ici_t="None"
   for( dr in nvec ){
     #get just the first observation for drug in question
     dr_data <- drug_list %>% dplyr::filter(preferred_name == dr) %>% slice(1)
     #handle case of drug not in list
     if(nrow(dr_data) == 0){
       warning("Ignoring unknown drug named :: ", dr)
       next()
     }
     #handle ici_inhibitors first
     if( dr_data$is_ici_inhibitor ){
       dat$ICI_Rx[di] <- ifelse(dat$ICI_Rx[di] == "None", dr_data$preferred_name, paste(dat$ICI_Rx[di],dr_data$preferred_name, sep=" + "))
       dat$ICI_Tx[di] <- TRUE
       #This is super clunky but I was really struggling to get the primary variable to update via c() so I gave up and did it this way
       drug_pathway = ifelse(dr_data$is_CTLA4, "CTLA4", "PD1")
       drug_target = dr_data$ici_pathway
       if( ici_p[1] == "None" ){
         ici_p <- c(drug_pathway)
       }else{
         ici_p <- c(ici_p, drug_pathway)
       }
       if( ici_t[1] == "None" ){
         ici_t <- c(drug_target)
       }else{
         ici_t <- c(ici_t, drug_target)
       }
     }else{
       dat$Non_ICI_Rx[di] <- ifelse(dat$Non_ICI_Rx[di] == "None", dr_data$preferred_name, paste(dat$Non_ICI_Rx[di], dr_data$preferred_name, sep=" + "))
       dat$Non_ICI_Tx[di] <- TRUE
     }
     #handle drug classes using the classes look up table ( since drug_list column names aren't the same as dat columns )
     for( dcli in 1:length(classes) ){
       rx_col <- paste(names(classes[dcli]),"Rx", sep="_")
       tx_col <- paste(names(classes[dcli]), "Tx", sep="_")
       #if this drug matches the current class, update the respective Rx and Tx fields
       if(dr_data[[classes[dcli]]]){
         dat[[rx_col]][di] <- ifelse(dat[[rx_col]][di] == "None", dr_data$preferred_name, paste(dat[[rx_col]][di], dr_data$preferred_name, sep=" + "))
         dat[[tx_col]][di] <- TRUE
       }
     }
   }
   #combine ICI_Target and ICI_Pathway values here to eliminate any duplicate pathways
   dat$ICI_Pathway[di] = paste(sort(unique(ici_p)), collapse=" + ")
   dat$ICI_Target[di] = paste(sort(unique(ici_t)), collapse=" + ")
   #handle rare case where there were both a PD1 and a CTLA4 inhibitor used
   dat$aCTLA4_aPD1_Tx[di] <- dat$aCTLA4_Tx[di] & dat$aPD1_Tx[di]
  }
  # 
  return(dat)
}
  
x = set_rx_tx(test_df)
#x$aCTLA4_aPD1_Tx

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
#' @param dat Dataframe to add columns to
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
  dat %<>% dplyr::mutate(Prior_Rx=pr_Rx, 
                  Prior_ICI_Rx=pr_Rx,
                  Prior_Tx=pr_Tx,
                  Prior_ICI_Tx=pr_Tx,
                  Prior_aCTLA4_Tx=pr_Tx,
                  Prior_aMAPK_Tx=pr_Tx
                  )
  
  dat %<>% dplyr::mutate(Subsq_Rx=sq_Rx,
                  Subsq_ICI_Rx=sq_Rx,
                  Subsq_Tx=sq_Tx,
                  Subsq_ICI_Tx=sq_Tx,
                  Subsq_aCTLA4_Tx=sq_Tx,
                  Subsq_aMAPK_Tx=sq_Tx,
                  Subsq_aCTLA4_aPD1_Tx=sq_Tx
                  )
  return(dat)
}

#init_prior_and_subsq_rx_tx(data.frame(Patient_ID=c(1:6)), TRUE, TRUE)