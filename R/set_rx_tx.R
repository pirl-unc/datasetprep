# library("readr")
# library("dplyr")
# library("stringr")

#test_df = data.frame(Patient_ID=c(1:5), Drug=c(NA, "Ipilimumab+pembro", "Vemurafenib + Atezolizumab + Dabrafenib", " ", "Vemurafenib  + Atezolizumab+ Dabrafenib"))#, "nivo + Binimetinib + ipi", "Binimetinib"))#, "Pembro", "Ecorafenib", "Vinblastine","Ramucrimab", "Atezolizumab", "Atezolizumab + Pembro"))

#drug_path = paste(find_folder_along_path(housekeeping::get_script_dir_path(include_file_name = F), "inst"), "rx_list", "rx_list.tsv", sep="/")
#drug_list = readr::read_tsv(drug_path, col_types=readr::cols(full_name="c",preferred_name="c", is_ici_inhibitor="l", ici_pathway="c", is_aVEGF="l", is_aBRAF="l", is_aMAPK="l", is_chemo="l", name_aliases="c", description="c" ))

drug_list = readr::read_tsv(system.file(file.path("rx_list", "rx_list.tsv"), package = "datasetprep"), 
                            col_types=readr::cols(full_name="c",preferred_name="c", is_ici_inhibitor="l",
                            ici_pathway="c", is_aVEGF="l", is_aBRAF="l", is_aMAPK="l", is_chemo="l",
                            name_aliases="c", description="c" ))
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
#'
set_rx_tx = function( dat ){
  #normalize drug names, using [] instead of $Drug to avoid copying entire df
  dat[,"Drug"] <- dat$Drug %>% stringr::str_trim() %>% gsub( "[ ]*\\+[ ]*", "+", .) %>% stringr::str_to_lower()
  #convert ""'s to NA's
  dat[,"Drug"][dat$Drug == ""] <- NA
  #tmp data.frame initialized with "None" and FALSE
  tmp_df <- data.frame( Drug=levels(factor(dat$Drug)), ICI_Pathway="None", ICI_Target="None",
                       ICI_Rx="None", ICI_Tx=FALSE, Non_ICI_Rx="None", Non_ICI_Tx=FALSE, aCTLA4_aPD1_Tx=FALSE )
  tmp_df[,paste(names(classes), "Rx", sep="_")] <- "None"
  tmp_df[,paste(names(classes), "Tx", sep="_")] <- FALSE
  #unfortunately NA's don't show up in levels so if there are any NA's, add a row for them now with default "Unknown" and NA
  if(any(is.na(dat$Drug))){
    tmp_df <- rbind(tmp_df, tmp_df[1,])
    tmp_df[nrow(tmp_df), "Drug"] = NA
    tmp_df[nrow(tmp_df), c(paste(names(classes),"Rx", sep="_"), "ICI_Rx", "Non_ICI_Rx")] = "Unknown"
    tmp_df[nrow(tmp_df), c(paste(names(classes),"Tx", sep="_"), "ICI_Tx", "Non_ICI_Tx","aCTLA4_aPD1_Tx")] = NA
  }
  #replace full_names with preferred_names in duplicated column so we can
  #preserve original Drug column for future back-merge with dat
  tmp_df %<>% dplyr::mutate(p_name=Drug)
  for( rxi in 1:nrow(drug_list) ){
    tmp_df[,"p_name"] <- tmp_df[,"p_name"] %>% gsub( drug_list$preferred_name[rxi], drug_list$preferred_name[rxi], ., ignore.case=TRUE ) %>%
                              gsub( drug_list$full_name[rxi], drug_list$preferred_name[rxi], ., ignore.case=TRUE ) 
  }
  #iterate over unique drug combinations
  for( di in 1:nrow(tmp_df) ){
    #if drug field is NA or NULL, continue to next record
    if(is.na(tmp_df$p_name[di]) | is.null(tmp_df$p_name[di])) next()
    #split on + to get all individual drugs
    nvec <- sort(stringr::str_split(tmp_df$p_name[di], "[ ]{0,1}\\+[ ]{0,1}")[[1]])
    #iterate on individual drug names
    ici_p="None"
    ici_t="None"
    for( dr in nvec ){
      #get just the first observation for drug in question
      dr_data <- drug_list %>% dplyr::filter(preferred_name == dr) %>% dplyr::slice(1)
      #handle case of drug not in list
      if(nrow(dr_data) == 0){
        warning("Ignoring unknown drug named :: ", dr)
        next()
      }
      #handle ici_inhibitors first
      if( dr_data$is_ici_inhibitor ){
        tmp_df$ICI_Rx[di] <- ifelse(tmp_df$ICI_Rx[di] == "None", dr, paste(tmp_df$ICI_Rx[di],dr, sep=" + "))
        tmp_df$ICI_Tx[di] <- TRUE
        #This is super clunky
        drug_pathway = ifelse(dr_data$is_CTLA4, "CTLA4", "PD1")
        drug_target = dr_data$ici_pathway
        if( ici_p[1] == "None" ) ici_p <- drug_pathway
        else ici_p <- c(ici_p, drug_pathway)
        if( ici_t[1] == "None" ) ici_t <- c(drug_target)
        else ici_t <- c(ici_t, drug_target)
      }else{
        tmp_df$Non_ICI_Rx[di] <- ifelse(tmp_df$Non_ICI_Rx[di] == "None", dr, paste(tmp_df$Non_ICI_Rx[di], dr, sep=" + "))
        tmp_df$Non_ICI_Tx[di] <- TRUE
      }
      #handle drug classes using the classes look up table ( since drug_list column names aren't the same as tmp_df columns )
      for( dcli in 1:length(classes) ){
        rx_col <- paste(names(classes[dcli]),"Rx", sep="_")
        tx_col <- paste(names(classes[dcli]), "Tx", sep="_")
        #if this drug matches the current class, update the respective Rx and Tx fields
        if(dr_data[[classes[dcli]]]){
          tmp_df[[rx_col]][di] <- ifelse(tmp_df[[rx_col]][di] == "None", dr, paste(tmp_df[[rx_col]][di], dr, sep=" + "))
          tmp_df[[tx_col]][di] <- TRUE
        }
      }
    }
    #combine ICI_Target and ICI_Pathway values here to eliminate any duplicate pathways
    tmp_df$ICI_Pathway[di] = paste(sort(unique(ici_p)), collapse=" + ")
    tmp_df$ICI_Target[di] = paste(sort(unique(ici_t)), collapse=" + ")
    #handle rare case where there were both a PD1 and a CTLA4 inhibitor used
    tmp_df$aCTLA4_aPD1_Tx[di] <- tmp_df$aCTLA4_Tx[di] & tmp_df$aPD1_Tx[di]
  }
  #remove the p_name column as no longer needed
  tmp_df$p_name <- NULL
  #merge new columns (tmp_df) into dat
  return(merge(dat, tmp_df, by="Drug"))
}
#set_rx_tx(test_df)

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