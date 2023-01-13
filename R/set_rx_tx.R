# library("stringr")

#test_df = data.frame(Patient_ID=c(1:5), Run_ID=LETTERS[1:5], Non_ICI_Rx=T, ICI_Rx=c(NA, "Ipilimumab+pembro", "Vemurafenib + Atezolizumab + Dabrafenib", " ", "Vemurafenib  + Atezolizumab+ Dabrafenib"),Drug=c(NA, "Ipilimumab+pembro", "Vemurafenib + Atezolizumab + Dabrafenib", " ", "Vemurafenib  + Atezolizumab+ Dabrafenib"))#, "nivo + Binimetinib + ipi", "Binimetinib"))#, "Pembro", "Ecorafenib", "Vinblastine","Ramucrimab", "Atezolizumab", "Atezolizumab + Pembro"))

#drug_path = paste(find_folder_along_path(housekeeping::get_script_dir_path(include_file_name = F), "inst"), "rx_list", "rx_list.tsv", sep="/")
#drug_list = readr::read_tsv(drug_path, col_types=readr::cols(Full_Name="c",Preferred_Name="c", is_ici_inhibitor="l", ici_pathway="c", is_aVEGF="l", is_aBRAF="l", is_aMAPK="l", is_chemo="l", name_aliases="c", description="c" ))


# To fix in next version - from Dante #####
# check - I would think it would use Name_Aliases to match more names but those aren't used.  
# check - Some names like temo are duplicated there any way so that would need cleaning up if that were the case. 
# check - Put Timo in 'Ambiguous_Names' and warn for that?  Maybe have  bounded by pipes so it's easy to grep them.
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set_rx_tx
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Takes a data.frame with a Drug column and returns formatted variables based on the drug names
#' 
#' @description 
#' This method looks up drugs by name, normalizes them and sets the various 
#' pathway fields ( i.e. ICI_Rx, ICI_Tx, aPD1_Rx, aPD1_Tx ).
#' 
#' @param dat Dataframe containing " + " separated Drugs as Drug variable
#' @param drug_table_path Full path to the drug table to be used
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
set_rx_tx = function( 
	dat, 
	drug_table_path=system.file("rx_table", "rx_table.tsv", package="datasetprep")
){
  warning("This method has been depricated. Please use converge_drug_aliases() and lookup_drug_properties(). Effective IMMEDIATELY!")
  return()
  #define look up table for tsv file names to data variable names
  drug_classes = c("Is_PD1", "Is_CTLA4", "Is_aVEGF", "Is_aBRAF", "Is_aMAPK", "Is_Chemo")
  names(drug_classes) = c("aPD1", "aCTLA4", "aVEGF", "aBRAF", "aMAPK", "Chemo")
  
  #load the table of drugs into a data frame  
  drug_df <- read.csv(drug_table_path, sep="\t",na.strings = "")
  
  #normalize drug names, using [] instead of $Drug to avoid copying entire df
  dat[,"Drug"] <- dat$Drug %>% trimws() %>% gsub( "[ ]*\\+[ ]*", input_sep, .) %>% tolower()
  #convert ""'s to NA's
  dat[which(dat$Drug == ""),"Drug"] <- NA
  #save initial drug column position so we can move it back to the same place after addition of new columns
  drug_column_index <- which(names(dat) == "Drug")
  #tmp data.frame initialized with "None" and FALSE
  subdat_df <- data.frame( Drug=levels(factor(dat$Drug)), ICI_Pathway="None", ICI_Target="None",
                       ICI_Rx="None", ICI_Tx=FALSE, Non_ICI_Rx="None", Non_ICI_Tx=FALSE, aCTLA4_aPD1_Tx=FALSE )
  subdat_df[,paste(names(drug_classes), "Rx", sep="_")] <- "None"
  subdat_df[,paste(names(drug_classes), "Tx", sep="_")] <- FALSE
  #unfortunately NA's don't show up in levels so if there are any NA's, add a row for them now with default "Unknown" and NA
  if(any(is.na(dat$Drug))){
    subdat_df <- rbind(subdat_df, subdat_df[1,])
    subdat_df[nrow(subdat_df), "Drug"] = NA
    subdat_df[nrow(subdat_df), c(paste(names(drug_classes),"Rx", sep="_"), "ICI_Rx", "Non_ICI_Rx")] = "Unknown"
    subdat_df[nrow(subdat_df), c(paste(names(drug_classes),"Tx", sep="_"), "ICI_Tx", "Non_ICI_Tx","aCTLA4_aPD1_Tx")] = NA
  }
  #replace Full_Names with Preferred_Names in duplicated column so we can
  #preserve original Drug column for future back-merge with dat
  subdat_df$Preferred_Name <- subdat_df$Drug
  for( rx_index in 1:nrow(drug_df) ){
  	# sub while ignoring case to set the preferred case
    subdat_df[,"Preferred_Name"] %<>% gsub( drug_df$Preferred_Name[rx_index], drug_df$Preferred_Name[rx_index], ., ignore.case=TRUE ) %>%
                              gsub( drug_df$Full_Name[rx_index], drug_df$Preferred_Name[rx_index], ., ignore.case=TRUE ) 
  }
  #iterate over unique drug combinations
  for( subdat_index in 1:nrow(subdat_df) ){
  	#subdat_index = subdat_index + 1
    #if drug field is NA or NULL, continue to next record
    if(is.na(subdat_df$Preferred_Name[subdat_index]) | is.null(subdat_df$Preferred_Name[subdat_index])) next()
    #split on + to get all individual drugs
    drug_names <- sort(stringr::str_split(subdat_df$Preferred_Name[subdat_index], "[ ]{0,1}\\+[ ]{0,1}")[[1]])
    #iterate on individual drug names
    ici_pathway="None"
    ici_target="None"
    #i=0
    for( dr in drug_names ){
    	#i = i+1
    	#dr=drug_names[i]
      #look up drug in question
      drug_data <- drug_df[ drug_df$Preferred_Name == dr, ]
      #handle case of drug not in list
      if(nrow(drug_data) == 0){
        warning("Ignoring unknown drug named :: ", dr)
        next()
      }
      #get just the first observation for this drug
      drug_data <- drug_data[1,,drop=F]
      #handle ici_inhibitors first
      if( drug_data$Is_ICI ){
        subdat_df$ICI_Rx[subdat_index] <- ifelse(subdat_df$ICI_Rx[subdat_index] == "None", dr, paste(subdat_df$ICI_Rx[subdat_index],dr, sep=" + "))
        subdat_df$ICI_Tx[subdat_index] <- TRUE

        # there could be multiple pathways and targets so we'll make a vector
        # to hold them all and will combine with a " + " at the end
        # but need to make sure "None{" isn't in the there
        if (ici_pathway[1] == "None"){
        	ici_pathway = drug_data$ICI_Pathway
        } else {
        	ici_pathway = c(ici_pathway, drug_data$ICI_Pathway)
        }
        if (ici_target[1] == "None"){
        	ici_target = drug_data$ICI_Target
        } else {
        	ici_target = c(ici_target, drug_data$ICI_Target)
        }
      }else{
        subdat_df$Non_ICI_Rx[subdat_index] <- ifelse(subdat_df$Non_ICI_Rx[subdat_index] == "None", dr, paste(subdat_df$Non_ICI_Rx[subdat_index], dr, sep=" + "))
        subdat_df$Non_ICI_Tx[subdat_index] <- TRUE
      }
      #handle drug classes using the drug_classes look up table ( since drug_df column names aren't the same as subdat_df columns )
      for( class_index in 1:length(drug_classes) ){
        class_name <- names(drug_classes)[class_index]
        rx_col <- paste(class_name,"Rx", sep="_")
        tx_col <- paste(class_name, "Tx", sep="_")
        #if this drug matches the current class, update the respective Rx and Tx fields
        if(drug_data[[drug_classes[class_index]]]){
          subdat_df[[rx_col]][subdat_index] <- ifelse(subdat_df[[rx_col]][subdat_index] == "None", dr, paste(subdat_df[[rx_col]][subdat_index], dr, sep=" + "))
          subdat_df[[tx_col]][subdat_index] <- TRUE
        }
      }
    }
    #combine ICI_Target and ICI_Pathway values here to eliminate any duplicate pathways
    subdat_df$ICI_Pathway[subdat_index] = paste(sort(unique(ici_pathway)), collapse=" + ")
    subdat_df$ICI_Target[subdat_index] = paste(sort(unique(ici_target)), collapse=" + ")
    #handle rare case where there were both a PD1 and a CTLA4 inhibitor used
    subdat_df$aCTLA4_aPD1_Tx[subdat_index] <- subdat_df$aCTLA4_Tx[subdat_index] & subdat_df$aPD1_Tx[subdat_index]
  }
  #remove the Preferred_Name column as no longer needed
  subdat_df$Preferred_Name <- NULL
  #merge new columns (subdat_df) into dat
  #to avoid duplicate columns, remove all overlapping columns from dat EXCEPT Drug which is used for merge
  dat %<>% as.data.frame() #following line doesn't work if dat is a data.table data.frame which it is when coming from Gide dataset
  dat <- dat[,colnames(dat) %ni% setdiff(colnames(subdat_df), "Drug")]
  cat("Added/Modified columns: ICI_Target, ICI_Pathway, ICI_Rx, ICI_Tx, Non_ICI_Rx, Non_ICI_Tx,",paste(names(drug_classes),"Rx", sep="_", collapse=", "),", ", paste(names(drug_classes),"Tx", sep="_", collapse=", "), "and aCTLA4_aPD1_Tx.")
  dat <- merge(dat, subdat_df, by="Drug")
  dat <- dat[housekeeping::move_to_position(names(dat), "Drug", drug_column_index)]
  return(dat)
}
### test run ###
#dx1=set_rx_tx(test_df,drug_table_path = file.path(getwd(),"Desktop/work/vincent_lab/datasetprep/inst/rx_table/rx_table.tsv"))

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
#' unknown ("Unknown", NA) or known ("None", FALSE). Defaults to FALSE ( unknown )
#' @param subsq_is_known Whether we want to initialize subsq values as 
#' unknown ("Unknown", NA) or known ("None", FALSE). Defaults to FALSE ( unknown )
#' @param default_unknown_Rx Value to use when an Rx value is unknown ( default "Unknown" )
#' @param default_unknown_Tx Value to use when a Tx value is unknown ( default NA )
#' @param default_known_Rx Value to use when an Rx value is known ( default "None" )
#' @param default_known_Tx Value to use when a Tx value is known ( default F )
#' 
#' @return Returns dataframe with columns
#' 
#' @export
init_prior_and_subsq_rx_tx = function(dat, prior_is_known=FALSE, subsq_is_known=FALSE, 
                                      default_unknown_Rx = "Unknown", default_unknown_Tx = NA, 
                                      default_known_Rx = "None", default_known_Tx = F ){
  
  pr_Rx = ifelse( prior_is_known, default_known_Rx, default_unknown_Rx )
  pr_Tx = ifelse( prior_is_known, default_known_Tx, default_unknown_Tx )
  sq_Rx = ifelse( subsq_is_known, default_known_Rx, default_unknown_Rx )
  sq_Tx = ifelse( subsq_is_known, default_known_Tx, default_unknown_Tx )
  
  dat$Prior_Rx=pr_Rx
  dat$Prior_ICI_Rx=pr_Rx
  dat$Prior_Tx=pr_Tx
  dat$Prior_ICI_Tx=pr_Tx
  dat$Prior_aCTLA4_Tx=pr_Tx
  dat$Prior_aMAPK_Tx=pr_Tx

  dat$Subsq_Rx=sq_Rx
  dat$Subsq_ICI_Rx=sq_Rx
  dat$Subsq_Tx=sq_Tx
  dat$Subsq_ICI_Tx=sq_Tx
  dat$Subsq_aCTLA4_Tx=sq_Tx
  dat$Subsq_aMAPK_Tx=sq_Tx
  dat$Subsq_aCTLA4_aPD1_Tx=sq_Tx

  cat("Added/Modified columns: Prior_Rx, Prior_ICI_Rx, Prior_Tx, Prior_ICI_Tx, Prior_aCTLA4_Tx, Prior_aMAPK_Tx, Subsq_Rx, Subsq_ICI_Rx, Subsq_Tx, Subsq_ICI_Tx, Subsq_aCTLA4_Tx, Subsq_aMAPK_Tx, Subsq_aCTLA4_aPD1_Tx")
  return(dat)
}

#init_prior_and_subsq_rx_tx(data.frame(Patient_ID=c(1:6)), TRUE, TRUE)