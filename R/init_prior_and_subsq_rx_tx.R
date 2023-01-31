
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