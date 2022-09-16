# The purpose of this script is to find the paths to output data if run outside of nextflow.
# If run inside nextflow everything will read and write to the working directory.
# Do not count on any libraries called here to be available in script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# init_paths
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Initializes output directory
#' 
#' @description 
#' Sets up the post processing directory and prepares temp library folder if necessary
#' 
#' @param base_dir The base directory in which the temp_data folder should be created for output
#' 
#' @return the post processing directory path that was created
#' 
#' @export
init_paths = function(base_dir){
  my_os = system("cat /etc/*-release", intern = T) %>%
    .[grepl("^ID=", .)] %>%
    gsub("ID=", "", .) %>%
    gsub("\\\"", "", .)
  
#  base_dir = dirname(housekeeping::get_script_dir_path())
  post_processing_dir = file.path(base_dir, "temp_data")
  dir.create(post_processing_dir, showWarnings = F)

    # need to detect if we are running on community rstudio or a rocker version(ie, in a custom container)
  if (my_os == "centos"){
    message("Running in CentOS. Assuming this is an Interactive Session Set-up through the OnDemand GUI.")
    # Interactive sessions allow you to use your user library so no temp is needed.
  } else if (my_os == "ubuntu"){
    message("Running in ubuntu. Assuming this is a customized container R >= 4.0")
    # singularity on the cluster prevents rstudio from changing the permission of folders which is needed to install packages,
    #  BUT we still want to be able to install packages on the fly to try them out before adding them to the dockerfile.
    #  for this reason we will make a temp_library directory
    #  If you like how the packages you installed are working you should add them to the build/Dockerfile
    temp_lib_dir = file.path(base_dir, "temp_library")
    dir.create(temp_lib_dir, showWarnings = F)
    .libPaths(temp_lib_dir)
  } else {
    warning(paste0("OS, ", my_os, " was not expected. Not sure how this is going to go. Good luck!"))
  }
  return(post_processing_dir)
}

#initialize this base path here since it doesn't ever change between datasets
#and can thus remain in the package environment
#RAW_DATA_DIR = "/datastore/nextgenout5/share/labs/Vincent_Lab/datasets"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create_input_path
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title Creates the full path to a given input file ( DEPRICATED )
#' 
#' @description 
#' Uses the RAW_DATA_DIR and the parameters passed in to create a full path to the input file being requested
#' 
#' @param dataset The name of the dataset ( which should match the folder name for this dataset in the filesystem )
#' @param filename The name of the input file
#' @param subfolder The path to the subfolder containing the input file, if any
#' 
#' @return The full path to the file being requested starting from 
#' /datastore/nextgenout5/share/labs/Vincent_Lab/datasets
#' 
#' @export
create_input_path = function( dataset, filename, subfolder=NA ){
  stop( "This method is depricated. You'll have to set your input path(s) manually. Sorry for that." )
  # if( !is.na(subfolder) ){
  #   #strip beginning and ending slashes from subfolder ...
  #   subfolder %<>% gsub("^/+", "", .) %>% gsub("/+$", "", . )
  #   return(file.path(RAW_DATA_DIR, dataset, subfolder, filename)) 
  # }else{
  #   return(file.path(RAW_DATA_DIR, dataset, filename))
  # }
}
#create_input_path("mds","data.txt")