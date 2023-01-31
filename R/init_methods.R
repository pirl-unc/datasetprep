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

