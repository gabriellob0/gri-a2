# Checks whether packages are installed, install uninstalled packages, and load
# all packages. Takes a vector of strings with packages names as an input, and
# returns invisibly.

load_packages <- function(packages = c("dplyr")) {
  
  is_installed <- function(pack) {
    
    #checks if package is installed
    test <- length(nzchar(find.package(package = pack, quiet = TRUE)))
    
    return(test == 1)
  }
  
  #vector with uninstalled packages
  uninstalled <- packages[!c(sapply(packages, is_installed))]
  
  #install uninstalled packages and load all dependencies
  install.packages(uninstalled)
  invisible(lapply(packages, library, character.only = TRUE))
}