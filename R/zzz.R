.onAttach <- function(libname, pkgname){
  
  # Query version number from DESCRIPTION
  v <- utils::packageVersion("rKIN")
  
  # Startup message
  packageStartupMessage(paste("rKIN (version ", v ,")", sep=""))
  
}