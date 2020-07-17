load.library <- function(package_name){
  if(is.character(package_name) != TRUE){
    stop("The package names need to be in a character vector.")
  }
  for(i in 1:length(package_name)){
    if(!(package_name[i]%in%installed.packages())){
      install.packages(package_name[i], repos = "http://cran.cnr.berkeley.edu/")
    }
  }
  package_statuses <- sapply(package_name, require, character.only = TRUE)
  print(package_statuses)
}