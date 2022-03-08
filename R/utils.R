#' Get the SEF version specified in the yaml file
#' @return string, version name
get_SEF_version <- function() {
  versioning <- configr::read.config(here::here("Versioning.yml"))
  return(names(versioning[1])[1])
}

#' Install useeior (via devtools) for specified SEF version.
#' @param SEF_version A string of SEF version, e.g. "v1.1.1".
install_useeior <- function(SEF_version) {
  versioning <- configr::read.config(here::here("Versioning.yml"))
  useeior_tag <- versioning[[SEF_version]][["useeior_tag"]]
  useeior_ver <- versioning[[SEF_version]][["useeior_ver"]]
  installed_pkg <- installed.packages()
  if (!"devtools"%in%installed_pkg[, "Package"]) {
    install.packages("devtools")
  }
  if (!"useeior"%in%installed_pkg[, "Package"]) {
    cli::cli_alert_info("Installing useeior v{useeior_ver} (tag @{useeior_tag}) from GitHub...")
    devtools::install_github(paste0("USEPA/useeior@", useeior_tag))
  }
  installed_useeior_ver <- installed_pkg[installed_pkg[, "Package"]=="useeior", "Version"]
  if ("useeior"%in%installed_pkg[, "Package"] && useeior_ver!=installed_useeior_ver) {
    cli::cli_alert_warning(c("A new version of useeior (v{useeior_ver}) will be installed for generating SEF {SEF_version}. ",
                             "The useeior v{installed_useeior_ver} you have installed will be overwritten."))
    cli::cli_alert_info("Installing useeior v{useeior_ver} (tag @{useeior_tag}) from GitHub...")
    devtools::install_github(paste0("USEPA/useeior@", useeior_tag))
  }
}

#' Check if model IO year is consistent with year in model name. Stop if different.
#' @param modelname Name of the model from a config file.
checkModelIOYear <- function(modelname){
  model <- useeior:::initializeModel(modelname,
                                     configpaths = paste0("modelspecs/", modelname, ".yml"))
  modelname_year <- as.numeric(paste0("20", sub(".*\\.", "", substr(modelname, 1, 13))))
  if(model$specs$BaseIOLevel!="Detail" && model$specs$IOYear!=modelname_year) {
    stop(paste("Year in model name of", modelname,
               "≠ IO year in model specs. Review model specs."))
  }
  if (model$specs$BaseIOLevel=="Detail" && model$specs$IOYear!=2012) {
    stop(paste("Year in model name of", modelname, "≠ 2012. Review model specs."))
  }
}
