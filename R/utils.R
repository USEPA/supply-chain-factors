#' Install useeior (via devtools) for specified SEF version.
#' @param SEF_version A string of SEF version, e.g. "v1.1.1".
install_useeior <- function(SEF_version) {
  versioning <- configr::read.config("Versioning.yml")
  useeior_tag <- versioning[[SEF_version]][["useeior_tag"]]
  useeior_ver <- versioning[[SEF_version]][["useeior_ver"]]
  installed_pkg <- installed.packages()
  if (!"devtools"%in%installed_pkg[, "Package"]) {
    install.packages("devtools")
  }
  if (!"useeior"%in%installed_pkg[, "Package"]) {
    devtools::install_github(paste0("USEPA/useeior@", useeior_tag))
  }
  if ("useeior"%in%installed_pkg[, "Package"] && useeior_ver!=installed_pkg[installed_pkg[, "Package"]=="useeior", "Version"]) {
    devtools::install_github(paste0("USEPA/useeior@", useeior_tag))
  }
}
