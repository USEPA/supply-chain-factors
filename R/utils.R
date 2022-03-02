#' Install useeior (via devtools) for specified SEF version.
#' @param SEF_version A string of SEF version, e.g. "v1.1.1".
install_useeior <- function(SEF_version) {
  versioning <- configr::read.config("Versioning.yml")
  useeior_version <- versioning[[SEF_version]][["useeior"]]
  if (!"devtools"%in%installed.packages()[, "Package"]) {
    install.packages("devtools")
  }
  devtools::install_github(paste0("USEPA/useeior@", useeior_version))
}
