#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Retrieve the list of rda files
#'
#' @importFrom tools file_path_sans_ext
#' @param pattern A regex value for listing the rda files
#'
#' @return Vector of strings, list of data files without extension
#' @export
#'
list_data <- function(pattern='*.rda') {
  data_path <- system.file('data', package='NPAModels')
  rda_files <- list.files(data_path,pattern = pattern)
  return(tools::file_path_sans_ext(rda_files))
}

#' Retrieves the list of available families
#'
#' @param species A string, species name. Default is `Hs` for Homo sapiens
#'
#' @return Vector of strings, the list of families
#' @export
#'
list_families <- function(species='Hs') {
  l <- list_data(pattern=paste0(species, '__'))
  splitted <- strsplit(l, '__')
  families <- unlist(lapply(splitted, function(x) x[2]))
  return(unique(families))
}

#' Retrieves the list of models
#'
#' @param species A string, species name
#' @param family  A string, the familiy name, if `NULL`, all available models
#' are listed.
#'
#' @return Vector of strings, the list of model names
#' @export
#'
list_models <- function(species='Hs', family=NULL) {
  l <- list_data(pattern=paste0(species, '__', family))
  splitted <- strsplit(l, '__')
  models <- unlist(lapply(splitted, function(x) x[3]))
  return(unique(models))
}
