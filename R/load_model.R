#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Retrieves an NPA model object to be used with NPA package
#'
#' @param species A string, species name. Default is `Hs`
#' @param family A string, family name. To know the list of available families,
#' use `list_families`` function.
#' @param name A string. Model name to be loaded. To know the list of available
#' models, use `list_models` function.
#' @param version A string. Model version. Default is the latest version.
#'
#' @return A NPAModel R6 class object to be used with NPA package
#' @export
#' @include utils.R
#' @examples 
#' list_models(species = "Mm", family = "CFA")
#' # m <- load_model(family = "CFA", name = "Apoptosis")
load_model <- function(species = 'Hs', family, name, version = 'latest') {
    data_name <- paste0(
        species, '__', family,
        '__', name, '__0__0__1')
    if (! data_name %in% list_data()) {
        message(paste0('Model: ', data_name, ' not found'))
        return(NULL)
        }
    envir <- get('.npa_models', envir = globalenv())
    do.call("data", list(data_name, envir=envir))
    return(invisible(NPAModel$new(data_name)))
}

#' Retrieves the list of NPAModels R6 class objects for a
#' given species
#'
#' @param species A string, species name.
#' @param verbose A logical, if TRUE, messages are shonw in the
#' console.
#' @return A list object with NPAModel R6 object per slots.
#' @export
#' @include utils.R
#' @examples 
#' list_models(species = "Mm")
#' # ms <- load_models(species = "Mm")
load_models <- function(species = 'Hs', verbose = FALSE) {
    models <- list()
    for (f in list_families(species)) {
      for (n in list_models(species, f)) {
        if (verbose) {
          message(paste0('Loading model: ', species, ' - ',
              f, ' - ', n))
        }
        model <- load_model(species, f, n)
        name <- paste0(f, ' / ', n)
        models[[name]] <- model
      }
    }
    return(models)
}