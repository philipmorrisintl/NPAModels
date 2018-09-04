#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Class providing object with methods for handling a network model
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} named NPAModel containing network model
#' @format \code{\link{R6Class}} object.
#' @field data A R list object containing model data
#' @field species A string, species name
#' @field family  A string, family name
#' @field version A string, version of the model

NPAModel <- R6Class(
  "NPAModel",
  private = list(
    species = NA,
    family = NA,
    name = NA,
    version = NA,
    data_name = NA
  ),
  public = list(
    initialize = function(data_name) {
      spl <- unlist(strsplit(data_name, "__"))
      private$data_name <- data_name
      private$species <- spl[1]
      private$family <- spl[2]
      private$name <- spl[3]
      private$version <-
        paste0(spl[4], '.', spl[5], '.', spl[6])
    },
    get_species = function() private$species,
    get_family = function() private$family,
    get_name = function() private$name,
    get_version = function() private$version,
    get_data = function() get(x = private$data_name, envir = .npa_models)
  ),
  lock_class = TRUE
)
setOldClass(c("NPAModel", "R6"))


# Enabling modification of class definition
NPAModel$unlock()


#==============================================================================
# R6 Method: Pretty print of the NPAModel object
#==============================================================================
NPAModel$set("public", "print", function(...) {
  "Pretty printing for a NPA R6 class object"
  s <- "NPAModel Object:"
  s <- paste(s, "- Species:", private$species)
  s <- paste(s, "- Family:", private$family)
  s <- paste(s, "- Name:", private$name)
  s <- paste(s, "- Version:", private$version)
  print(s)
  invisible(self)
})

# Locking class to disable method or field modification
NPAModel$lock()
