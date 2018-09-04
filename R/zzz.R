#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

.onLoad <- function(libname, pkgname) {
    if (!exists(x = '.npa_models')) {
        assign(".npa_models", new.env(hash = TRUE), envir = .GlobalEnv)
    }
}
