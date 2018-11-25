#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Preprocess network data.
#' @param minimize Logical, all unecessary slots are removed
#' @param lq Logical, prepare the network for NPA computation
#' @include getLQ.R
#' @include minimizeNetwork.R
#' @export
#' @return A NULL value.
#' @examples 
#' ## This example processes the package models to make them minimal
#' preprocessNetworks(minimize = TRUE, lq = FALSE)
preprocessNetworks <- function(minimize = FALSE, lq = TRUE) {
    folder <- path.package("NPAModels")
    datalist_fp <- file.path(folder, 'data', 'datalist')
    precomputed_fp <- file.path(folder, 'resources', 'precomputed')
    if (!file.exists(precomputed_fp)) {
        precomputed_fp  <- file.path(folder, 'inst', 'resources', 'precomputed')
    }
    message("Checking for networks to process...")

    datalist <- readLines(datalist_fp)
    precomputed <- readLines(precomputed_fp)
    new_computed <- NULL
    to_process  <- datalist[is.na(match(datalist, precomputed))]
    for(f in to_process) {
        message(paste0("Loading file: ", f))
        network_fp <- file.path(folder, 'data', f)
        net_name <- load(network_fp)
        net <- get(net_name)
        if (minimize) {
            message(paste0("Minimizing network: ", net_name))
            net <- minimizeNetwork(net)
        }
        if (lq) {
            message(paste0("Processing network: ", net_name))
            net <- getLQ(net, verbose=FALSE, pattern.gene = "^EXP\\(")
            new_computed  <- c(new_computed, paste0(net_name, '.rda'))
        }
        assign(net_name, net)
        save(list=net_name, file = network_fp, compress = "xz")
        rm(net)
        gc(reset = TRUE)
        message("Done.")
    }
    if (lq) {
        write(new_computed, file=precomputed_fp, append=TRUE)
    }
    message("All networks are ready to be scored.")
    return(NULL)
}
