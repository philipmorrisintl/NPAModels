#####################################################################
## Copyright 2021 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Exports the network backbone as tsv file
#'
#' @param net A R6 NPAModel object
#' @param output_path A string correspond to the tsv output filepath.
#' If not provided, ``backbone.tsv`` will be generated in working directory.
#' @return Nothing is returned. File is created.
#' @export

exportBackbone <- function(net, output_path=file.path(getwd(), 'backbone.tsv')) {
    net <- net$get_data()
    edges <- net$model$edges
    df <- data.frame(subject=edges$Source.Node, object=edges$Target.Node,
                     relation=edges$Direction)
    write.table(df, file=output_path, quote=FALSE, sep='\t', row.names=FALSE)
}

#' Exports the network downstreams as tsv file
#'
#' @param net A R6 NPAModel object
#' @param output_path A string correspond to the tsv output filepath.
#' If not provided, ``downstream.tsv`` will be generated in working directory.
#' @return Nothing is returned. File is created.
#' @export

exportDownstreams <- function(net, output_path=file.path(getwd(), 'downstream.tsv')) {
    net <- net$get_data()
    df <- data.frame()
    for (inode_idx in seq_along(net$startNodeDown)) {
        parent_node <- rep(names(net$startNodeDown[inode_idx]),
                           nrow(net$startNodeDown[[inode_idx]]))
        current_df <- data.frame(
            subject = parent_node,
            object = delexp(net$startNodeDown[[inode_idx]]$nodeLabel),
            relation = net$startNodeDown[[inode_idx]]$Direction
        )
        df <- rbind(df, current_df)
    }
    write.table(df, file=output_path, quote=FALSE, sep='\t', row.names=FALSE)
}



#' Utility function to remove EXP() for transcripts in downstreams dataframes
#'
#' @param net A string of vector of strings to be processed
#' @return A string or vector of strings processed.
delexp <- function(x) {
    x <- gsub(
        "^EXP\\(",
        "",
        gsub(
            "^exp\\(",
            "",
            gsub(
                "\\)$", "", as.character(x), perl=TRUE
            ),
            perl=TRUE
        ),
        perl=TRUE
    )
    x <- gsub(
        "^R\\(",
        "",
        gsub(
            "^r\\(",
            "",
            gsub(
                "\\)$", "", as.character(x), perl=TRUE
            ),
            perl=TRUE
        ),
        perl=TRUE
    )
    return(x)
}
