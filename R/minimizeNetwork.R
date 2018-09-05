#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Reduces the slots of a network object to the minimum
#' required slots for getLQ function to be applied
#' @param model List with few slots to be cleanded
#' @return Same object with some slots removed

minimizeNetwork <- function(model) {
    # First level of list content
    slots_to_keep  <- c(
                        "model",
                        "startNodeDown"
                        )
    for(slot_ in names(model)) {
        if (!slot_ %in% slots_to_keep) {
            model[[slot_]] <- NULL
        }
    }
    # Second level of data to keep in model
    for (slot_ in names(model$model)) {
        if (slot_ != "edges") {
            model$model[[slot_]] <- NULL
        }
    }
    if ('Relationship.Type' %in% names(model$model$edges)) {
        model$model$edges$Direction <- model$model$edges$Relationship.Type
    }
    cols_to_keep  <- c(
                        "Source.Node",
                        "Direction",
                        "Target.Node"
                        )
    model$model$edges <- model$model$edges[cols_to_keep]
    # Second level of data to keep in startNodeDown
    col_to_keep  <- c(
                        "nodeLabel",
                        "Direction"
                        )
    snd <- lapply(model$startNodeDown, FUN=function(X) {
                      X[col_to_keep]
                        })
    model$startNodeDown <- snd
    # Applying the getLQ to a model may add an additional
    # startNodeDown slot in the list
    if (length(which(names(model) == "startNodeDown")) > 1) {
        model$startNodeDown <- NULL
    }
    model
}

