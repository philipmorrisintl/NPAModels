#####################################################################
## Copyright 2018 Philip Morris Products, S.A.
## Quai Jeanrenaud 5, 2000 Neuchatel, Switzerland
#####################################################################

#' Get precomputed matrix for fast NPA computations
#'
#' @param Model A R list object with slot model containing nodes and edges
#' data.frame
#' @param verbose A logical. Default is \code{TRUE}, messages will be displayed
#' during the function execution
#' @param pattern.gene A character vector. Regular expression for filtering
#' downstrean genes
#' @return A R list object with computed metric like L2, L3 matrices,
#' Qbackbone, ...
#' @importFrom methods is
#' @export
#' @examples
#' data(Hs__CFA__Apoptosis__0__0__1) 
#' # Hs__CFA__Apoptosis__0__0__1 <- getLQ(
#' #    Hs__CFA__Apoptosis__0__0__1, verbose = TRUE)

getLQ <- function(Model, verbose = TRUE, pattern.gene = "^EXP\\(") {
    # model$edges
    sourcenode.index <- 1
    targetnode.index <- 3
    signedge.index <- 2
    neg.rel <- c(
        "directlyDecreases",
        "decreases",
        "negativeCorrelation",
        "|-cw-|",
        "=|",
        "-|",
        "-neg-",
        "-1"
        )
    perm <- function(x) {
        sample(x, length(x), replace = FALSE)
    }
    permL3 <- function(L3, seed = 145) {
        L3perm <- L3
        ltmp <- L3perm
        diag(ltmp) <- 0
        diagL <- diag(L3perm) - rowSums(abs(ltmp))
        diag(L3perm) <- 0
        L3perm[upper.tri(L3perm)] <- 0
        ## Removed, this should be done outside the function
        # set.seed(seed)
        L3perm[lower.tri(L3perm)] <- perm(L3perm[lower.tri(L3perm)])
        L3perm <- L3perm + t(L3perm)
        for (k in which(rowSums(abs(L3perm)) == 0)) {
            ## Removed, this should be done outside the function
            # set.seed(seed + 12 * k)
            add <- sample(c(1:nrow(L3perm))[-k], 1)
            L3perm[k, add] <- 1
            L3perm[add, k] <- 1
        }  #ensure no isolated nodes
        diag(L3perm) <- rowSums(abs(L3perm)) + diagL
        all(L3perm == t(L3perm))
        return(L3perm)
    }
    getSym <- function(a) {
        d0 <- diag(a)
        a <- a + t(a)
        # a[abs(a) > 1] = 1
        diag(a) <- d0
        return(a)
    }
    getSignedAdj <- function(E1, symmetric = TRUE) {
        nds <- sort(unique(as.vector(E1[, c(1, 2)])))
        A <- tapply(as.numeric(E1[, 3]), list(
            factor(E1[, 1], levels = nds),
            factor(E1[,2], levels = nds)),
            sum)
        A[is.na(A)] <- 0
        A[abs(A) > 1] <- sign(A[abs(A) > 1])
        if (symmetric == TRUE & !all(A == t(A))) {
            A <- getSym(A)
        }
        return(A)
    }
    # Some time some nodes are downstream, but not EXP()
    Model$startNodeDown <- lapply(Model$startNodeDown, function(x) {
        y <- x[grep(pattern.gene, as.character(x$nodeLabel), perl = TRUE), ]
        y <- y[y$Direction != 0, ]
        return(y)
    })
    # Check no EXP() in the backbone
    geneinback <- grep(pattern.gene, unique(as.vector(as.matrix(
        Model$model$edges[,c(sourcenode.index, targetnode.index)]))),
        perl = TRUE)
    if (length(geneinback) > 0) {
        print(geneinback)
        stop(paste("Some backbone nodes are genes", pattern.gene))
    }
    # Get signed backbone edges
    Ebackbone <- Model$model$edges[, c(sourcenode.index, signedge.index,
        targetnode.index)]
    dire <- rep(1, nrow(Ebackbone))
    dire[Ebackbone[, 2] %in% neg.rel] <- -1
    Ebackbone <- data.frame(Ebackbone[, c(1, 3)], Direction = dire)
    Ebackbone <- Ebackbone[order(Ebackbone[, 2]), ]
    Ebackbone <- Ebackbone[order(Ebackbone[, 1]), ]
    Ebackbone <- unique(Ebackbone)  #if duplicated edges...
    # Get edges from functional layer to transcriptional layer
    dhyp <- NULL
    Ehyp <- NULL
    for (k in 1:length(Model$startNodeDown)) {
        Ehyp <- rbind(Ehyp, cbind(rep(names(Model$startNodeDown)[k],
            nrow(Model$startNodeDown[[k]])),
            as.character(Model$startNodeDown[[k]]$nodeLabel)))
        dhyp <- c(dhyp,
            Model$startNodeDown[[k]]$Direction/nrow(Model$startNodeDown[[k]]))
        #normalize edges to transcript
    }
    Edown <- cbind(Ehyp, Direction = dhyp)
    colnames(Edown) <- colnames(Ebackbone)
    E00 <- rbind(Ebackbone, Edown)
    Ad <- getSignedAdj(as.matrix(E00), symmetric = TRUE)
    if (verbose == TRUE) {
        message(paste("Graph size=", nrow(Ad)))
    }
    L <- diag(rowSums(abs(Ad))) - Ad
    # Q = diag(rowSums(abs(Ad)))+ Ad
    #not needed:save memory Get indices of transript layer
    downgene <- grep(pattern.gene, rownames(L), perl = TRUE)
    # Get key matrices
    L2 <- L[downgene, -downgene]
    L3 <- L[-downgene, -downgene]
    L3inv <- try(solve(L3))
    if (is(class(L3inv), "try-error")) {
        svL3 <- svd(L3)
        lambdainv <- rep(0, length(svL3$d))
        lambdainv[abs(svL3$d) > 1e-13] <- 1/svL3$d[abs(svL3$d) > 1e-13]
        L3inv <- svL3$v %*% diag(lambdainv) %*% t(svL3$u)
        rownames(L3inv) <- rownames(L3)
        colnames(L3inv) <- colnames(L3)
    }
    L3invtL2 <- L3inv %*% t(L2)
    notDown <- which(rownames(L) %in% rownames(L3))
    if (!all(sort(c(notDown, downgene)) == 1:nrow(L))) {
        stop("wrong indexes...")
    }
    # Q
    Qbackbone <- -L3  #Q[notDown, notDown]
    # Recompute the diag:
    if (length(notDown) == 1) {
        Qbackbone <- matrix(Qbackbone, ncol = 1)
        rownames(Qbackbone) <- colnames(Qbackbone) <- rownames(L)[notDown]
    }
    diag(Qbackbone) <- 0
    diag(Qbackbone) <- rowSums(abs(Qbackbone))
    if (nrow(Qbackbone) == 1) {
        diag(Qbackbone) <- 1
    }
    NetSize <- nrow(Ebackbone)
    # Prepare for K-stat
    sqrtMat <- function(A) {
        sv <- svd(A)
        if (!all(sv$d > 0)) {
            stop("Matrix not positive definite")
        }
        Asqrt <- sv$u %*% sqrt(diag(sv$d)) %*% t(sv$v)
        return(Asqrt)
    }
    Qb.sqrt <- sqrtMat(Qbackbone)
    b <- 500
    ## Removed, this should be done outside the function
    # set.seed(2674)
    QbL3inv.perm <- lapply(1:b, function(i) {
        l3 <- permL3(L3, seed = i + 858)
        ql3inv <- try(-Qb.sqrt %*% solve(l3), silent = TRUE)
        if (inherits(ql3inv, "try-error")) {
            ql3inv <- matrix(NA, nrow(L3), ncol(L3))
        }
        return(ql3inv)
    })
    return(list(
                Qbackbone = Qbackbone,
                L3invtL2 = L3invtL2,
                L2 = L2,
                L3 = L3,
                QbL3inv.perm = QbL3inv.perm,
                sgn = Ebackbone[, 3],
                NetSize = NetSize,
                backbone = Ebackbone[, c(1, 3, 2)],
                startNodeDown = Model$startNodeDown,
                g = Model$g))
}
