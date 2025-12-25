#' Alignment Scores for Evaluating the Degree of Mixing Samples
#'
#' This function evaluates the degree of mixing samples from different batches
#' in the batch corrected data. It is based on the dissimilarity matrix from
#' Principal Component Analysis.
#'
#' @importFrom mixOmics pca
#' @importFrom stats dist
#'
#' @param data A numeric matrix. Samples are in rows, while variables are in
#' columns. \code{NA}s are not allowed.
#' @param batch A factor or a class vector for the batch grouping information
#' (categorical outcome variable).
#' The length should be equal to the number of samples in the data.
#' @param var The proportion of data variance explained by
#' the principal components,
#' ranging from \code{0} to \code{1}. Default value is \code{0.95}.
#' @param k Integer, the number of nearest neighbours.
#' By default \code{10\%} of the number of samples are used.
#' @param ncomp Integer, the number of components for
#' principal component analysis.
#' Default value is \code{20}.
#'
#' @return A numeric alignment score that ranges from \code{0} to \code{1},
#' representing poor to perfect
#' performance of mixing the samples from different batches.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{Scatter_Density}}, \code{\link{box_plot}},
#' \code{\link{density_plot}} and \code{\link{partVar_plot}} as the other
#' methods for batch effect detection and batch effect removal assessment.
#'
#' @references
#' \insertRef{butler2018integrating}{PLSDAbatch}
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data("sponge_data")
#'     X <- SummarizedExperiment::assays(sponge_data)$Clr_value # centered log ratio transformed data
#'     batch <- SummarizedExperiment::rowData(sponge_data)$Y.bat # batch information
#'     names(batch) <- rownames(sponge_data)
#'
#'     alignment_score(data = X, batch = batch, var = 0.95, k = 3, ncomp = 20)
#' }
#'
#' @export
alignment_score <- function(data,
                            batch,
                            var = 0.95,
                            k = NULL,
                            ncomp = 20) {
    ## Basic checks
    if (anyNA(data)) {
        stop("`data` must not contain NA.")
    }

    n <- nrow(data)

    if (n < 2L) {
        stop("`data` must have at least 2 samples (rows) to compute an alignment score.")
    }

    if (length(batch) != n) {
        stop("`batch` must have the same length as the number of rows in `data`.")
    }

    if (nlevels(as.factor(batch)) < 2L) {
        warning("`batch` has fewer than 2 levels. Alignment score is not meaningful and NA is returned.")
        return(NA_real_)
    }

    if (!is.numeric(var) ||
        length(var) != 1L || var <= 0 || var > 1) {
        stop("`var` must be a single numeric value in (0, 1].")
    }

    if (!is.numeric(ncomp) || ncomp < 1) {
        stop("`ncomp` must be a positive integer.")
    }

    ## Handle default k more safely
    if (missing(k) || is.null(k)) {
        k <- round(0.1 * n)
    }
    k <- max(1L, min(as.integer(k), n - 1L))

    ## PCA
    pca.res <- mixOmics::pca(
        X = data,
        ncomp = min(ncomp, ncol(data), n - 1L),
        scale = TRUE
    )

    ## Choose number of components based on explained variance
    cumvar <- cumsum(pca.res$prop_expl_var$X)
    # first component where cumvar >= var; at least 1
    ncomp.use <- min(which(cumvar >= var))
    if (is.infinite(ncomp.use)) {
        # in case cumvar never reaches var, use all available components
        ncomp.use <- length(cumvar)
    }

    ## Distances in PCA space
    scores <- pca.res$variates$X[, seq_len(ncomp.use), drop = FALSE]
    dist.mat <- as.matrix(stats::dist(scores))
    ## Do not select self as neighbour
    diag(dist.mat) <- Inf

    ## Nearest neighbours for each sample (column represents a sample)
    # order returns indices of rows for each column
    ord_idx <- apply(dist.mat, 2L, order)
    # keep only the first k neighbours for each column
    if (k == 1L) {
        nn_idx <- matrix(ord_idx[1L, ], nrow = 1L)
    } else {
        nn_idx <- ord_idx[seq_len(k), , drop = FALSE]
    }

    ## Count same-batch neighbours
    batch <- as.factor(batch)
    # matrix of neighbour batch labels (rows: neighbours, cols: samples)
    nn_batches <- matrix(batch[nn_idx], nrow = k)
    # replicate focal sample's batch label across rows for comparison
    focal_batches <- matrix(rep(batch, each = k), nrow = k)
    same_batch_counts <- colSums(nn_batches == focal_batches)

    ## Alignment score (normalised)
    m <- mean(same_batch_counts)
    n_samples <- ncol(dist.mat)

    ## Original formula: 1 - (mean(x) - k/n) /(k - k/n)
    score <- 1 - (m - k / n_samples) / (k - k / n_samples)

    ## Bound score in [0, 1] in case of numerical issues
    score <- max(0, min(1, score))

    return(score)
}
