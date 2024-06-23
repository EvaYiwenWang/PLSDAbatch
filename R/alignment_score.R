#' Alignment Scores for Evaluating the Degree of Mixing Samples
#'
#' This function evaluates the degree of mixing samples from different batches
#' in the batch corrected data. It is based on the dissimilarity matrix from
#' Principal Component Analysis.
#'
#' @importFrom mixOmics pca
#' @importFrom Rdpack reprompt
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
#' @author Yiwen Wang, Kim-Anh Lê Cao
#'
#' @seealso \code{\link{Scatter_Density}}, \code{\link{box_plot}},
#' \code{\link{density_plot}} and \code{\link{partVar_plot}} as the other
#' methods for batch effect detection and batch effect removal assessment.
#'
#' @references
#' \insertRef{butler2018integrating}{PLSDAbatch}
#'
#' @examples
#' if (requireNamespace("TreeSummarizedExperiment")) {
#' library(TreeSummarizedExperiment) # for functions assays(),rowData()
#' data('sponge_data')
#' X <- assays(sponge_data)$Clr_value # centered log ratio transformed data
#' batch <- rowData(sponge_data)$Y.bat # batch information
#' names(batch) <- rownames(sponge_data)
#'
#' alignment_score(data = X, batch = batch, var = 0.95, k = 3, ncomp = 20)
#' }
#' @export
alignment_score <- function(data,
                            batch,
                            var = 0.95,
                            k = round(0.1*nrow(data)),
                            ncomp = 20){

    pca.res <- mixOmics::pca(X = data, ncomp = ncomp, scale = TRUE)
    ncomp.use <- sum(cumsum(pca.res$prop_expl_var$X) < var)
    dist.mat <- as.matrix(dist(pca.res$variates$X[,seq_len(ncomp.use)],
                                upper = TRUE, diag = TRUE))
    diag(dist.mat) <- NA

    x <- c()
    for(i in seq_len(ncol(dist.mat))){
        x[i] <- sum(batch[names(sort(dist.mat[,i])[seq_len(k)])] ==
        batch[rownames(dist.mat)[i]])
    }

    result <- 1 - (mean(x) - k/ncol(dist.mat)) /(k - k/ncol(dist.mat))
    return(result)
}
