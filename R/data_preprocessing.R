#' Prefiltering for Microbiome Count Data
#'
#' This function prefilters microbiome count data to remove samples or microbial
#' variables with very low abundance.
#'
#' @param data A numeric matrix or data frame with samples in rows and
#' variables in columns.
#' @param keep.spl Numeric, the minimum total count of a sample to be kept.
#' Samples with a total count smaller than \code{keep.spl} are removed.
#' Default is \code{10}.
#' @param keep.var Numeric, the minimum percentage (between \code{0} and
#' \code{100}) of total counts a variable must contribute to be kept.
#' For example, \code{keep.var = 0.01} keeps variables that account for
#' at least \code{0.01\%} of the total counts. Default is \code{0.01}.
#'
#' @return \code{PreFL} returns a list that contains the following components:
#' \item{data.filter}{The filtered data matrix.}
#' \item{sample.idx}{The indices of samples kept.}
#' \item{var.idx}{The indices of variables kept.}
#' \item{zero.prob.before}{The proportion of zeros in the input data.}
#' \item{zero.prob.after}{The proportion of zeros after filtering.}
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @references
#' \insertRef{le2016mixmc}{PLSDAbatch}
#'
#' @export
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data("AD_data")
#'
#'     ad.count <- SummarizedExperiment::assays(AD_data$FullData)$Count
#'     ad.filter.res <- PreFL(data = ad.count)
#'
#'     ad.filter <- ad.filter.res$data.filter
#'     ad.zero.before <- ad.filter.res$zero.prob.before
#'     ad.zero.after <- ad.filter.res$zero.prob.after
#' }
#'
PreFL <- function(data,
                  keep.spl = 10,
                  keep.var = 0.01) {
    # Coerce to numeric matrix
    if (!is.matrix(data)) {
        data <- as.matrix(data)
    }
    if (!is.numeric(data)) {
        stop("'data' must be a numeric matrix or a numeric data frame.")
    }

    # Zero proportion before filtering
    zero.prob.before <- mean(data == 0, na.rm = TRUE)

    # Sample filtering: total counts >= keep.spl
    sample.idx <- which(rowSums(data, na.rm = TRUE) >= keep.spl)
    if (length(sample.idx) == 0L) {
        stop("All samples were filtered out. Consider lowering 'keep.spl'.")
    }
    data <- data[sample.idx, , drop = FALSE]

    # Variable filtering: percentage of total counts >= keep.var
    total.count <- sum(data, na.rm = TRUE)
    if (total.count == 0) {
        stop("Total count is zero after sample filtering.")
    }

    var.perc <- colSums(data, na.rm = TRUE) * 100 / total.count
    var.idx <- which(var.perc >= keep.var)

    if (length(var.idx) == 0L) {
        stop("All variables were filtered out. Consider lowering 'keep.var'.")
    }

    data.out <- data[, var.idx, drop = FALSE]

    # Zero proportion after filtering
    zero.prob.after <- mean(data.out == 0, na.rm = TRUE)

    result <- list(
        data.filter      = data.out,
        sample.idx       = sample.idx,
        var.idx          = var.idx,
        zero.prob.before = zero.prob.before,
        zero.prob.after  = zero.prob.after
    )

    invisible(result)
}
