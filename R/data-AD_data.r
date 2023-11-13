#' Anaerobic digestion study
#'
#' This study explored the microbial indicators that could improve the efficacy
#' of anaerobic digestion (AD) bioprocess and prevent its failure. The samples
#' were treated with two different ranges of phenol concentration (effect of
#' interest) and processed at five different dates (batch effect). This study
#' includes a clear and strong batch effect with an approx. balanced
#' batch x treatment design.
#'
#'
#' @name AD_data
#' @docType data
#' @usage data('AD_data')
#' @format A list containing three TreeSummarizedExperiment objects
#' \code{FullData}, \code{EgData} and \code{CorrectData}:
#' \describe{
#' \item{FullData}{A TreeSummarizedExperiment object containing the counts of 75
#' samples and 567 OTUs. The meta data information of each sample is stored in
#' the rowData, while the taxonomy of each OTU is stored in the colData.}
#' \item{EgData}{A TreeSummarizedExperiment object containing the values of 75
#' samples and 231 OTUs filtered and centered log ratio transformed from the
#' \code{FullData} with raw counts.The rowData includes \code{Y.trt} and
#' \code{Y.bat}. \code{Y.trt} is the effect of interest, which is a factor of
#' phenol concentrations for each sample in the AD study; \code{Y.bat} is the
#' batch effect, which is a factor of sample processing dates for each sample.
#' The taxonomy of each OTU is stored in the colData. The rowTree is built based
#' on the \code{Y.bat}.}
#' \item{CorrectData}{A TreeSummarizedExperiment object containing seven
#' datasets before or after batch effect correction using different methods.
#' Each assay includes 75 samples and 231 OTUs.}}
#'
#' @return None.
#' @references
#' \insertRef{chapleur2016increasing}{PLSDAbatch}
#' @source The raw data were provided by Dr. Olivier Chapleur and published at
#' the referenced article. Filtering and normalisation described in our package
#' vignette.
#' @keywords datasets
NULL
