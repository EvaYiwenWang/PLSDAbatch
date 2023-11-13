#' Sponge \emph{A. aerophoba} study
#'
#' This study investigated the relationship between metabolite concentration
#' and microbial abundance of specific sponge tissues. The samples were
#' collected from two types of tissues (Ectosome vs. Choanosome) and processed
#' on two separate denaturing gradient gels in electrophoresis. This study
#' includes relative abundance data only and a completely balanced
#' batch x treatment design.
#'
#'
#' @name sponge_data
#' @docType data
#' @usage data('sponge_data')
#' @format A TreeSummarizedExperiment object containing the relative
#' abundance (Tss_value) and centered log ratio transformed values (Clr_value)
#' of 32 samples and 24 OTUs. The rowData includes \code{Y.trt} and
#' \code{Y.bat}. \code{Y.trt} is the effect of interest, which is a factor of
#' sponge tissues for each sample in the sponge study; \code{Y.bat} is the
#' batch effect, which is a factor of electrophoresis gels where each sample
#' processed. The rowTree is built based on the \code{Y.bat}.
#'
#'
#' @return None.
#' @references
#' \insertRef{sacristan2011exploring}{PLSDAbatch}
#' @source The raw data were downloaded from the referenced article. Filtering
#' and normalisation described in
#' https://evayiwenwang.github.io/PLSDAbatch_workflow/.
#' @keywords datasets
NULL
