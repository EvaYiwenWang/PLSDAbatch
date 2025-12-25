#' Partitioned Variance Plot
#'
#' This function draws a partitioned variance plot explained
#' by different sources.
#'
#' @import ggplot2
#'
#' @param prop.df A data frame that contains the proportion of variance
#' explained by different sources.
#' @param text.cex Numeric, the size of text on the plot.
#' Use the size rule of \code{ggplot2::geom_text()}.
#' @param x.angle Numeric, angle of x axis, in the range of
#' \eqn{0} to \eqn{360}.
#' @param x.hjust Numeric, horizontal justification of x axis,
#' in the range of \eqn{0} to \eqn{1}.
#' @param title Character, the plot title.
#' @param color.set A vector of characters, indicating the set of colors to use.
#' The colors are represented by hexadecimal color code.
#'
#' @return A \code{ggplot} object.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{Scatter_Density}}, \code{\link{box_plot}},
#' \code{\link{density_plot}} and \code{\link{alignment_score}} as the other
#' methods for batch effect detection and batch effect removal assessment.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("vegan", quietly = TRUE) &&
#'     requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     ## First example
#'     data("AD_data")
#'     # centered log ratio transformed data
#'     ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
#'     ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat # batch information
#'     ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt # treatment information
#'     names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
#'
#'     ad.factors.df <- data.frame(trt = ad.trt, batch = ad.batch)
#'     rda.res <- vegan::varpart(ad.clr, ~trt, ~batch,
#'         data = ad.factors.df, scale = TRUE
#'     )
#'
#'     ad.prop.df <- data.frame(
#'         Treatment = NA, Batch = NA,
#'         Intersection = NA,
#'         Residuals = NA
#'     )
#'     ad.prop.df[1, ] <- rda.res$part$indfract$Adj.R.squared
#'
#'     ad.prop.df <- ad.prop.df[, c(1, 3, 2, 4)]
#'
#'     partVar_plot(prop.df = ad.prop.df)
#'
#'     ## Second example
#'     # a list of data corrected from different methods
#'     ad.corrected.list <- SummarizedExperiment::assays(AD_data$CorrectData)
#'     ad.prop.df <- data.frame(
#'         Treatment = NA, Batch = NA,
#'         Intersection = NA,
#'         Residuals = NA
#'     )
#'     for (i in seq_len(length(ad.corrected.list))) {
#'         rda.res <- vegan::varpart(ad.corrected.list[[i]], ~trt, ~batch,
#'             data = ad.factors.df, scale = TRUE
#'         )
#'         ad.prop.df[i, ] <- rda.res$part$indfract$Adj.R.squared
#'     }
#'
#'     rownames(ad.prop.df) <- names(ad.corrected.list)
#'
#'     ad.prop.df <- ad.prop.df[, c(1, 3, 2, 4)]
#'
#'     partVar_plot(prop.df = ad.prop.df)
#' }
#'
partVar_plot <- function(prop.df,
                         text.cex = 3,
                         x.angle = 60,
                         x.hjust = 1,
                         title = NULL,
                         color.set = NULL) {
    # Avoid NSE check notes
    Prop <- Methods <- Type <- ypos <- NULL

    # 1. Basic input checks
    if (!is.data.frame(prop.df) && !is.matrix(prop.df)) {
        stop("prop.df must be a data.frame or matrix.")
    }

    # Ensure prop.df is a data.frame
    prop.df <- as.data.frame(prop.df)

    # Add default row/column names if missing
    if (is.null(rownames(prop.df))) {
        rownames(prop.df) <- paste0("Method_", seq_len(nrow(prop.df)))
    }
    if (is.null(colnames(prop.df))) {
        colnames(prop.df) <- paste0("Type_", seq_len(ncol(prop.df)))
    }

    # 2. Convert to numeric matrix and normalise rows to sum to 1
    mat <- as.matrix(prop.df)
    rownames(mat) <- rownames(prop.df)
    mode(mat) <- "numeric"

    if (any(mat < 0, na.rm = TRUE)) {
        warning("Negative values in 'prop.df' were set to 0 and rows were renormalised.")
        mat[mat < 0] <- 0
    }

    # Row sums and normalisation to proportions
    row.sums <- rowSums(mat, na.rm = TRUE)

    if (any(row.sums <= 0)) {
        stop("Each row of 'prop.df' must have a positive total variance after clipping negatives.")
    }

    mat <- sweep(mat, 1, row.sums, "/")

    # Check for NA after processing
    if (any(is.na(mat))) {
        stop(
            "NA values detected in 'prop.df' after processing. Please remove or impute NA before plotting."
        )
    }

    # 3. Build long-format data.frame for ggplot
    rda.ggplot <- data.frame(
        Prop = c(t(mat)),
        Methods = rep(rownames(mat), each = ncol(mat)),
        Type = rep(colnames(mat), nrow(mat)),
        stringsAsFactors = FALSE
    )

    rda.ggplot$Methods <- factor(rda.ggplot$Methods, levels = rownames(mat))
    rda.ggplot$Type <- factor(rda.ggplot$Type, levels = rev(colnames(mat)))

    # Compute text positions
    rda.position <- t(apply(mat, 1, function(x) {
        h <- ifelse(x <= 0.03, 0.03, x)
        cs <- cumsum(h)
    }))
    rda.position[, 1] <- 0.06
    rda.position[, 4] <- 1

    rda.ggplot$ypos <- c(t(rda.position))

    # 5. Handle colour settings
    types <- levels(rda.ggplot$Type) # this is rev(colnames(mat))
    K <- length(types)

    if (is.null(color.set)) {
        base.cols <- pb_color(seq_len(25))

        if (K <= 25) {
            color.set <- base.cols[seq_len(K)]
        } else {
            # Repeat base colours and adjust brightness for later cycles
            cycles <- ceiling(K / 25)

            col.list <- vector("list", cycles)
            col.list[[1]] <- base.cols # first cycle (original)

            # Build remaining cycles: alternate lighten/darken
            if (cycles >= 2L) {
                for (i in seq(2L, cycles)) {
                    if (i %% 2 == 0) {
                        col.list[[i]] <- lighten(base.cols, amount = 0.15)
                    } else {
                        col.list[[i]] <- darken(base.cols, amount = 0.15)
                    }
                }
            }

            color.all <- unlist(col.list, use.names = FALSE)
            color.set <- color.all[seq_len(K)]
        }
    } else {
        # User-supplied colour vector
        if (length(color.set) < K) {
            warning("Length of 'color.set' is smaller than the number of types; recycling colours.")
            color.set <- rep(color.set, length.out = K)
        } else if (length(color.set) > K) {
            color.set <- color.set[seq_len(K)]
        }
    }

    # Align user-supplied colors to stacking order (bottom → top)
    base_order <- colnames(mat) # Treatment, Intersection, Batch, Residual
    stack_order <- rev(base_order) # Residual, Batch, Intersection, Treatment (actual stacking)

    names(color.set) <- base_order # user colors correspond to base order
    color.set <- color.set[stack_order] # reorder colors to match stacking order

    # Explicitly align colours with Type levels
    names(color.set) <- types

    # 6. Plot
    p <- ggplot(rda.ggplot, aes(x = Methods, y = Prop, fill = Type)) +
        geom_bar(stat = "identity") +
        ylab("Explained variance (proportion)") +
        scale_fill_manual(name = "Variation sources", values = color.set) +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = x.angle, hjust = x.hjust),
            panel.grid.minor = element_blank()
        ) +
        geom_text(
            aes(y = ypos, label = round(Prop, digits = 3)),
            vjust = 1.6,
            color = "black",
            size = text.cex
        ) +
        labs(title = title)

    p
}
