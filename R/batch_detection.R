#' Scatter Plot of Components with Marginal Density Plots
#'
#' This function draws a sample scatter plot for two selected components,
#' together with marginal density plots along each axis.
#' It is generic in the sense that it only requires a matrix or dataframe
#' of component scores, and can therefore be used with PCA, PLS or any
#' other multivariate method that returns component scores.
#'
#' @import ggplot2
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggpubr get_legend
#' @importFrom grid grid.rect gpar
#'
#' @param components A numeric matrix or data frame containing component
#' scores. Samples are in rows and components in columns.
#' @param comp Integer vector of length two indicating which components
#' to plot on the x and y axes respectively, for example \code{c(1, 2)}
#' or \code{c(1, 3)}.
#' @param expl.var Optional numeric vector giving the proportion of variance
#' explained by each component. If provided, it should have length at least
#' \code{max(comp)} and is used to annotate the axis labels.
#' @param batch Optional factor or vector giving batch grouping information
#' (categorical outcome). If provided, its length must match the number of
#' samples in \code{components}. When present, points and density plots are
#' coloured by batch.
#' @param trt Optional factor or vector giving treatment grouping
#' information (categorical outcome). If provided, its length must match
#' the number of samples in \code{components}. When present, points and
#' density plots are further distinguished by treatment-specific shapes
#' or line types.
#' @param xlim Optional numeric vector of length two giving the x-axis
#' limits for the scatter plot. If \code{NULL}, limits are chosen
#' automatically from the data.
#' @param ylim Optional numeric vector of length two giving the y-axis
#' limits for the scatter plot. If \code{NULL}, limits are chosen
#' automatically from the data.
#' @param color.set Optional character vector of colours (hexadecimal codes)
#' used to represent batch levels. If \code{NULL} and \code{batch} is
#' provided, an internal palette is used and extended when the number of
#' batches exceeds the base palette size. If \code{batch} is \code{NULL},
#' this argument is ignored.
#' @param shape.set Optional numeric vector of plotting characters
#' used to represent treatment levels. If \code{NULL} and \code{trt} is
#' provided, an internal sequence of hollow and solid shapes is used.
#' If \code{trt} is \code{NULL}, this argument is ignored and a fixed
#' shape is used.
#' @param batch.legend.title Character string giving the legend title
#' for batch.
#' @param trt.legend.title Character string giving the legend title
#' for treatment.
#' @param density.lwd Numeric value giving the line width for the density
#' curves in the marginal plots.
#' @param title Character string giving the main title.
#' @param title.cex Numeric value controlling the relative size of the
#' main title.
#' @param legend.cex Numeric value controlling the relative size of legend
#' text.
#' @param legend.title.cex Numeric value controlling the relative size of
#' legend titles.
#'
#' @return A grob object containing the combined scatter
#' and density plots.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{box_plot}}, \code{\link{density_plot}},
#' \code{\link{alignment_score}} and \code{\link{partVar_plot}} as other
#' methods for batch effect detection and batch effect removal assessment.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("mixOmics", quietly = TRUE) &&
#'     requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     ## Example using a PCA object from mixOmics
#'
#'     data("AD_data")
#'
#'     ## centered log-ratio transformed data
#'     ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
#'     ad.pca <- mixOmics::pca(ad.clr, ncomp = 3, scale = TRUE)
#'
#'     ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat # batch information
#'     ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt # treatment information
#'     names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
#'
#'     ## components and explained variance extracted from the PCA object
#'     comp.mat <- ad.pca$variates$X
#'     expl.var <- ad.pca$prop_expl_var$X
#'
#'     ## Scatter plot of the first two components
#'     Scatter_Density(
#'         components = comp.mat,
#'         comp = c(1, 2),
#'         expl.var = expl.var,
#'         batch = ad.batch,
#'         trt = ad.trt
#'     )
#'
#'     ## Scatter plot of components 1 and 3, with a user-defined colour set
#'     cols <- rainbow(10)
#'     Scatter_Density(
#'         components = comp.mat,
#'         comp = c(1, 3),
#'         expl.var = expl.var,
#'         batch = ad.batch,
#'         trt = ad.trt,
#'         color.set = cols
#'     )
#' }
#'
Scatter_Density <- function(components,
                            comp = c(1, 2),
                            expl.var = NULL,
                            batch = NULL,
                            trt = NULL,
                            xlim = NULL,
                            ylim = NULL,
                            color.set = NULL,
                            shape.set = NULL,
                            batch.legend.title = "Batch",
                            trt.legend.title = "Treatment",
                            density.lwd = 0.2,
                            title = NULL,
                            title.cex = 1.5,
                            legend.cex = 0.7,
                            legend.title.cex = 0.75) {
    ## 1. Basic checks
    components <- as.data.frame(components)

    if (length(comp) != 2L) {
        stop("'comp' must be an integer vector of length 2, e.g. c(1, 2).")
    }
    if (any(comp < 1L) || any(comp > ncol(components))) {
        stop("'comp' indices must be between 1 and ncol(components).")
    }

    n <- nrow(components)

    has_batch <- !is.null(batch)
    has_trt <- !is.null(trt)

    if (has_batch && length(batch) != n) {
        stop("'batch' length must match the number of samples.")
    }
    if (has_trt && length(trt) != n) {
        stop("'trt' length must match the number of samples.")
    }

    if (has_batch) {
        batch <- as.factor(batch)
    }
    if (has_trt) {
        trt <- as.factor(trt)
    }

    ## 2. Build internal data frame
    df <- data.frame(CompX = components[, comp[1]], CompY = components[, comp[2]])
    if (has_batch) {
        df$batch <- batch
    }
    if (has_trt) {
        df$trt <- trt
    }

    # 3. Colour handling (only if batch is present)
    if (has_batch) {
        K <- nlevels(batch)

        if (is.null(color.set)) {
            base.cols <- pb_color(seq_len(25))

            if (K <= 25) {
                color.set <- base.cols[seq_len(K)]
            } else {
                cycles <- ceiling(K / 25)
                col.list <- vector("list", cycles)
                col.list[[1]] <- base.cols

                if (cycles >= 2L) {
                    for (i in seq(2L, cycles)) {
                        if (i %% 2 == 0) {
                            col.list[[i]] <- lighten(base.cols, amount = 0.15)
                        } else {
                            col.list[[i]] <- darken(base.cols, amount = 0.15)
                        }
                    }
                }

                color.set <- unlist(col.list, use.names = FALSE)[seq_len(K)]
            }
        } else {
            if (length(color.set) < K) {
                warning("'color.set' shorter than number of batch levels. Recycling.")
                color.set <- rep(color.set, length.out = K)
            } else {
                color.set <- color.set[seq_len(K)]
            }
        }
    }

    # 4. Shape handling (only if trt is present)
    if (has_trt) {
        L <- nlevels(trt)

        if (is.null(shape.set)) {
            base.shapes <- c(1, 16, 0, 15, 2, 17, 5, 18, 3, 9, 4, 7, 8, 6)

            if (L <= length(base.shapes)) {
                shape.set <- base.shapes[seq_len(L)]
            } else {
                warning("Not enough distinct shapes. Recycling.")
                shape.set <- rep(base.shapes, length.out = L)
            }
        } else {
            if (length(shape.set) < L) {
                warning("'shape.set' shorter than number of trt levels. Recycling.")
                shape.set <- rep(shape.set, length.out = L)
            } else {
                shape.set <- shape.set[seq_len(L)]
            }
        }
    }

    ## 5. Axis labels (if expl.var is provided)
    if (!is.null(expl.var)) {
        if (length(expl.var) < max(comp)) {
            stop("'expl.var' must have length >= max(comp).")
        }
        xl <- paste0("Comp ", comp[1], ": ", round(as.numeric(expl.var[comp[1]]) * 100), "% expl.var")
        yl <- paste0("Comp ", comp[2], ": ", round(as.numeric(expl.var[comp[2]]) * 100), "% expl.var")
    } else {
        xl <- paste0("Comp ", comp[1])
        yl <- paste0("Comp ", comp[2])
    }


    ## 6. Main scatter plot
    if (has_batch && has_trt) {
        pMain <- ggplot(df, aes(CompX, CompY, colour = batch, shape = trt)) +
            geom_point() +
            scale_color_manual(values = color.set) +
            scale_shape_manual(values = shape.set) +
            labs(colour = batch.legend.title, shape = trt.legend.title)
    } else if (has_batch && !has_trt) {
        pMain <- ggplot(df, aes(CompX, CompY, colour = batch)) +
            geom_point(shape = 16) +
            scale_color_manual(values = color.set) +
            labs(colour = batch.legend.title)
    } else if (!has_batch && has_trt) {
        pMain <- ggplot(df, aes(CompX, CompY, shape = trt)) +
            geom_point(colour = "grey50") +
            scale_shape_manual(values = shape.set) +
            labs(shape = trt.legend.title)
    } else {
        ## no batch, no trt

        pMain <- ggplot(df, aes(CompX, CompY)) +
            geom_point(colour = "grey50", shape = 16)
    }

    pMain <- pMain +
        xlab(xl) + ylab(yl) +
        scale_x_continuous(limits = xlim) +
        scale_y_continuous(limits = ylim) +
        theme_bw() +
        theme(
            legend.position   = "right",
            legend.box        = "horizontal",
            legend.direction  = "vertical",
            legend.key.height = unit(0.2, "cm"),
            legend.key.width  = unit(0.1, "cm"),
            legend.title      = element_text(size = rel(legend.title.cex)),
            legend.spacing.x  = unit(0.1, "cm"),
            legend.spacing.y  = unit(0.1, "cm"),
            legend.text       = element_text(size = rel(legend.cex))
        )

    ## Update axis limits
    xlim.update <- layer_scales(pMain)$x$get_limits()
    ylim.update <- layer_scales(pMain)$y$get_limits()

    ## 7. Density plots
    ## Rules:
    ##   no batch → grey fill
    ##   no trt   → no linetype mapping

    ## Top density
    if (has_batch && has_trt) {
        pTop <- ggplot(df, aes(CompX, fill = batch, linetype = trt)) +
            geom_density(linewidth = density.lwd, alpha = 0.5) +
            scale_fill_manual(values = color.set)
    } else if (has_batch && !has_trt) {
        pTop <- ggplot(df, aes(CompX, fill = batch)) +
            geom_density(linewidth = density.lwd, alpha = 0.5) +
            scale_fill_manual(values = color.set)
    } else if (!has_batch && has_trt) {
        pTop <- ggplot(df, aes(CompX, linetype = trt)) +
            geom_density(
                linewidth = density.lwd,
                alpha = 0.5,
                fill = "grey50",
                colour = "black"
            )
    } else {
        pTop <- ggplot(df, aes(CompX)) +
            geom_density(
                linewidth = density.lwd,
                alpha = 0.5,
                fill = "grey50",
                colour = "black"
            )
    }


    pTop <- pTop +
        scale_x_continuous(limits = xlim.update) +
        ylab("Density") +
        labs(title = title) +
        theme(
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = rel(0.8)),
            plot.title = element_text(hjust = 0.5, size = rel(title.cex)),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            legend.position = "none"
        )


    ## Right density
    if (has_batch && has_trt) {
        pRight <- ggplot(df, aes(CompY, fill = batch, linetype = trt)) +
            geom_density(linewidth = density.lwd, alpha = 0.5) +
            scale_fill_manual(values = color.set)
    } else if (has_batch && !has_trt) {
        pRight <- ggplot(df, aes(CompY, fill = batch)) +
            geom_density(linewidth = density.lwd, alpha = 0.5) +
            scale_fill_manual(values = color.set)
    } else if (!has_batch && has_trt) {
        pRight <- ggplot(df, aes(CompY, linetype = trt)) +
            geom_density(
                linewidth = density.lwd,
                alpha = 0.5,
                fill = "grey50",
                colour = "black"
            )
    } else {
        pRight <- ggplot(df, aes(CompY)) +
            geom_density(
                linewidth = density.lwd,
                alpha = 0.5,
                fill = "grey50",
                colour = "black"
            )
    }

    pRight <- pRight +
        coord_flip() +
        scale_x_continuous(limits = ylim.update) +
        ylab("Density") +
        theme(
            axis.title.x = element_text(size = rel(0.8)),
            axis.title.y = element_blank(),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            legend.position = "none"
        )


    ## 8. Legend
    if (!has_batch && !has_trt) {
        legend <- grid::grid.rect(gp = grid::gpar(col = "white"))
    } else {
        legend <- ggpubr::get_legend(pMain)
    }

    ## 9. Arrange grid
    combined_grob <- gridExtra::arrangeGrob(
        pTop,
        legend,
        pMain + theme(legend.position = "none"),
        pRight,
        ncol = 2,
        nrow = 2,
        widths = c(3, 1),
        heights = c(1, 3)
    )
    grid::grid.newpage()
    grid::grid.draw(combined_grob)
    return(invisible(combined_grob))
}


#' Box Plot by Batch
#'
#' This function draws side-by-side box plots for each batch.
#'
#' @import ggplot2
#'
#' @param df A data frame with at least two columns.
#' The first column contains the numeric values to be plotted on the y-axis,
#' and the second column contains the batch information (categorical).
#' Additional columns, if present, are ignored.
#' @param title Character, the plot title.
#' @param batch.legend.title Character, the legend title for the batch groups.
#' @param ylab Character, the y-axis title.
#' @param color.set A character vector specifying the colours to use for the
#' batch groups. Colours can be given as hexadecimal codes or any values
#' understood by \code{ggplot2}. If \code{NULL}, a default palette based on
#' \code{pb_color()} is used. If the length is shorter than the number of
#' batch levels, it will be recycled with a warning.
#' @param x.angle Numeric, angle of the x-axis labels in degrees,
#' in the range from \eqn{0} to \eqn{360}.
#' @param x.hjust Numeric, horizontal justification of the x-axis labels,
#' in the range from \eqn{0} to \eqn{1}.
#' @param x.vjust Numeric, vertical justification of the x-axis labels,
#' in the range from \eqn{0} to \eqn{1}.
#'
#' @return
#' A \code{ggplot} object representing the box plots.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{Scatter_Density}}, \code{\link{density_plot}},
#' \code{\link{alignment_score}} and \code{\link{partVar_plot}} as other
#' methods for batch effect detection and batch effect removal assessment.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data("AD_data")
#'
#'     # centered log-ratio transformed data
#'     ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
#'     ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
#'     names(ad.batch) <- rownames(AD_data$EgData)
#'
#'     ad.df <- data.frame(
#'         value = ad.clr[, 1],
#'         batch = ad.batch
#'     )
#'
#'     box_plot(df = ad.df, title = "OTU 12", x.angle = 30)
#'
#'     # using a custom colour set
#'     colorlist <- rainbow(10)
#'     box_plot(
#'         df = ad.df, title = "OTU 12",
#'         color.set = colorlist, x.angle = 30
#'     )
#' }
#'
box_plot <- function(df,
                     title = NULL,
                     batch.legend.title = "Batch",
                     ylab = "Value",
                     color.set = NULL,
                     x.angle = 0,
                     x.hjust = 0.5,
                     x.vjust = 0.5) {
    # Basic checks
    if (!is.data.frame(df) || ncol(df) < 2L) {
        stop("'df' must be a data frame with at least two columns: value and batch.")
    }

    value <- df[[1L]]
    batch <- df[[2L]]

    # Ensure batch is factor
    if (!is.factor(batch)) {
        batch <- factor(batch)
    }

    K <- nlevels(batch)

    # Colour handling
    if (is.null(color.set)) {
        # base palette: 25 colours from pb_color()
        base.cols <- pb_color(seq_len(25))

        if (K <= 25L) {
            color.set <- base.cols[seq_len(K)]
        } else {
            cycles <- ceiling(K / 25L)
            col.list <- vector("list", cycles)
            col.list[[1L]] <- base.cols

            if (cycles >= 2L) {
                for (i in seq(2L, cycles)) {
                    if (i %% 2L == 0L) {
                        col.list[[i]] <- lighten(base.cols, amount = 0.15)
                    } else {
                        col.list[[i]] <- darken(base.cols, amount = 0.15)
                    }
                }
            }

            color.set <- unlist(col.list, use.names = FALSE)[seq_len(K)]
        }
    } else {
        if (!is.character(color.set)) {
            stop("'color.set' must be a character vector of colour values.")
        }

        if (length(color.set) < K) {
            warning("'color.set' is shorter than the number of batch levels. Recycling.")
            color.set <- rep(color.set, length.out = K)
        } else {
            color.set <- color.set[seq_len(K)]
        }
    }

    # Data frame used for plotting
    df.plot <- data.frame(
        value = value,
        batch = batch,
        stringsAsFactors = FALSE
    )

    ggplot(data = df.plot, aes(x = batch, y = value, fill = batch)) +
        geom_boxplot() +
        stat_boxplot(geom = "errorbar", width = 0.4) +
        scale_fill_manual(values = color.set, drop = FALSE) +
        theme_bw() +
        theme(
            axis.text.x = element_text(
                angle = x.angle,
                hjust = x.hjust,
                vjust = x.vjust
            ),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = rel(1.2))
        ) +
        labs(fill = batch.legend.title, y = ylab, title = title)
}


#' Density Plot by Batch
#'
#' This function draws overlapping density plots for each batch.
#'
#' @import ggplot2
#'
#' @param df A data frame with at least two columns.
#' The first column contains the numeric values to be plotted on the x-axis,
#' and the second column contains the batch information (categorical).
#' Additional columns, if present, are ignored.
#' @param title Character, the plot title.
#' @param batch.legend.title Character, the legend title for the batch groups.
#' @param xlab Character, the x-axis title.
#' @param color.set A character vector specifying the colours to use for the
#' batch groups. Colours can be given as hexadecimal codes or any values
#' understood by \code{ggplot2}. If \code{NULL}, a default palette based on
#' \code{pb_color()} is used. If the length is shorter than the number of
#' batch levels, it will be recycled with a warning.
#' @param title.hjust Numeric, horizontal justification of the plot title,
#' in the range from \eqn{0} to \eqn{1}.
#'
#' @return
#' A \code{ggplot} object representing the density plots.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{Scatter_Density}}, \code{\link{box_plot}},
#' \code{\link{alignment_score}} and \code{\link{partVar_plot}} as other
#' methods for batch effect detection and batch effect removal assessment.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data("AD_data")
#'
#'     # centered log-ratio transformed data
#'     ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
#'     ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
#'     names(ad.batch) <- rownames(AD_data$EgData)
#'
#'     ad.df <- data.frame(
#'         value = ad.clr[, 1],
#'         batch = ad.batch
#'     )
#'
#'     density_plot(df = ad.df, title = "OTU 12")
#'
#'     # using a custom colour set
#'     colorlist <- rainbow(10)
#'     density_plot(
#'         df = ad.df, title = "OTU 12",
#'         color.set = colorlist
#'     )
#' }
#'
density_plot <- function(df,
                         title = NULL,
                         batch.legend.title = "Batch",
                         xlab = "Value",
                         color.set = NULL,
                         title.hjust = 0.5) {
    # Basic checks
    if (!is.data.frame(df) || ncol(df) < 2L) {
        stop("'df' must be a data frame with at least two columns: value and batch.")
    }

    value <- df[[1L]]
    batch <- df[[2L]]

    # Ensure batch is factor
    if (!is.factor(batch)) {
        batch <- factor(batch)
    }

    K <- nlevels(batch)

    # Colour handling
    if (is.null(color.set)) {
        # base palette: 25 colours from pb_color()
        base.cols <- pb_color(seq_len(25))

        if (K <= 25L) {
            color.set <- base.cols[seq_len(K)]
        } else {
            cycles <- ceiling(K / 25L)
            col.list <- vector("list", cycles)
            col.list[[1L]] <- base.cols

            if (cycles >= 2L) {
                for (i in seq(2L, cycles)) {
                    if (i %% 2L == 0L) {
                        col.list[[i]] <- lighten(base.cols, amount = 0.15)
                    } else {
                        col.list[[i]] <- darken(base.cols, amount = 0.15)
                    }
                }
            }

            color.set <- unlist(col.list, use.names = FALSE)[seq_len(K)]
        }
    } else {
        if (!is.character(color.set)) {
            stop("'color.set' must be a character vector of colour values.")
        }

        if (length(color.set) < K) {
            warning("'color.set' is shorter than the number of batch levels. Recycling.")
            color.set <- rep(color.set, length.out = K)
        } else {
            color.set <- color.set[seq_len(K)]
        }
    }

    # Data frame used for plotting
    df.plot <- data.frame(
        value = value,
        batch = batch,
        stringsAsFactors = FALSE
    )

    ggplot(data = df.plot, aes(x = value, fill = batch)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = color.set, drop = FALSE) +
        labs(
            title = title,
            x     = xlab,
            y     = "Density",
            fill  = batch.legend.title
        ) +
        theme_bw() +
        theme(
            plot.title = element_text(hjust = title.hjust),
            panel.grid = element_blank()
        )
}
