#' Linear Regression
#'
#' This function fits linear regression models (either a linear model or a
#' linear mixed model) to each microbial variable, including treatment and batch
#' covariates as specified. For each variable, two models are fitted:
#' (i) a model including only the treatment effect (\code{trt.only});
#' (ii) a model including both treatment and batch effects (\code{trt.batch}).
#' A selected criterion (e.g., AIC, BIC, RMSE, R2) is used to choose the
#' better model for each variable, and only the p-value of the selected model
#' is returned.
#'
#' @importFrom lmerTest lmer
#' @importFrom performance rmse r2
#' @importFrom stats lm p.adjust anova
#'
#' @param data A data frame containing microbial variables. Rows correspond
#' to samples and columns to features.
#' @param trt A factor or class vector representing the treatment groups.
#' This argument is mandatory and is coerced to a factor internally. The
#' p-values correspond to the global treatment effect extracted from
#' \code{anova()}.
#' @param batch.fix A factor or vector representing a batch effect treated
#' as a fixed effect. Required when \code{type = "linear model"}.
#' @param batch.fix2 A second fixed batch effect. Can only be used when
#' \code{batch.fix} is provided.
#' @param batch.random A factor or vector representing a batch effect treated
#' as a random effect. Required when \code{type = "linear mixed model"}.
#' @param type Either \code{"linear model"} or \code{"linear mixed model"}.
#' @param p.adjust.method Method for p-value adjustment. One of:
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"},
#' \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"},
#' or \code{"none"}.
#' @param criterion A character string indicating the model selection
#' criterion used to choose between the treatment-only model and the
#' treatment+batch model for each microbial variable. One of
#' \code{"R2"}, \code{"RMSE"}, \code{"RSE"}, \code{"AIC"} or \code{"BIC"}.
#' When \code{criterion = "R2"}:
#' \itemize{
#'   \item For \code{type = "linear model"}, the comparison is based on
#'   the \strong{adjusted R2} of the two models (treatment-only versus
#'   treatment+batch).
#'   \item For \code{type = "linear mixed model"}, the comparison is based on
#'   the \strong{conditional R2} of the mixed model versus the R2 of the
#'   corresponding treatment-only linear model.
#' }
#' A larger R2 indicates a better model. For all other criteria
#' (\code{"RMSE"}, \code{"RSE"}, \code{"AIC"}, \code{"BIC"}), smaller
#' values indicate a better model.
#' @param return.model Logical. If \code{TRUE}, fitted model objects
#' (\code{lm} or \code{lmerMod}) are returned. If \code{FALSE}, model
#' objects are replaced with \code{NULL} to save memory.
#'
#' @return A list containing:
#' \item{type}{The type of model used ("linear model" or "linear mixed model").}
#' \item{model}{A list of fitted \code{lm} or \code{lmerMod} objects, or
#' \code{NULL} if \code{return.model = FALSE}.}
#' \item{raw.p}{A vector of p-values corresponding to the selected model
#' (based on \code{criterion}) for each microbial variable.}
#' \item{adj.p}{Adjusted p-values.}
#' \item{p.adjust.method}{The p-value adjustment method used.}
#' \item{criterion}{The criterion used to select between the two models.}
#' \item{best.model}{For each feature, either \code{"trt.only"} or
#' \code{"trt.batch"}, indicating which model was selected.}
#'
#' \item{raw.R2}{For \code{type = "linear model"}, the R2 values for the
#' treatment-only and treatment+batch models. For \code{type = "linear mixed model"},
#' this field contains \code{NA}.}
#'
#' \item{adj.R2}{Adjusted R2 values (linear model only). For mixed models,
#' this field contains \code{NA}.}
#'
#' \item{cond.R2}{Conditional R2 for mixed models: first column corresponds
#' to the treatment-only linear model, second column to the mixed model
#' including batch.random. For linear models, this field is \code{NA}.}
#'
#' \item{RMSE}{The root mean squared error for both models (two columns:
#' \code{trt.only} and \code{trt.batch}).}
#' \item{RSE}{Residual standard error for both models.}
#' \item{AIC}{AIC values for both models.}
#' \item{BIC}{BIC values for both models.}
#'
#' @note For each microbial variable, two models are always fitted:
#' \enumerate{
#'   \item{\code{trt.only}: \code{y ~ trt}}
#'   \item{\code{trt.batch}: \code{y ~ trt + batch} (or with random effects)}
#' }
#' The selected model is determined solely by \code{criterion}. Only the
#' p-value corresponding to the selected model is returned in \code{raw.p}.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{percentile_norm}}, \code{\link{PLSDA_batch}}
#'
#' @export
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data("AD_data")
#'
#'     # centered log ratio transformed data
#'     ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
#'     ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat # batch information
#'     ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt # treatment information
#'     names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
#'     ad.lm <- linear_regres(
#'         data = ad.clr, trt = ad.trt,
#'         batch.fix = ad.batch,
#'         type = "linear model",
#'         criterion = "AIC"
#'     )
#'     ad.p.adj <- data.frame(best_model = ad.lm$best.model, adjust_p = ad.lm$adj.p)
#'     head(ad.p.adj)
#'     head(ad.lm$AIC)
#'     table(ad.lm$best.model)
#' }
#'
linear_regres <- function(data,
                          trt,
                          batch.fix = NULL,
                          batch.fix2 = NULL,
                          batch.random = NULL,
                          type = "linear model",
                          p.adjust.method = "fdr",
                          criterion = "AIC",
                          return.model = TRUE) {
    type <- match.arg(type, c("linear model", "linear mixed model"))
    p.adjust.method <- match.arg(
        p.adjust.method,
        c(
            "holm",
            "hochberg",
            "hommel",
            "bonferroni",
            "BH",
            "BY",
            "fdr",
            "none"
        )
    )
    criterion <- match.arg(toupper(criterion), c("R2", "RMSE", "RSE", "AIC", "BIC"))

    data <- as.data.frame(data)

    # Input checks

    # 1. trt must be provided
    if (missing(trt) || is.null(trt)) {
        stop("'trt' must be provided and cannot be NULL.")
    }

    # 2. At least one batch effect must be provided (fixed or random)
    if (is.null(batch.fix) && is.null(batch.random)) {
        stop("At least one batch effect must be provided: either 'batch.fix' or 'batch.random'.")
    }

    # 3. batch.fix2 can only be used when batch.fix exists
    if (!is.null(batch.fix2) && is.null(batch.fix)) {
        stop(
            "'batch.fix2' was provided but 'batch.fix' is NULL.\n",
            "To include a second fixed batch effect, 'batch.fix' must also be provided."
        )
    }

    # 4. Length consistency check
    n <- nrow(data)

    if (length(trt) != n) {
        stop("'trt' must have the same length as the number of rows in 'data'.")
    }
    if (!is.null(batch.fix) && length(batch.fix) != n) {
        stop("'batch.fix' must have the same length as the number of rows in 'data'.")
    }
    if (!is.null(batch.fix2) && length(batch.fix2) != n) {
        stop("'batch.fix2' must have the same length as the number of rows in 'data'.")
    }
    if (!is.null(batch.random) && length(batch.random) != n) {
        stop("'batch.random' must have the same length as the number of rows in 'data'.")
    }

    # 5. Coerce to factor to match documentation
    trt <- as.factor(trt)
    if (!is.null(batch.fix)) {
        batch.fix <- as.factor(batch.fix)
    }
    if (!is.null(batch.fix2)) {
        batch.fix2 <- as.factor(batch.fix2)
    }
    if (!is.null(batch.random)) {
        batch.random <- as.factor(batch.random)
    }

    nvar <- ncol(data)
    empty_df <- function() {
        data.frame(
            trt.only  = rep(NA_real_, nvar),
            trt.batch = rep(NA_real_, nvar),
            row.names = colnames(data)
        )
    }

    ## Objects common to both types

    # will fill later depending on branch
    best.model <- factor(rep(NA_character_, nvar), levels = c("trt.only", "trt.batch"))
    p <- rep(NA_real_, nvar)
    p.trt.only <- rep(NA_real_, nvar)
    p.trt.batch <- rep(NA_real_, nvar)
    model <- vector("list", length = nvar)

    ## Linear model

    if (type == "linear model") {
        if (is.null(batch.fix)) {
            stop("'batch.fix' should be provided.")
        }

        raw.R2 <- empty_df()
        adj.R2 <- empty_df()
        RMSE <- empty_df()
        RSE <- empty_df()
        AIC <- empty_df()
        BIC <- empty_df()

        cond.R2 <- NA

        for (i in seq_len(nvar)) {
            y <- data[, i]

            res.lm0 <- stats::lm(y ~ trt)

            if (!is.null(batch.fix2)) {
                res.lm <- stats::lm(y ~ trt + batch.fix + batch.fix2)
            } else {
                res.lm <- stats::lm(y ~ trt + batch.fix)
            }

            summary.res0 <- summary(res.lm0)
            summary.res <- summary(res.lm)

            if (return.model) {
                model[[i]] <- res.lm
            }

            anova0 <- stats::anova(res.lm0)
            if ("trt" %in% rownames(anova0)) {
                p.trt.only[i] <- anova0["trt", "Pr(>F)"]
            }

            anova1 <- stats::anova(res.lm)
            if ("trt" %in% rownames(anova1)) {
                p.trt.batch[i] <- anova1["trt", "Pr(>F)"]
            }

            raw.R2[i, ] <- c(summary.res0$r.squared, summary.res$r.squared)
            adj.R2[i, ] <- c(summary.res0$adj.r.squared, summary.res$adj.r.squared)
            RMSE[i, ] <- c(performance::rmse(res.lm0), performance::rmse(res.lm))
            RSE[i, ] <- c(summary.res0$sigma, summary.res$sigma)
            AIC[i, ] <- c(AIC(res.lm0), AIC(res.lm))
            BIC[i, ] <- c(BIC(res.lm0), BIC(res.lm))

            vals <- switch(criterion,
                "R2" = as.numeric(adj.R2[i, ]),
                "RMSE" = as.numeric(RMSE[i, ]),
                "RSE" = as.numeric(RSE[i, ]),
                "AIC" = as.numeric(AIC[i, ]),
                "BIC" = as.numeric(BIC[i, ])
            )

            if (criterion == "R2") {
                best.idx <- which.max(vals)
            } else {
                best.idx <- which.min(vals)
            }
            best.model[i] <- if (best.idx == 1) {
                "trt.only"
            } else {
                "trt.batch"
            }
            p[i] <- if (best.idx == 1) {
                p.trt.only[i]
            } else {
                p.trt.batch[i]
            }
        }
        p.adj <- stats::p.adjust(p, method = p.adjust.method)
    }

    ## Linear mixed model
    if (type == "linear mixed model") {
        if (is.null(batch.random)) {
            stop("'batch.random' should be provided.")
        }


        cond.R2 <- empty_df()
        RMSE <- empty_df()
        RSE <- empty_df()
        AIC <- empty_df()
        BIC <- empty_df()

        raw.R2 <- adj.R2 <- NA

        for (i in seq_len(nvar)) {
            y <- data[, i]

            res.lm0 <- stats::lm(y ~ trt)

            if (!is.null(batch.fix)) {
                if (!is.null(batch.fix2)) {
                    res.lmm <- lmerTest::lmer(y ~ trt + batch.fix + batch.fix2 +
                        (1 | batch.random))
                } else {
                    res.lmm <- lmerTest::lmer(y ~ trt + batch.fix +
                        (1 | batch.random))
                }
            } else {
                res.lmm <- lmerTest::lmer(y ~ trt + (1 | batch.random))
            }

            summary.res0 <- summary(res.lm0)
            summary.res <- summary(res.lmm)

            if (return.model) {
                model[[i]] <- res.lmm
            }

            anova0 <- stats::anova(res.lm0)
            if ("trt" %in% rownames(anova0)) {
                p.trt.only[i] <- anova0["trt", "Pr(>F)"]
            }

            anova1 <- stats::anova(res.lmm)
            if ("trt" %in% rownames(anova1)) {
                p.trt.batch[i] <- anova1["trt", "Pr(>F)"]
            }

            r2_vals <- performance::r2(res.lmm)
            cond_val <- r2_vals[["R2_conditional"]]
            cond.R2[i, ] <- c(summary.res0$r.squared, cond_val)

            RMSE[i, ] <- c(
                performance::rmse(res.lm0),
                performance::rmse(res.lmm)
            )
            RSE[i, ] <- c(summary.res0$sigma, summary.res$sigma)
            AIC[i, ] <- c(AIC(res.lm0), AIC(res.lmm))
            BIC[i, ] <- c(BIC(res.lm0), BIC(res.lmm))

            vals <- switch(criterion,
                "R2" = as.numeric(cond.R2[i, ]),
                "RMSE" = as.numeric(RMSE[i, ]),
                "RSE" = as.numeric(RSE[i, ]),
                "AIC" = as.numeric(AIC[i, ]),
                "BIC" = as.numeric(BIC[i, ])
            )

            if (criterion == "R2") {
                best.idx <- which.max(vals)
            } else {
                best.idx <- which.min(vals)
            }

            best.model[i] <- if (best.idx == 1) {
                "trt.only"
            } else {
                "trt.batch"
            }
            p[i] <- if (best.idx == 1) {
                p.trt.only[i]
            } else {
                p.trt.batch[i]
            }
        }
        p.adj <- stats::p.adjust(p, method = p.adjust.method)
    }
    names(model) <- names(p) <- names(p.adj) <- rownames(RMSE) <-
        rownames(RSE) <- rownames(AIC) <- rownames(BIC) <- colnames(data)

    result <- list(
        type = type,
        model = model,
        raw.p = p,
        adj.p = p.adj,
        p.adjust.method = p.adjust.method,
        criterion = criterion,
        best.model = best.model,
        raw.R2 = raw.R2,
        adj.R2 = adj.R2,
        cond.R2 = cond.R2,
        RMSE = RMSE,
        RSE = RSE,
        AIC = AIC,
        BIC = BIC
    )

    return(invisible(result))
}


#' Percentile score
#'
#' This function converts the relative abundance of microbial variables
#' (i.e. bacterial taxa) in case (i.e. disease) samples to percentiles of
#' the corresponding variables in control (i.e. healthy) samples.
#' It is a built-in function of \code{percentile_norm()}.
#'
#'
#' @param df A data frame or matrix that contains the microbial variables
#' to be converted into percentile scores. Samples are in rows and variables
#' are in columns.
#' @param control.index A numeric vector that contains the indices
#' of control samples (row indices of \code{df}).
#'
#' @return A data frame of percentile scores (between 0 and 1) for each
#' microbial variable and each sample.
#'
#' @keywords internal
#'
#' @references
#' \insertRef{gibbons2018correcting}{PLSDAbatch}
#'
#'
#' @examples
#' NULL
#'
percentileofscore <- function(df, control.index) {
    # Basic checks
    if (!is.matrix(df) && !is.data.frame(df)) {
        stop("'df' must be a matrix or data.frame.")
    }
    if (is.data.frame(df)) {
        df <- as.matrix(df)
    }
    if (!is.numeric(control.index)) {
        stop("'control.index' must be a numeric vector of row indices.")
    }

    n <- nrow(df)
    p <- ncol(df)

    # Preallocate
    out <- matrix(NA_real_,
        nrow = n,
        ncol = p,
        dimnames = dimnames(df)
    )

    for (i in seq_len(p)) {
        control <- sort(df[control.index, i])
        m <- length(control)
        if (m == 0L) {
            stop("No control samples found for column ", i, ".")
        }

        x <- df[, i]

        # strict: number of control < x
        strict <- findInterval(x, control, left.open = TRUE)
        # weak: number of control <= x
        weak <- findInterval(x, control, rightmost.closed = TRUE)

        out[, i] <- (strict + weak) / (2 * m)
    }

    out <- as.data.frame(out, stringsAsFactors = FALSE)
    rownames(out) <- rownames(df)
    colnames(out) <- colnames(df)

    invisible(out)
}


#' Percentile normalisation
#'
#' This function corrects for batch effects in case-control microbiome studies.
#' Briefly, the relative abundance of microbial variables (i.e. bacterial taxa)
#' in case (i.e. disease) samples is converted to percentiles of the
#' corresponding variables in control (i.e. healthy) samples within each batch,
#' before pooling data across batches. Pooled batches must share comparable
#' case and control cohort definitions.
#'
#' @param data A data frame or matrix that contains the microbial variables
#' to be corrected for batch effects. Samples are in rows and variables
#' are in columns.
#' @param batch A factor or vector for the batch grouping information
#' (categorical outcome variable). Its length must match the number of rows
#' in \code{data}.
#' @param trt A factor or vector for the treatment grouping information
#' (categorical outcome variable). Its length must match the number of rows
#' in \code{data}.
#' @param ctrl.grp Character, the label of control samples (i.e. healthy) in
#' \code{trt}.
#'
#' @return A data frame with batch effects corrected by percentile
#' normalisation.
#'
#' @author Yiwen Wang, Kim-Anh Le Cao
#'
#' @seealso \code{\link{linear_regres}} and \code{\link{PLSDA_batch}} as
#' other methods for batch effect management.
#'
#' @references
#' \insertRef{gibbons2018correcting}{PLSDAbatch}
#'
#' @export
#'
#' @examples
#' if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
#'     data("AD_data")
#'
#'     # centered log ratio transformed data
#'     ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
#'     ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat # batch information
#'     ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt # treatment information
#'     names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
#'     ad.PN <- percentile_norm(
#'         data = ad.clr, batch = ad.batch,
#'         trt = ad.trt, ctrl.grp = "0-0.5"
#'     )
#' }
#'
percentile_norm <- function(data, batch, trt, ctrl.grp) {
    # Basic checks
    if (missing(data) ||
        missing(batch) || missing(trt) || missing(ctrl.grp)) {
        stop("Arguments 'data', 'batch', 'trt', and 'ctrl.grp' must all be provided.")
    }

    if (!is.matrix(data) && !is.data.frame(data)) {
        stop("'data' must be a matrix or data.frame.")
    }

    if (is.data.frame(data)) {
        data <- as.matrix(data)
    }

    if (nrow(data) != length(batch) || nrow(data) != length(trt)) {
        stop("Lengths of 'batch' and 'trt' must match the number of rows in 'data'.")
    }

    batch <- as.factor(batch)
    trt <- as.factor(trt)

    if (!ctrl.grp %in% levels(trt)) {
        stop("Control group '", ctrl.grp, "' is not found in 'trt'.")
    }

    # Keep original row order and names
    original_rownames <- rownames(data)

    # Split row indices by batch
    idx_by_batch <- split(seq_len(nrow(data)), batch)

    # Apply percentileofscore within each batch
    res_list <- lapply(seq_along(idx_by_batch), function(k) {
        idx <- idx_by_batch[[k]]

        trt_each_b <- trt[idx]
        ctrl_idx_local <- which(trt_each_b == ctrl.grp)

        if (length(ctrl_idx_local) == 0L) {
            stop(
                "No control samples with 'ctrl.grp = ",
                ctrl.grp,
                "' found in batch '",
                names(idx_by_batch)[k],
                "'."
            )
        }

        data_each_b <- data[idx, , drop = FALSE]

        percentileofscore(data_each_b, control.index = ctrl_idx_local)
    })

    # Combine batches in the order of factor levels
    names(res_list) <- names(idx_by_batch)
    data_pn <- do.call(rbind, res_list)

    # Reorder to match the original 'data' row order (if rownames are present)
    if (!is.null(original_rownames)) {
        data_pn <- data_pn[original_rownames, , drop = FALSE]
    }

    # Return as data.frame, invisibly, consistent with percentileofscore()
    data_pn <- as.data.frame(data_pn, stringsAsFactors = FALSE)
    invisible(data_pn)
}
