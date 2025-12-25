test_that("checking partVar_plot", {
    data("AD_data")
    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)

    ad.factors.df <- data.frame(trt = ad.trt, batch = ad.batch)
    rda.res <- vegan::varpart(ad.clr, ~trt, ~batch,
        data = ad.factors.df, scale = TRUE
    )

    ad.prop.df <- data.frame(
        Treatment = NA, Batch = NA,
        Intersection = NA,
        Residuals = NA
    )
    ad.prop.df[1, ] <- rda.res$part$indfract$Adj.R.squared

    ad.prop.df <- ad.prop.df[, c(1, 3, 2, 4)]

    res.pv <- partVar_plot(prop.df = ad.prop.df)
    expect_is(res.pv, "ggplot")
})
