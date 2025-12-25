test_that("checking Scatter_Density", {
    data("AD_data")
    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.pca <- mixOmics::pca(ad.clr, ncomp = 3, scale = TRUE)
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)

    comp.mat <- ad.pca$variates$X
    expl.var <- ad.pca$prop_expl_var$X
    res.sdplot <- Scatter_Density(
        components = comp.mat, comp = c(1, 2),
        expl.var = expl.var, batch = ad.batch,
        trt = ad.trt
    )
    expect_is(res.sdplot, "gtable")
    expect_is(res.sdplot, "grob")
})


test_that("checking box_plot", {
    data("AD_data")
    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    names(ad.batch) <- rownames(AD_data$EgData)
    ad.df <- data.frame(value = ad.clr[, 1], batch = ad.batch)
    res.bplot <- box_plot(df = ad.df, title = "OTU 12", x.angle = 30)
    expect_equal(colnames(ad.clr)[1], "OTU12")
    expect_is(res.bplot, "ggplot")
})


test_that("checking density_plot", {
    data("AD_data")
    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    names(ad.batch) <- rownames(AD_data$EgData)
    ad.df <- data.frame(value = ad.clr[, 1], batch = ad.batch)
    res.dplot <- density_plot(df = ad.df, title = "OTU 12")
    expect_equal(colnames(ad.clr)[1], "OTU12")
    expect_is(res.dplot, "ggplot")
})
