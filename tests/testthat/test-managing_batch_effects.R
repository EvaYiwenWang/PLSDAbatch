test_that("checking linear_regres", {
    data("AD_data")

    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
    ad.lm <- linear_regres(
        data = ad.clr, trt = ad.trt,
        batch.fix = ad.batch,
        type = "linear model",
        criterion = "AIC"
    )
    ad.p.adj <- ad.lm$adj.p
    ad.aic <- ad.lm$AIC

    expect_is(ad.lm, "list")
    expect_is(ad.p.adj, "numeric")
    expect_is(ad.aic, "data.frame")
})

test_that("checking percentileofscore", {
    data("AD_data")

    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
    trt.first.b <- ad.trt[ad.batch == "09/04/2015"]
    ad.first.b.pn <- PLSDAbatch:::percentileofscore(
        ad.clr[ad.batch == "09/04/2015", ],
        which(trt.first.b == "0-0.5")
    )

    expect_is(ad.first.b.pn, "data.frame")
    expect_equal(dim(ad.first.b.pn), c(9, 231))
})


test_that("checking percentile_norm", {
    data("AD_data")

    ad.clr <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad.batch <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    ad.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
    ad.PN <- percentile_norm(
        data = ad.clr, batch = ad.batch,
        trt = ad.trt, ctrl.grp = "0-0.5"
    )

    expect_is(ad.PN, "data.frame")
    expect_equal(dim(ad.PN), c(75, 231))
})
