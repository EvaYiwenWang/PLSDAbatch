test_that("checking PLSDA_batch", {
    data("AD_data")

    X <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    Y.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    Y.bat <- SummarizedExperiment::rowData(AD_data$EgData)$Y.bat
    names(Y.bat) <- names(Y.trt) <- rownames(AD_data$EgData)
    ad_plsda_batch <- PLSDA_batch(X, Y.trt, Y.bat, ncomp.trt = 1, ncomp.bat = 5)
    ad_X.corrected <- ad_plsda_batch$X.nobatch

    expect_is(ad_plsda_batch, "list")
    expect_equal(length(names(ad_plsda_batch)), 17)

    expect_is(ad_X.corrected, "matrix")
    expect_equal(dim(ad_X.corrected), c(75, 231))
})


test_that("checking deflate_mtx", {
    data("AD_data")

    X <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    ad_pca <- mixOmics::pca(X, ncomp = 3)
    ad.def.mtx <- PLSDAbatch:::deflate_mtx(X, ad_pca$variates$X[, 1])
    ad_pca.def <- mixOmics::pca(ad.def.mtx, ncomp = 2)

    expect_is(ad.def.mtx, "matrix")
    expect_equal(ad_pca$variates$X[, 2], ad_pca.def$variates$X[, 1])
})


test_that("checking PLSDA", {
    data("AD_data")

    X <- SummarizedExperiment::assays(AD_data$EgData)$Clr_value
    Y.trt <- SummarizedExperiment::rowData(AD_data$EgData)$Y.trt
    names(Y.trt) <- rownames(AD_data$EgData)

    X.scale <- scale(X, center = TRUE, scale = TRUE)

    Y.trt.mat <- mixOmics::unmap(as.numeric(Y.trt))
    Y.trt.scale <- scale(Y.trt.mat, center = TRUE, scale = TRUE)

    ad_plsda.trt <- PLSDAbatch:::PLSDA(X.scale, Y.trt.scale, ncomp = 1)
    X.notrt <- ad_plsda.trt$defl_data$X

    expect_is(X.notrt, "matrix")
    expect_equal(dim(X.notrt), c(75, 231))
})
