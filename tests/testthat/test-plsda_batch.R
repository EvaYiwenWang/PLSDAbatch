test_that("checking PLSDA_batch", {
  library(TreeSummarizedExperiment)
  data('AD_data')

  X <- assays(AD_data$EgData)$Clr_value
  Y.trt <- rowData(AD_data$EgData)$Y.trt
  Y.bat <- rowData(AD_data$EgData)$Y.bat
  names(Y.bat) <- names(Y.trt) <- rownames(AD_data$EgData)
  ad_plsda_batch <- PLSDA_batch(X, Y.trt, Y.bat, ncomp.trt = 1, ncomp.bat = 5)
  ad_X.corrected <- ad_plsda_batch$X.nobatch

  expect_is(ad_plsda_batch, 'list')
  expect_equal(length(names(ad_plsda_batch)), 17)

  expect_is(ad_X.corrected, 'matrix')
  expect_equal(dim(ad_X.corrected), c(75, 231))
})


test_that("checking deflate_mtx", {
  data('AD_data')
  library(mixOmics)
  library(TreeSummarizedExperiment)

  X <- assays(AD_data$EgData)$Clr_value
  ad_pca <- pca(X, ncomp = 3)
  ad.def.mtx <- deflate_mtx(X, ad_pca$variates$X[ ,1])
  ad_pca.def <- pca(ad.def.mtx, ncomp = 2)

  expect_is(ad.def.mtx, 'matrix')
  expect_equal(ad_pca$variates$X[ ,2], ad_pca.def$variates$X[ ,1])
})


test_that("checking PLSDA", {
  data('AD_data')
  library(mixOmics)
  library(TreeSummarizedExperiment)

  X <- assays(AD_data$EgData)$Clr_value
  Y.trt <- rowData(AD_data$EgData)$Y.trt
  names(Y.trt) <- rownames(AD_data$EgData)

  X.scale <- scale(X, center = TRUE, scale = TRUE)

  Y.trt.mat <- unmap(as.numeric(Y.trt))
  Y.trt.scale <- scale(Y.trt.mat, center = TRUE, scale = TRUE)

  ad_plsda.trt <- PLSDA(X.scale, Y.trt.scale, ncomp = 1)
  X.notrt <- ad_plsda.trt$defl_data$X

  expect_is(X.notrt, 'matrix')
  expect_equal(dim(X.notrt), c(75,231))
})
