test_that("checking Scatter_Density", {
  library(mixOmics)
  library(TreeSummarizedExperiment)

  data('AD_data')
  ad.clr <- assays(AD_data$EgData)$Clr_value
  ad.pca.before <- pca(ad.clr, ncomp = 3, scale = TRUE)
  ad.batch <- rowData(AD_data$EgData)$Y.bat
  ad.trt <- rowData(AD_data$EgData)$Y.trt
  names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
  res.sdplot <- Scatter_Density(object = ad.pca.before, batch = ad.batch, trt = ad.trt)
  expect_is(res.sdplot, 'gtable')
  expect_is(res.sdplot, 'grob')
})


test_that("checking box_plot", {
  library(TreeSummarizedExperiment)
  data('AD_data')

  ad.clr <- assays(AD_data$EgData)$Clr_value
  ad.batch <- rowData(AD_data$EgData)$Y.bat
  names(ad.batch) <- rownames(AD_data$EgData)
  ad.df <- data.frame(value = ad.clr[,1], batch = ad.batch)
  res.bplot <- box_plot(df = ad.df, title = 'OTU 12', x.angle = 30)
  expect_equal(colnames(ad.clr)[1], 'OTU12')
  expect_is(res.bplot, 'ggplot')
})


test_that("checking density_plot", {
  library(TreeSummarizedExperiment)
  data('AD_data')

  ad.clr <- assays(AD_data$EgData)$Clr_value
  ad.batch <- rowData(AD_data$EgData)$Y.bat
  names(ad.batch) <- rownames(AD_data$EgData)
  ad.df <- data.frame(value = ad.clr[,1], batch = ad.batch)
  res.dplot <- density_plot(df = ad.df, title = 'OTU 12')
  expect_equal(colnames(ad.clr)[1], 'OTU12')
  expect_is(res.dplot, 'ggplot')
})
