test_that("checking Scatter_Density", {
  library(mixOmics)
  data('AD_data')
  ad.clr <- AD_data$EgData$X.clr
  ad.pca.before <- pca(ad.clr, ncomp = 3, scale = TRUE)
  ad.batch <- AD_data$EgData$Y.bat
  ad.trt <- AD_data$EgData$Y.trt
  res.sdplot <- Scatter_Density(object = ad.pca.before, batch = ad.batch, trt = ad.trt)
  expect_is(res.sdplot, 'gtable')
  expect_is(res.sdplot, 'grob')
})


test_that("checking box_plot", {
  data('AD_data')
  ad.clr <- AD_data$EgData$X.clr
  ad.batch <- AD_data$EgData$Y.bat
  ad.df <- data.frame(value = ad.clr[,1], batch = ad.batch)
  res.bplot <- box_plot(df = ad.df, title = 'OTU 12', x.angle = 30)
  expect_is(res.bplot, 'ggplot')
})


test_that("checking density_plot", {
  data('AD_data')
  ad.clr <- AD_data$EgData$X.clr
  ad.batch <- AD_data$EgData$Y.bat
  ad.df <- data.frame(value = ad.clr[,1], batch = ad.batch)
  res.dplot <- density_plot(df = ad.df, title = 'OTU 12')
  expect_is(res.dplot, 'ggplot')
})
