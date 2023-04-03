test_that("checking PLSDA_batch", {
  data('AD_data')
  X <- AD_data$EgData$X.clr
  Y.trt <- AD_data$EgData$Y.trt
  Y.bat <- AD_data$EgData$Y.bat
  ad_plsda_batch <- PLSDA_batch(X, Y.trt, Y.bat, ncomp.trt = 1, ncomp.bat = 5)
  ad_X.corrected <- ad_plsda_batch$X.nobatch

  expect_is(ad_plsda_batch, 'list')
  expect_equal(length(names(ad_plsda_batch)), 17)

  expect_is(ad_X.corrected, 'matrix')
  expect_equal(dim(ad_X.corrected), c(75, 231))
})
