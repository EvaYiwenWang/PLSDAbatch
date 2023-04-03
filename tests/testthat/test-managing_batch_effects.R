test_that("checking linear_regres", {
  data('AD_data')
  ad.clr <- AD_data$EgData$X.clr
  ad.batch <- AD_data$EgData$Y.bat
  ad.trt <- AD_data$EgData$Y.trt
  ad.lm <- linear_regres(data = ad.clr, trt = ad.trt,
                         batch.fix = ad.batch,
                         type = 'linear model')
  ad.p.adj <- ad.lm$adj.p
  ad.aic <- ad.lm$AIC

  expect_is(ad.lm, 'list')

  expect_is(ad.p.adj, 'numeric')
  expect_equal(as.numeric(round(head(ad.p.adj), digits = 3)),
               c(0.322, 0.013, 0.488, 0.000, 0.089, 0.001))

  expect_is(ad.aic, 'data.frame')
  expect_equal(as.numeric(round(ad.aic[1,], digits = 3)),
               c(208.563, 164.136))

})


test_that("checking percentile_norm", {
  data('AD_data')
  ad.clr <- AD_data$EgData$X.clr
  ad.batch <- AD_data$EgData$Y.bat
  ad.trt <- AD_data$EgData$Y.trt
  ad.PN <- percentile_norm(data = ad.clr, batch = ad.batch,
                          trt = ad.trt, ctrl.grp = '0-0.5')

  expect_is(ad.PN, 'data.frame')
  expect_equal(dim(ad.PN), c(75, 231))
})


