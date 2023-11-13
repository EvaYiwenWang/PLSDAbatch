test_that("checking linear_regres", {
  library(TreeSummarizedExperiment)
  data('AD_data')

  ad.clr <- assays(AD_data$EgData)$Clr_value
  ad.batch <- rowData(AD_data$EgData)$Y.bat
  ad.trt <- rowData(AD_data$EgData)$Y.trt
  names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
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

test_that("checking percentileofscore", {
  library(TreeSummarizedExperiment)
  data('AD_data')

  ad.clr <- assays(AD_data$EgData)$Clr_value
  ad.batch <- rowData(AD_data$EgData)$Y.bat
  ad.trt <- rowData(AD_data$EgData)$Y.trt
  names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
  trt.first.b <- ad.trt[ad.batch == '09/04/2015']
  ad.first.b.pn <- percentileofscore(ad.clr[ad.batch == '09/04/2015', ],
                                      which(trt.first.b == '0-0.5'))

  expect_is(ad.first.b.pn, 'data.frame')
  expect_equal(dim(ad.first.b.pn), c(9, 231))
})


test_that("checking percentile_norm", {
  library(TreeSummarizedExperiment)
  data('AD_data')

  ad.clr <- assays(AD_data$EgData)$Clr_value
  ad.batch <- rowData(AD_data$EgData)$Y.bat
  ad.trt <- rowData(AD_data$EgData)$Y.trt
  names(ad.batch) <- names(ad.trt) <- rownames(AD_data$EgData)
  ad.PN <- percentile_norm(data = ad.clr, batch = ad.batch,
                          trt = ad.trt, ctrl.grp = '0-0.5')

  expect_is(ad.PN, 'data.frame')
  expect_equal(dim(ad.PN), c(75, 231))
})


