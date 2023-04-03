test_that("checking partVar_plot", {
  library(vegan)
  data('AD_data')
  ad.clr <- AD_data$EgData$X.clr
  ad.batch <- AD_data$EgData$Y.bat
  ad.trt <- AD_data$EgData$Y.trt

  ad.factors.df <- data.frame(trt = ad.trt, batch = ad.batch)
  rda.res <- varpart(ad.clr, ~ trt, ~ batch,
                      data = ad.factors.df, scale = TRUE)

  ad.prop.df <- data.frame(Treatment = NA, Batch = NA,
                           Intersection = NA,
                           Residuals = NA)
  ad.prop.df[1,] <- rda.res$part$indfract$Adj.R.squared

  ad.prop.df <- ad.prop.df[, c(1,3,2,4)]

  ad.prop.df[ad.prop.df < 0] <- 0
  ad.prop.df <- as.data.frame(t(apply(ad.prop.df, 1, function(x){x/sum(x)})))

  res.pv <- partVar_plot(prop.df = ad.prop.df)
  expect_is(res.pv, 'ggplot')
})


