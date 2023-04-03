test_that("checking PreFL", {
  data('AD_data')
  ad.count <- AD_data$FullData$X.count # microbial count data

  ad.filter.res <- PreFL(data = ad.count)

  ad.zero.prob <- ad.filter.res$zero.prob

  ad.filter <- ad.filter.res$data.filter

  expect_is(ad.zero.prob, 'numeric')
  expect_equal(round(ad.zero.prob, digits = 3), 0.633)

  expect_is(ad.filter, 'matrix')
})
