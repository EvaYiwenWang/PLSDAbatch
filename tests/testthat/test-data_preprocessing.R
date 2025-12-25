test_that("checking PreFL", {
    data("AD_data")
    ad.count <- SummarizedExperiment::assays(AD_data$FullData)$Count # microbial count data
    ad.filter.res <- PreFL(data = ad.count)
    ad.zero.prob.before <- ad.filter.res$zero.prob.before
    ad.filter <- ad.filter.res$data.filter

    expect_is(ad.zero.prob.before, "numeric")
    expect_equal(round(ad.zero.prob.before, digits = 3), 0.633)

    expect_is(ad.filter, "matrix")
})
