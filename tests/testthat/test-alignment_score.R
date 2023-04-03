test_that("checking alignment_score", {
    data('sponge_data')
    X <- sponge_data$X.clr
    batch <- sponge_data$Y.bat
    res.alscore <- alignment_score(data = X, batch = batch, var = 0.95, k = 3, ncomp = 20)
    expect_is(res.alscore, 'numeric')
    expect_equal(round(res.alscore, digits = 3), 0.054)
 })
