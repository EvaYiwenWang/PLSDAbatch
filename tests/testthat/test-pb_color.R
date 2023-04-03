test_that("checking pb_color", {
  res.pbcolor <- pb_color(seq_len(5))
  expect_is(res.pbcolor, 'character')
  expect_equal(res.pbcolor[1], '#F8766D')
})
