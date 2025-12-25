test_that("checking pb_color", {
    res.pbcolor <- pb_color(seq_len(5))
    expect_is(res.pbcolor, "character")
    expect_equal(res.pbcolor[1], "#F8766D")
})


test_that("checking lighten", {
    res.lighten <- lighten(c("red", "blue"), amount = 0.1)
    expect_is(res.lighten, "character")
    expect_equal(res.lighten[1], "#FF1A1A")
})

test_that("checking darken", {
    res.darken <- darken(c("red", "blue"), amount = 0.1)
    expect_is(res.darken, "character")
    expect_equal(res.darken[1], "#E60000")
})
