test_that("cumseq() create incremental sequence for a change in value", {
  x <- c(0, 0, 1, 2, 2)
  y <- c(1L, 1L, 2L, 3L, 3L)

  expect_identical(cumseq(x), y)
})
