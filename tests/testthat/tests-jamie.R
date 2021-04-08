test_that("Score questionnaire items by adding them up.", {
  test_df <- data.frame(i1 = 1:4,
                        i2 = 1:4,
                        i3 = 1:4,
                        i4 = 1:4,
                        i5 = 1:4)
  expect_equal(score_surveys(test_df), c(5, 10, 15, 20))
  expect_equal(score_surveys(test_df, 2), c(5, 10, 15, 20))
})

test_that("Test pomp function.", {
  scores <- c(5, 10, 15, 20, 25)
  expect_equal(pomp(scores, 5, 25), c(0, 25, 50, 75, 100))
})
