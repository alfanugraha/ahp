library(gtools)

test_that("generation works", {
  res <- generate_combinations(3)
  smpl <- matrix(c(1,2,1,3,2,3), nrow = 3, byrow = T)

  expect_equal(res, smpl)
})
