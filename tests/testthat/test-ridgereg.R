data("iris")
x <- MASS::lm.ridge(Petal.Length~Species, data=iris, lambda = 2)
z <- ridgereg(formula=Petal.Length~Species, data=iris, 2)

test_that("Similar coefficients", {
  x1<- x$coef[[1]]/x$scales[[1]]
  x2 <- x$coef[[2]]/x$scales[[2]]
  expect_equal(z$coefficients[[2]], x1, tolerance = 1e-2)
  expect_equal(z$coefficients[[3]], x2, tolerance = 1e-2)
})

test_that("Same class",{
  expect_s3_class(z, "ridgereg")
})
