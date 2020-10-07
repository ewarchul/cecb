testthat::test_that("operator properly concatenate strings", {
	lhs = "Hello"
	rhs = ", World"
	output = lhs %++% rhs
	testthat::expect_equal(output, "Hello, World")
})
testthat::test_that("alias for operator works correctly", {
	values = c(1, 0, 1)
	c(a, b, c) %<~% values
	testthat::expect_equal(b, 0)
	testthat::expect_equal(c(a, b, c), values)
})

