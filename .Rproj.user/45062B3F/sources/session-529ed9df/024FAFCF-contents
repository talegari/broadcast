test_that("is_broadcastable works", {
  # Check that two matrices with the same dimensions are broadcastable.
  expect_true(is_broadcastable(array(1:4, dim = c(2, 2)), array(5:8, dim = c(2, 2))))

  # Check that two matrices with different dimensions are broadcastable.
  expect_true(is_broadcastable(array(1:4, dim = c(2, 2)), array(5:8, dim = c(4, 1))))

  # Check that two matrices with different dimensions are not broadcastable.
  expect_false(is_broadcastable(array(1:4, dim = c(2, 2)), array(5:8, dim = c(3, 3))))
})

test_that("raise_error_non_conformal works", {
  # Try to broadcast two matrices with different dimensions and expect an error.
  expect_error(raise_error_non_conformal(c(2, 2), c(3, 3)))
})

test_that("get_pairwise_broadcast_dim works", {
  # Get the pairwise broadcast dimension of two matrices with the same dimensions.
  expect_equal(get_pairwise_broadcast_dim(array(1:4, dim = c(2, 2)), array(5:8, dim = c(2, 2))), c(2, 2))

  # Get the pairwise broadcast dimension of two matrices with different dimensions.
  expect_equal(get_pairwise_broadcast_dim(array(1:4, dim = c(2, 2)), array(5:8, dim = c(4, 1))), c(4, 2))
})

test_that("broadcast_to works", {
  mat = array(1:4, dim = c(2, 2))
  expected_result = structure(c(1L, 2L, 1L, 2L, 3L, 4L, 3L, 4L, 1L, 2L, 1L, 2L, 3L, 4L, 3L, 4L),
                              dim = c(4L, 4L))
  expect_equal(broadcast_to(mat, c(4, 4)), expected_result)
})


test_that("array_pairwise works", {
  # Apply a function to each pair of elements in two matrices, broadcasting them to a common dimension first.
  mat1 = array(1:4, dim = c(2, 2))
  mat2 = array(5:8, dim = c(4, 1))
  expected_result = structure(c(1, 64, 1, 256, 243, 4096, 2187, 65536),
                              dim = c(4L, 2L))
  expect_equal(array_pairwise(mat1, mat2, function(x, y)
    x ^ y), expected_result)
})

test_that("%a+% works", {
  # Add two matrices, broadcasting them to a common dimension first.
  mat1 = array(1:4, dim = c(2, 2))
  mat2 = array(5:8, dim = c(4, 1))
  expected_result = structure(c(6L, 8L, 8L, 10L, 8L, 10L, 10L, 12L),
                              dim = c(4L, 2L))
  expect_equal(mat1 %a+% mat2, expected_result)
})

test_that("%a-% works", {
  # Subtract two matrices, broadcasting them to a common dimension first.
  mat1 = array(1:4, dim = c(2, 2))
  mat2 = array(5:8, dim = c(4, 1))
  expected_result = structure(c(-4L, -4L, -6L, -6L, -2L, -2L, -4L, -4L),
                              dim = c(4L, 2L))
  expect_equal(mat1 %a-% mat2, expected_result)
})

test_that("%a*% works", {
  # Multiply two matrices pointwise, broadcasting them to a common dimension first.
  mat1 = array(1:4, dim = c(2, 2))
  mat2 = array(5:8, dim = c(4, 1))
  expected_result = structure(c(5L, 12L, 7L, 16L, 15L, 24L, 21L, 32L),
                              dim = c(4L, 2L))
  expect_equal(mat1 %a*% mat2, expected_result)
})

test_that("%a/% works", {
  # Divide two matrices pointwise, broadcasting them to a common dimension first.
  mat1 = array(1:4, dim = c(2, 2))
  mat2 = array(5:8, dim = c(4, 1))
  expected_result = structure(
    c(
      0.2,
      0.333333333333333,
      0.142857142857143,
      0.25,
      0.6,
      0.666666666666667,
      0.428571428571429,
      0.5
    ),
    dim = c(4L, 2L)
  )
  expect_equal(mat1 %a/% mat2, expected_result)
})
