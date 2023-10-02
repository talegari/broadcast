#' @name is_broadcastable
#' @title Check if two matrices or their dimensions are broadcastable
#' @description If number of rows(or cols) of one matrix divides the other, then
#'   the pair is broadcastable
#' @param x A intergerish vector of size 2 representing the dim or matrix
#' @param y A intergerish vector of size 2 representing the dim or matrix
#' @return A boolean indicating whether the two arrays are broadcastable
#' @examples
#' x = array(1:6, dim = c(2, 3))
#' y = array(7:9, dim = c(3, 1))
#'
#' is_broadcastable(x, y)
#' is_broadcastable(dim(x), dim(y))
#' is_broadcastable(x, dim(y))
#' @export
is_broadcastable = function(x, y){

  if (is.matrix(x)){
    dim_x = dim(x)
  } else if (is.vector(x)){
    checkmate::assert_integerish(x,
                                 any.missing = FALSE,
                                 len = 2,
                                 lower = 1
                                 )
    dim_x = x
  }

  if (is.matrix(y)){
    dim_y = dim(y)
  } else if (is.vector(y)){
    checkmate::assert_integerish(y,
                                 any.missing = FALSE,
                                 len = 2,
                                 lower = 1
                                 )
    dim_y = y
  }

  flag = (max(dim_x[1], dim_y[1]) %% min(dim_x[1], dim_y[1]) == 0) &&
         (max(dim_x[2], dim_y[2]) %% min(dim_x[2], dim_y[2]) == 0)

  return(flag)
}

#' @name raise_error_non_conformal
#' @title Raise an error if two matrices are not broadcastable
#' @description Raises an error if the two matrices cannot be broadcast to have
#'   the same dimensions.
#' @param dim_mat1 A list of two integers representing the dimensions of the
#'   first matrix.
#' @param dim_mat2 A list of two integers representing the dimensions of the
#'   second matrix.
#' @return None
raise_error_non_conformal = function(dim_mat1, dim_mat2){

  if (!is_broadcastable(dim_mat1, dim_mat2)){
    stop(glue::glue(
      "Cannot broadcast {dim_mat1[1]} X {dim_mat1[2]} to {dim_vec[1]} X {dim_vec[2]}")
      )
  }
}

#' @name get_pairwise_broadcast_dim
#' @title Get the pairwise broadcast dimension of two matrices
#' @description Gets the pairwise broadcast dimension of two matrices, which is
#'   the maximum of the two matrices row and column dimensions.
#' @param mat1 A matrix.
#' @param mat2 A matrix.
#' @return A list of two integers representing the pairwise broadcast dimension
#'   of the two matrices.
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' m2 = array(5:8, dim = c(4, 1))
#'
#' get_pairwise_broadcast_dim(m1, m2)
#' @export
get_pairwise_broadcast_dim = function(mat1, mat2){

  raise_error_non_conformal(dim(mat1), dim(mat2))

  n_row = max(nrow(mat1), nrow(mat2))
  n_col = max(ncol(mat1), ncol(mat2))
  return(c(n_row, n_col))
}

#' @name broadcast_to
#' @title Broadcast a matrix to a given dimension
#' @description Broadcasts a matrix to a given dimension.
#' @param mat A matrix.
#' @param dim_vec A list of two integers representing the desired dimension.
#' @param check A boolean indicating whether to check if the matrix is
#'   broadcastable to the given dimension.
#' @return A matrix broadcast to the given dimension.
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' broadcast_to(m1, c(4, 6))
#' @export
broadcast_to = function(mat, dim_vec, check = TRUE){

  checkmate::assert_class(mat, "matrix")
  if (check) { raise_error_non_conformal(dim(mat), dim_vec) }

  mat = do.call(rbind,
                replicate(dim_vec[1] / nrow(mat), mat, simplify = FALSE)
                )

  mat = do.call(cbind,
                replicate(dim_vec[2] / ncol(mat), mat, simplify = FALSE)
                )

  return(mat)
}

#' @name %a+%
#' @title Add two matrices, broadcasting them to a common dimension first
#' @description Adds two matrices, broadcasting them to a common dimension first
#' @param mat1 A matrix.
#' @param mat2 A matrix.
#' @aliases array_add
#' @return A matrix containing the sum of `mat1` and `mat2`.
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' m2 = array(5:8, dim = c(4, 1))
#' m1 %a+% m2
#' @export
`%a+%` = function(mat1, mat2){
  new_dim = get_pairwise_broadcast_dim(mat1, mat2)

  broadcast_to(mat1, new_dim, check = FALSE) +
  broadcast_to(mat2, new_dim, check = FALSE)
  }

#' @name %a-%
#' @title Subtract two matrices, broadcasting them to a common dimension first
#' @description Subtracts two matrices, broadcasting them to a common dimension
#'   first
#' @aliases array_sub
#' @param mat1 A matrix.
#' @param mat2 A matrix.
#' @return A matrix containing the subtraction of `mat1` and `mat2`.
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' m2 = array(5:8, dim = c(4, 1))
#' m1 %a-% m2
#' @export
`%a-%` = function(mat1, mat2){
    new_dim = get_pairwise_broadcast_dim(mat1, mat2)

  broadcast_to(mat1, new_dim, check = FALSE) -
  broadcast_to(mat2, new_dim, check = FALSE)
}

#' @name %a*%
#' @title Multiply two matrices pointwise, broadcasting them to a common
#'   dimension first
#' @description Multiplies two matrices pointwise, broadcasting them to a common
#'   dimension first
#' @aliases array_mul
#' @param mat1 A matrix.
#' @param mat2 A matrix.
#' @return A matrix containing the pointwise multiplication of `mat1` and `mat2`
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' m2 = array(5:8, dim = c(4, 1))
#' m1 %a*% m2
#' @export
`%a*%` = function(mat1, mat2){
    new_dim = get_pairwise_broadcast_dim(mat1, mat2)

  broadcast_to(mat1, new_dim, check = FALSE) *
  broadcast_to(mat2, new_dim, check = FALSE)
}

#' @name %a/%
#' @title Divide two matrices pointwise, broadcasting them to a common
#'   dimension first
#' @description Divides two matrices pointwise, broadcasting them to a common
#'   dimension first
#' @aliases array_div
#' @param mat1 A matrix.
#' @param mat2 A matrix.
#' @return A matrix containing the pointwise division of `mat1` and `mat2`
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' m2 = array(5:8, dim = c(4, 1))
#' m1 %a/% m2
#' @export
`%a/%` = function(mat1, mat2){
    new_dim = get_pairwise_broadcast_dim(mat1, mat2)

  broadcast_to(mat1, new_dim, check = FALSE) /
  broadcast_to(mat2, new_dim, check = FALSE)
}

array_add = `%a+%`
array_sub = `%a-%`
array_mul = `%a*%`
array_div = `%a/%`

#' @name array_pairwise
#' @title Apply a function to each pair of elements in two arrays, broadcasting
#'   them to a common dimension first
#' @description Applies a function to each pair of elements in two arrays,
#'   broadcasting them to a common dimension first.
#' @param mat1 A matrix
#' @param mat2 A matrix
#' @param f A function that takes two arguments and returns a single value.
#' @param ... Additional arguments to be passed to the function `f`.
#' @return An array containing the results of applying `f` to each pair of
#'   elements in `mat1` and `mat2`
#' @examples
#' m1 = array(1:4, dim = c(2, 2))
#' m2 = array(5:8, dim = c(4, 1))
#' array_pairwise(m1, m2, function(x, y) x^y)
#' @export
array_pairwise = function(mat1, mat2, f, ...){

  new_dim = get_pairwise_broadcast_dim(mat1, mat2)

  mat_1 = broadcast_to(mat1, new_dim, check = FALSE)
  mat_2 = broadcast_to(mat2, new_dim, check = FALSE)

  f(mat_1, mat_2)
}
