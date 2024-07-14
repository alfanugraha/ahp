#' Generate a set of criteria of preferences from AHP
#'
#' @param n A number of defined criteria
#' @param ... Arguments of combinations function
#'
#' @return generate_combinations returns an array of matrix object
#' @import gtools
#' @export
#'
#' @examples
#' generate_combinations(3)
generate_combinations <- function(n, ...){
  num_options <- ((n-1) ^ 2 + (n-1)) / 2
  sets <- combinations(num_options, 2, repeats.allowed = F, v=1:num_options, ...)
  sets
}
