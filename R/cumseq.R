#' Create a sequence cumulatively
#'
#' Create an incremental sequence of numbers where there is a change
#' in the value of a vector.
#'
#' @param x A vector
#' @param fill_missing A value to replace missing values
#'
#' @return
#' @export
#'
#' @examples
#' cumseq(c(0, 0, 1, 0, 1, 1, 0))
#' cumseq(c(0, 0, 1, 0, NA, NA, 0), fill_missing = 9)
#'
cumseq <- function(x, fill_missing = NULL) {
  stopifnot("fill_missing must not be a NA" = !is.na(fill_missing))

  if (anyNA(x) & is.null(fill_missing))
    stop("Missing values found. Add a value in fill_missing.")

  if (!is.null(fill_missing)) {
    compare_class <- all.equal(class(x), class(fill_missing))
    stopifnot("Classes of x and fill_missing must be the same." = compare_class)

    x[is.na(x)] <- fill_missing
  } else {
    x
  }

  r <- rle(x)
  rep(seq_along(r$values), times = r$lengths)
}


