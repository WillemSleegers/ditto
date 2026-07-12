#' Clean text for comparison
#'
#' Lowercases a string, removes punctuation, and collapses runs of
#' whitespace, so that surface differences that are not meaningful do not
#' distort similarity scores.
#'
#' @param x A character vector.
#' @return A cleaned character vector of the same length.
#' @seealso [compare_strings()], which compares text but does not clean it.
#' @examples
#' clean("To what extent do you AGREE?")
#' @export
clean <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("[[:punct:]]") |>
    stringr::str_squish()
}
