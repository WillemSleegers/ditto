#' Clean text for comparison
#'
#' Lowercases a string, removes punctuation, and collapses runs of
#' whitespace, so that surface differences that are not meaningful do not
#' distort similarity scores. A decimal mark between digits is kept, as is a
#' percent sign directly after a number, so `"6.2%"` stays `"6.2%"` rather
#' than becoming `"62"`. Space between a number and its percent sign is
#' removed, so `"6.2 %"` also becomes `"6.2%"`.
#'
#' @param x A character vector.
#' @param decimal_mark The decimal mark to keep when it appears between
#'   digits, either `"."` (the default) or `","`. The other mark is treated
#'   as ordinary punctuation, so thousands separators are removed.
#' @return A cleaned character vector of the same length.
#' @seealso [compare_strings()], which compares text but does not clean it.
#' @examples
#' clean("To what extent do you AGREE?")
#' clean("It rose 6,2% to 1.000", decimal_mark = ",")
#' @export
clean <- function(x, decimal_mark = c(".", ",")) {
  decimal_mark <- match.arg(decimal_mark)
  mark <- paste0("\\", decimal_mark)
  pattern <- paste0("(?!(?<=\\d)", mark, "(?=\\d))(?!(?<=\\d)%)[[:punct:]]")

  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("(?<=\\d)\\s+%", "%") |>
    stringr::str_remove_all(pattern) |>
    stringr::str_squish()
}
