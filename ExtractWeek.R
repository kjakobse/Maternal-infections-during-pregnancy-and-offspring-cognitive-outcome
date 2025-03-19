ExtractWeek <- function(tbl, term) {
  nums <- stringr::str_extract_all(tbl$term, "\\d")
  term_out <- purrr::map_chr(
    nums,
    function(x) {
      if(x[1] == "4" && x[2] == "0") {
        out <- "\u226540"
      } else if (length(x) == 2) {
        out <- paste0(x[1], "-", x[2])
      } else {
        out <- paste0(x[1], x[2], "-", x[3], x[4])
      }
      return(out)
    }
  )
  return(term_out)
}