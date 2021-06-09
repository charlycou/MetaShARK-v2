#' @export
listToReactiveValues <- function(
  rv, lv = 0, name = "root"
) {
  if (missing(rv)) {
    stop("No rv provided")
  }
  if (!is.list(rv)) {
    return(rv)
  }
  
  # Remove NULL and list(NULL) values
  if(any(sapply(rv, identical, NULL)))
    rv <- rv[!sapply(rv, identical, NULL)]
  
  # Check for names
  if(any(isFALSE(sapply(names(rv), isContentTruthy)))) {
    .names <- replace(
      names(rv),
      names(rv) == "",
      unlist(rv[names(rv) == ""])
    )
    if(length(.names) == 0)
      .names <- unlist(rv)
    names(rv) <- .names
  }
  
  out <- lapply(seq_along(rv), function(sub) {
    rv[[sub]] <<- listToReactiveValues(
      rv[[sub]], lv + 1, name = names(rv)[sub]
    )
  })
  
  rv <- do.call(reactiveValues, rv)
  
  return(rv)
}
