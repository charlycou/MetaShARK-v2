assignPath <- function(x, path, value) {
  stopifnot(is.character(path))
  stopifnot(length(path) > 0)
  
  if(length(path) > 1)
    x[[path[1]]] <- assignPath(x[[path[1]]], path[-1], value)
  if(length(path) == 1)
    x[[path]] <- value
  
  return(x)
}