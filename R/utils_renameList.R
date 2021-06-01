renameList <- function(
  st, lv = 0, name = "root"
) {
  if (missing(st)) {
    stop("No structure provided")
  }
  if (!is.list(st)) {
    return(st)
  }
  
  # Remove .attrs (R attributes for list)
  if(".attrs" %in% names(st))
    st <- st[names(st) != ".attrs"]
  
  # Remove NULL and list(NULL) values
  if(any(sapply(st, identical, NULL)))
    st <- st[!sapply(st, identical, NULL)]
  
  # Recurse
  lapply(seq_along(st), function(sub) {
    st[[sub]] <<- renameList(
      st[[sub]], lv + 1, name = names(st)[sub]
    )
  })
  
  # if duplicated names and different.names is set, suffix numbers
  if(any(duplicated(names(st)))) {
    .duplicated <- names(st)[duplicated(names(st))]
    sapply(.duplicated, function(string) {
      indexes <- which(names(st) == string)
      new <- paste0(string, "_", 1:length(indexes))
      names(st)[indexes] <<- new
    })
  }
  
  return(st)
}
