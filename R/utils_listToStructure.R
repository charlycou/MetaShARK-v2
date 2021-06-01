listToStructure <- function(
  st, lv = 0, name = "root", 
  node.attributes = list(), 
  root.attributes = node.attributes,
  leaf.attributes = node.attributes
) {
  if (missing(st)) {
    stop("No structure provided")
  }
  if (!is.list(st)) {
    return(structure(st))
  }
  
  # Remove .attrs (R attributes for list)
  if(".attrs" %in% names(st))
    st <- st[names(st) != ".attrs"]
  
  # Remove NULL and list(NULL) values
  if(any(sapply(st, identical, NULL)))
    st <- st[!sapply(st, identical, NULL)]
  
  # Check for names
  .names <- replace(
    names(st),
    names(st) == "",
    unlist(st[names(st) == ""])
  )
  if(length(.names) == 0)
    .names <- unlist(st)
  
  out <- lapply(seq_along(st), function(sub) {
    subst <- listToStructure(
      st[[sub]], lv + 1, name = .names[sub],
      node.attributes = node.attributes,
      root.attributes = root.attributes,
      leaf.attributes = leaf.attributes
    )
    st[[sub]] <<- do.call(
      structure,
      args = c(
        .Data = list(subst),
        {
          if(lv == 0) root.attributes else
            if(!is.list(subst)) leaf.attributes else
              node.attributes
        }
      )
    )
  })
  
  # if(length(.names) != length(st))
  #   browser()
  names(st) <- .names
  
  return(st)
}
