cleanModules <- function(main.env) {
  
  # Reset main.env$EAL
  main.env$EAL$history <- "SelectDP"
  main.env$EAL$old.page <- main.env$EAL$page
  main.env$EAL$page <- 1
  # Reset annotations variable
  main.env$local.annotations <- reactiveValues(
    annotation.table = data.frame(stringsAsFactors = FALSE),
    result.table = data.frame(stringsAsFactors = FALSE),
    tree.content =list()
  )
  
  return(main.env)
}