# Build a tree list out of annotations.txt table
# TODO make this recursive
#' @noRd
buildAnnotationTree <- function(annotation.table) {
  .built <- list()
  .toremove.sub <- c()
  sapply(unique(annotation.table$context), function(con)
    .built[[con]] <<- list()
  )
  sapply(1:nrow(annotation.table), function(ind) {
    row <- annotation.table[ind,]
    sub <- row$subject
    con <- row$context
    
    # If subject is not a context,
    if(isFALSE(sub %in% unique(annotation.table$context))) {
      # add it to its context
      .built[[con]] <<- c(.built[[con]], sub)
    }
    .toremove.sub <<- c(.toremove.sub, isFALSE(sub %in% unique(annotation.table$context)))
  })
  annotation.table <- annotation.table %>%
    filter(subject %in% annotation.table$subject[!.toremove.sub]) %>%
    select(c(context, subject))
  sapply(nrow(annotation.table):1, function(ind) {
    row <- annotation.table[ind,]
    
    .built[[row$context]] <<- c(.built[row$subject], .built[[row$context]])
    .built[[row$subject]] <<- NULL
  })
  if(names(.built) == "eml")
    .built <- .built[[1]]
  # Turn into structures
  .tree <- listToStructure(
    .built,
    node.attributes = list(sticon = "project-diagram", stopened = TRUE)
  )
  
  return(.tree)
}

#' @noRd
cleanAnnotation <- function(main.env) {
  main.env$local.annotations$annotation.table <- data.frame()
  main.env$local.annotations$tree.content <- c()
  
  return(main.env)
}

getPredicate <- function(api.key, ontology = "OBOREL", predicate) {
  if(missing(api.key) || !isTruthy(api.key)) {
    warning("No api key provided to getPredicate. Returning NULL.")
    return(NULL)
  }
  
  # Check `ontology` is available in BioPortal
  predicate.classes <- cedarr::accessOntology(api.key, ontology, item = "classes")
  # Search for `predicate` in `ontology` classes
  predicate.id <- functionToFindApproximatelyATermAmongOthers()
  predicate.found <- cedarr::accessClass(api.key, ontology, predicate.id)
  
  return(predicate.found)
}

getObject <- function(ontology, object) {
  
}

# TODO Build a row:
# 
# Idea
# 
# Use a modalDialog for each annotation written.
# 
# Content
# 
# 1. Id: /%1/%2/... 
# values according to the context (as named as possible)
# Levels:
# /dataset
# /<file.name>
#   /<attribute>
# /<person.name>
# 
# 2. element: /%1/%2
# values according to the context (unnamed)
# /dataset
# /dataTable OR /otherEntity
#   /attribute
# /ResponsibleParty
# 
# 3. context: %1
# upper level of current item
# eml
# dataset
#   <file.name> (from dataset)
# dataset
# 
# 4. subject: 
# last item of Id
# 
# 5. predicate_label:
# name of predicate
# 
# 6. predicate URI
# uri of predicate
# 
# 7 & 8. same as predicate for object
# have a search engine to get 