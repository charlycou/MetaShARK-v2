#' @importFrom XML newXMLNode xmlValue
listToXML <- function(node, sublist){
  children <- seq_along(sublist)[names(sublist) != ".attrs"]
  
  sapply(children, function(child){
    child_name <- names(sublist)[[child]]
    child_attr <- if(".attrs" %in% names(sublist[[child]]))
      sublist[[child]]$.attrs else
        NULL
    child_node <- newXMLNode(child_name, parent=node, attrs = child_attr)
    
    if (typeof(sublist[[child]]) == "list"){
      if("text" %in% names(sublist[[child]]))
        xmlValue(child_node) <- sublist[[child]]$text
      else
        listToXML(child_node, sublist[[child]])
    }
    else {
      xmlValue(child_node) <- sublist[[child]]
    }
    
  })
  
  return(node)
}
