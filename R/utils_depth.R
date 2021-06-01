depth <- function(this,thisdepth=0, mode = "max"){
  stopifnot(mode %in% c("min", "max"))
  
  if(!is.list(this)){
    return(thisdepth)
  }else{
    depths <- unlist(lapply(this,depth,thisdepth=thisdepth+1))
    return(switch(
      mode,
      min = min(depths),
      max = max(depths)
    ))    
  }
}