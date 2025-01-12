#' @noRd
templateModules <- function(main.env, page){
  if(is.null(main.env$local.rv))
    stop("No content provided.")
  if(is.null(main.env$save.variable))
    stop("No save variable provided")
  
  if(page %in% c(1,2,3,6))
    do.call(
      what = switch(
        as.character(page),
        "1" = ".templateInit",
        "2" = ".templateAttributes",
        "3" = ".templateCV_GeoCov",
        "6" = ".templateTaxCov"
      ),
      args = list(
        main.env
      )
    )
}

#' @import shiny
#' @importFrom EMLassemblyline template_directories template_core_metadata
#'
#' @noRd
.templateInit <- function(main.env) {
  dir.create(
    main.env$save.variable$SelectDP$dp.path,
    recursive = TRUE
  )
  
  if(exists("template_issues")) 
    rm("template_issues", envir = .GlobalEnv)
    
  x <- try({
    EMLassemblyline::template_directories(
      main.env$save.variable$SelectDP$dp.path,
      main.env$save.variable$SelectDP$dp.name
    )
    
    EMLassemblyline::template_core_metadata(
      main.env$save.variable$SelectDP$dp.metadata.path,
      main.env$local.rv$dp.license()
    )
    
    if(isTRUE(main.env$wip))
      EMLassemblyline::template_annotations(
        main.env$save.variable$SelectDP$dp.metadata.path,
        main.env$save.variable$SelectDP$dp.data.path,
        dir(main.env$save.variable$SelectDP$dp.data.path)
      )
    
    # Check for EAL issues
    if(exists("template_issues")) 
      stop("EAL issues")
  })
  
  # End set up variable
  if (class(x) != "try-error") {
    main.env$local.rv$dp.list <- c(
      main.env$local.rv$dp.list, 
      main.env$local.rv$dp.name()
    )
    main.env$EAL$old.page <- main.env$EAL$page
    main.env$EAL$page <- main.env$EAL$page + 1
  } else { # Remove all that has been done
    unlink(
      main.env$save.variable$SelectDP$dp.path,
      recursive = TRUE
    )
    main.env$save.variable <- initReactive(main.env = main.env$EAL)
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  }
  
  return(x)
}

#' @import shiny
#' @importFrom EMLassemblyline template_table_attributes
#'
#' @noRd
.templateAttributes <- function(main.env){
  if(exists("template_issues")) 
    rm("template_issues", envir = .GlobalEnv)
  
  x <- try({
    EMLassemblyline::template_table_attributes(
      path = isolate(main.env$save.variable$SelectDP$dp.metadata.path),
      data.path = isolate(main.env$save.variable$SelectDP$dp.data.path),
      data.table = isolate(main.env$save.variable$DataFiles$name)
    )
    
    # Check for EAL issues
    if(exists("template_issues")) 
      stop("EAL issues")
  })
  
  if(class(x) == "try-error") {
    isolate({main.env$EAL$page <- main.env$EAL$page - 1})
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  }
}

#' @import shiny
#' @importFrom EMLassemblyline template_categorical_variables template_geographic_coverage
#' 
#' @noRd
.templateCV_GeoCov <- function(main.env){
  devmsg(main.env$EAL$page, tag = "template")
  # for each attribute data frame
  md.tables <- if(main.env$EAL$page == 3)
    main.env$local.rv$md.tables else
      main.env$save.variable$Attributes$content
  
  # loop required to check each 'class' column -- replaced in savevariable_functions.R by a reactive()
  .do.template.catvars <- if(main.env$EAL$page == 3)
    main.env$local.rv$use.catvars() else
      main.env$save.variable$Attributes$use.catvars
  
  if(exists("template_issues")) 
    rm("template_issues", envir = .GlobalEnv)
  
  # EMLAL: template new fields if needed
  x <- try({
    if (isTRUE(.do.template.catvars)) {
      EMLassemblyline::template_categorical_variables(
        path = main.env$save.variable$SelectDP$dp.metadata.path,
        data.path = main.env$save.variable$SelectDP$dp.data.path
      )
    }
    
    # Check for EAL issues
    if(exists("template_issues")) 
      stop("EAL template issues - CatVar")
    
    # Template if not existing
    if("GeoCov" %in% names(main.env$save.variable)) {
      EMLassemblyline::template_geographic_coverage(
        path = main.env$save.variable$SelectDP$dp.metadata.path,
        data.path = main.env$save.variable$SelectDP$dp.data.path,
        empty = TRUE,
        write.file = TRUE
      )
    }
    # Check for EAL issues
    if(exists("template_issues"))
      stop("EAL template issues - GeoCov")
    
    return("done")
  })
  
  if(class(x) == "try-error") {
    isolate({main.env$EAL$page <- main.env$EAL$page - 1})
    devmsg(EMLassemblyline::issues(), tag = "on template")
    showNotification(
      x,
      type = "error",
      closeButton = TRUE,
      duration = NULL
    )
  } else # skip to Geographic Coverage
  if (isFALSE(.do.template.catvars)) {
    isolate({main.env$EAL$page <- main.env$EAL$page + 1})
  }
}

#' @import shiny
#' @importFrom EMLassemblyline template_taxonomic_coverage
#' 
#' @noRd
.templateTaxCov <- function(main.env){
  if(exists("template_issues")) 
    rm("template_issues", envir = .GlobalEnv)
  
  if (isTRUE(main.env$local.rv$complete)) {
    showModal(
      modalDialog(
        title = "Templating taxonomic coverage",
        tagList(
          tags$h3("Taxonomic coverage is being processed"),
          "Please wait until completion. This might take minutes.",
          "Selected authorities being queried:",
          main.env$FORMATS$taxa.authorities |>
            filter(id == main.env$local.rv$taxa.authority) |>
            select(authority) |>
            unlist() |> 
            lapply(tags$li) |> 
            tagList() |> 
            tags$ul()
          
        ),
        footer = NULL
      )
    )
    
    x <- try({
      EMLassemblyline::template_taxonomic_coverage(
        main.env$save.variable$SelectDP$dp.metadata.path,
        main.env$save.variable$SelectDP$dp.data.path,
        taxa.table = main.env$local.rv$taxa.table,
        taxa.col = main.env$local.rv$taxa.col,
        taxa.name.type = main.env$local.rv$taxa.name.type,
        taxa.authority = main.env$local.rv$taxa.authority
      )
      
      # Check for EAL issues
      if(exists("template_issues")) 
        stop("EAL issues")
    })
    
    removeModal()
    
    if(class(x) == "try-error")
      showNotification(
        x,
        type = "error",
        closeButton = TRUE,
        duration = NULL
      )
    else
      showNotification(
        "Taxonomic Coverage has been written.",
        type = "message"
      )
  } else {
    showNotification(
      "Taxonomic Coverage has been skipped.",
      type = "message"
    )
  }
}

checkTemplates <- function(main.env) {
  pat <- switch(
    as.character(main.env$EAL$page),
    `3` = "^attribute",
    `4` = "^catvar",
    `5` = "^geographic_coverage",
    `6` = "^taxonomic_coverage",
    ""
  )
  
  check <- isContentTruthy(
    dir(
      main.env$save.variable$SelectDP$dp.metadata.path,
      pattern = pat
    )
  )
  
  if(isFALSE(check))
    templateModules(
      main.env, 
      switch(
        as.character(main.env$EAL$page),
        `3` = 2,
        `4` = 3,
        `5` = 3,
        `6` = 6
      ))
}
