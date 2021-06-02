annotationsUI <- function(id) {
  ns <- NS(id)
  
  modalDialog(
    title = sprintf("Annotations"),
    tags$div(
      id = "help",
      tags$p("This window lets you annotate your dataset by adding semantic 
             annotations to it. MetaShARK relies on the CEDAR resource center to
             get adequate terms among more than 900 ontologies."),
      tags$p("If you're not used to ontologies, click below to get further 
             explanations."),
      # Help ====
      collapsibleUI(
        ns("help"),
        tagList(
          tags$h3("Semantic annotations"),
          tags$p(tags$b("Semantic annotation"), "is a way to reference any data and
               make it understandable without ambiguity. For this, one uses a 
               referenced vocabulary (also known as ", tags$b("controlled vocabulary"),
               "or", tags$b("ontologies"), "). Anotating a dataset is the action
               of adding terms coming from defined ontologies to describe the 
               content of the data. According to this practice, anyone can refer
               to the origin of the annotation and get a clear and shared definition
               of the terms used to describe the data."),
          tags$p("Ontologies can be produced by multiple groups: laboratories, scientific
               assemblies, researcher teams, ... They are usually published at OWL
               format. These objects represent a large volume and might be quite
               complex."),
          tags$p("MetaShARK uses", tags$code("{cedarr}"),"a lightweight R package 
               to query the controlled vocabularies remotely. These vocabularies
               are centralized in the CEDAR (Centor for Expanded Data Annotation 
               and Retrieval). Get more information at:", tags$a("CEDAR website",
               href="https://metadatacenter.org/"), ".")
        )
      )
    ),
    tags$div(
      id = ns("content"),
      # Tree ====
      column(
        3,
        "Your dataset can be annotated on multiple levels: click any item of the
        tree there after.",
        shinyTree(ns("tree"))
      ),
      column(
        9,
        # Annotation ====
        tags$div(
          tags$h3(textOutput(ns("selected_node"))),
          tags$div(id = "inserthere_annotations"),
          actionButton(ns("add_ui"), label = NULL, icon = icon("plus"))
        ),
        # CEDAR browser ====
        tags$div(
          tags$h3("CEDAR browser"),
          selectizeInput(
            ns("cedar_ontology_list"),
            "Select one or more ontology (default to all); search one by typing.",
            choices = main.env$SEMANTICS$ontologies
          ),
          shinyWidgets::searchInput(
            ns("cedar_search"),
            "Type terms to build your annotation"
          )
        )
      )
    ),
    footer = tags$span(
      actionButton(ns("cancel"), "Cancel"),
      shinyjs::disabled(actionButton(ns("validate"), "Validate"))
    ),
    size = "l",
    easyClose = FALSE
  )
}

annotations <- function(id, label, main.env) {
  moduleServer(id, function(input, output, session) {
    # Require cedarr token
    
    
    # On load
    # Tree ====
    # Update tree with content from setup step
    observeEvent(main.env$local.annotations$tree.content, {
      req(main.env$EAL$page != 1) 
      req(isContentTruthy(main.env$local.annotations$tree.content))
      devmsg("update tree", tag = "annotations")
      shinyTree::updateTree(
        session = session, 
        treeId = "tree",
        data = main.env$local.annotations$tree.content
      )
    }, 
    priority = -1,
    label = "Annotations: update tree"
    )
    
    # - Annotation UI
    # -- make a way to search URI
    # On cancel, return nothing
    observeEvent(input$cancel, {
      main.env <- cleanAnnotation(main.env)
      removeModal()
    })
  
    # On validate
    observeEvent(input$validate, {
      # - return modified save.variable
      # - save save.variable
      # - write new annotations.txt
    })
  })
}

#' #' @importFrom cedarr search
#' #' @importFrom shinyjs disabled enable disable
#' annotations <- function(input, output, session, save.variable, main.env) {
#'   ns <- session$ns
#'
#'   # Initialize variables ----
#'   rv <- reactiveValues(
#'     annotations = data.frame(
#'       id = character(),
#'       element = character(),
#'       context = character(),
#'       subject = character(),
#'       predicate_label = character(),
#'       predicate.uri = character(),
#'       object.label = character(),
#'       object.uri = character()
#'     ),
#'     file = paste(
#'       save.variable$SelectDP$dp.metadata.path,
#'       "annotations.txt",
#'       collapse = "/"
#'     )
#'   )
#'   if(file.exists(rv$file))
#'     rv$annotations <- fread(rv$file)
#'   else
#'     fwrite(rv$annotations, rv$file)
#'
#'   # Set UI ----
#'   # Default terms
#'   # Custom terms
#'   onclick("addui", {
#'     onInsertUI <- modalDialog(
#'       title = "Select an element to annotate",
#'       ... = tagList(
#'
#'       ),
#'       footer = tagList(
#'         modalButton("Cancel"),
#'         actionButton(
#'           NS(id, "insert"),
#'           "New annotation"
#'         ) %>% disabled
#'       )
#'     )
#'
#'
#'
#'     insertAnnot(
#'       as.character(id),
#'       rv,
#'       ns,
#'       main.env
#'     )
#'   })
#'
#'   # Process data ----
#'
#'   # Output ----
#'   return(save.variable)
#' }
#'
#' #' @title insertPersonnelInput
#' #'
#' #' @description helper function to insert PersonnelInput* functions. Calling this from
#' #' a shiny server will insert PersonnelInputUI and create its server part. Provided with
#' #' features to delete them.
#' #'
#' #' @import shiny
#' insertAnnotInput <- function(id, rv, ns, main.env, value = NULL) {
#'
#'   # initialize IDs ----
#'   div_id <- id
#'   site_id <- paste0("site_", id)
#'   rmv_id <- paste0("rmv_", id)
#'
#'   # Proper module server ----
#'   # insert new UI
#'   newUI <- AnnotModUI(
#'     ns(id), div_id, site_id, rmv_id,
#'     value = value
#'   )
#'   insertUI(
#'     selector = paste0("#", NS(id, "inserthere")),
#'     ui = newUI
#'   )
#'
#'   # create associated server
#'   rv <- callModule(
#'     AnnotMod, id, # module args
#'     main.env, rv, # reactiveValues
#'     rmv_id, site_id, div_id, # renderUI ids
#'     value = value # set saved
#'   )
#'
#'   # Output ----
#'   return(rv)
#' }
#'
#' #' @title AnnotModUI
#' #'
#' #' @description module to document EML annotation
#' #'
#' #' @importFrom shinyBS bsTooltip
#' AnnotModUI <- function(id, div_id, site_id, rmv_id, value = NULL) {
#'   ns <- NS(id)
#'
#'   value <- if(isContentTruthy(value)){
#'     value[value$id == div_id,]
#'   } else {
#'     rep(NA, 3)
#'   }
#'
#'   tags$div(
#'     id = site_id,
#'     fluidRow(
#'       class = "inputBox",
#'       column(11,
#'         column(4,
#'           actionButton(
#'             paste0("subject_", id),
#'             if(is.na(value[1])) "[Subject]" else value[1]
#'           )
#'         ),
#'         column(4,
#'           actionButton(
#'             paste0("predicate_", id),
#'             if(is.na(value[2])) "[Predicate]" else value[2]
#'           )
#'         ),
#'         column(4,
#'           actionButton(
#'             paste0("object_", id),
#'             if(is.na(value[3])) "[Object]" else value[3]
#'           )
#'         )
#'       ),
#'       column(1,
#'         if (is.null(value)) {
#'           actionButton(
#'             ns(rmv_id),
#'             "",
#'             icon("trash"),
#'             class = "danger"
#'           )
#'         },
#'         style = "padding-left: 0"
#'       )
#'     )
#'   )
#' }
#'
#' #' @title AnnotMod
#' #'
#' #' @describeIn AnnotModUI
#' #'
#' #' @import shiny
#' #' @importFrom rorcid as.orcid orcid_person orcid_employments orcid_email orcid_fundings
#' #' @importFrom stringr str_extract
#' AnnotMod <- function(input, output, session, main.env,
#'   rv, rmv_id, site_id, ref, role = NULL, saved = NULL) {
#'   ns <- session$ns
#'
#'   # Variable initialization ----
#'   if(!is.null(saved)){
#'     value <- saved[saved$id == ref,]
#'   } else {
#'     value <- NULL
#'   }
#'
#'   local.rv <- reactiveValues(
#'     ref = ref,
#'     id = if(!is.null(value)) value$id
#'       else character(),
#'     element = if(!is.null(value)) value$element
#'       else character(),
#'     context = if(!is.null(value)) value$context
#'       else character(),
#'     subject = if(!is.null(value)) value$subject
#'       else "[Subject]",
#'     predicate_label = if(!is.null(value)) value$predicate_label
#'       else "[Predicate]",
#'     predicate.uri = if(!is.null(value)) value$predicate.uri
#'       else character(),
#'     object.label = if(!is.null(value)) value$object.label
#'       else "[Object]",
#'     object.uri = if(!is.null(value)) value$object.uri
#'       else character()
#'   )
#'
#'   # Display ontology access ====
#'   onclick(paste0("subject_", id), {
#'     local.rv <- ontoloGUI("subject", local.rv)
#'   })
#'   onclick(paste0("predicate_", id), {
#'     local.rv <- ontoloGUI("predicate", local.rv)
#'   })
#'   onclick(paste0("object_", id), {
#'     local.rv <- ontoloGUI("object", local.rv)
#'   })
#'
#'   # Metadata save ----
#'   observe({
#'     req(
#'       !is.null(value) ||
#'         (any(grepl(rmv_id, names(input))) &&
#'             input[[rmv_id]] < 1)
#'     )
#'     personnel <- isolate(rv$Personnel)
#'     # Fetch correct index
#'     ind <- if (ref %in% personnel$id) {
#'       match(ref, personnel$id) # find its index
#'     }
#'     else {
#'       dim(personnel)[1] + 1
#'     }
#'
#'     # print values into rv at selected index
#'     localValues <- printReactiveValues(local.rv)
#'     localValues <- localValues[colnames(personnel)]
#'     localValues[which(!sapply(localValues, isTruthy))] <- ""
#'     isolate(rv$Personnel[ind,] <- localValues)
#'   })
#'
#'   # Remove UI====
#'   if(is.null(role))
#'     onclick(rmv_id, {
#'       # unload the RV
#'       ind <- match(ref, rv$Personnel$id)
#'       rv$Personnel <- rv$Personnel %>% slice(-ind)
#'
#'       # remove the UI
#'       removeUI(selector = paste0("#", site_id), immediate = TRUE)
#'     })
#'
#'   # Output ----
#'   return(rv)
#' }
