MetaFINUI <- function(id, main.env) {
  ns <- NS(id)
  
  tagList(
    tags$h3("MetaFIN"),
    fileInput(
      ns("file"),
      "Select a valid EML file (.xml)",
      accept = "application/xml",
      width = "50%"
    ),
    shinyjs::hidden(
      tags$div(
        id = ns("content_area"),
        actionButton(ns("dev"), "DEV"),
        actionButton(
          ns("validate"),
          "Validate",
          icon = icon("check")
        ),
        downloadButton(
          ns("save"),
          "Save",
          icon = icon("save"),
          width = "100%"
        ),
        tags$hr(),
        column(
          4,
          shinyTree(
            ns("tree"),
            search = TRUE,
            searchtime = 1000
          ),
          style="overflow-y: scroll;
                 max-height: 650px"
        ),
        column(
          8,
          tags$div(id = "inserthere_mf_form"),
          collapsibleUI(
            ns("attributes"),
            "Attributes",
            tagList(
              tags$div(id = "inserthere_mf_attributes")
            )
          )
        )
      ) # end content_area
    )
  )
}

MetaFIN <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    
    local.rv <- reactiveValues(
      form = data.frame(
        id = character(),
        inputId = character(),
        path = character()
      ),
      count = 0
    )
    
    # File input ====
    
    xml.path <- reactive({
      req(input$file)
      input$file$datapath
    })
    
    observe({
      shinyjs::toggle("content_area", condition = isContentTruthy(xml.path()))
    })
    
    # Resources ====
    
    # * read eml file ----
    EML <- reactive({
      req(xml.path())
      
      return(XML::xmlParse(xml.path()))
    })
    
    # * save original eml ----
    EML.save <- reactive({
      req(EML())
      
      return(XML::xmlClone(EML()))
    })
    
    # * make root node ----
    eml.root <- reactive({
      req(EML())
      
      return(XML::xmlRoot(EML()))
    })
    
    # * make eml as list and RV ----
    eml.list <- list()
    makeReactiveBinding("eml.list")
    # eml.rv <- reactiveValues()

    observe({
      req(eml.root())

      .out <- XML::xmlToList(eml.root())
      eml.list <<- renameList(.out)

      # .out <- listToReactiveValues(eml.list)
      # eml.rv <<- .out
    })
    
    # * isolate root node ----
    # used to rewrite EML on save
    eml.back <- reactive({
      req(eml.root())
      
      return(XML::xmlClone(eml.root()) %>% XML::removeChildren("dataset"))
    })
    
    # * render tree ----
    eml.shinyTree <- reactive({
      validate(
        need(isContentTruthy(eml.list), "no eml.list provided")
      )
      
      listToStructure(eml.list)
    })
    
    output$tree <- renderTree(eml.shinyTree())
    
    # * dev ----
    observeEvent(input$dev, {
      browser()
    })
    
    # Use tree ====
    
    # * get content ----
    content <- eventReactive(input$tree, {
      req(isContentTruthy(get_selected(input$tree)))
      
      .path <- paste(
        c(
          attr(get_selected(input$tree)[[1]], "ancestry"),
          get_selected(input$tree)[[1]][1]
        ),
        collapse = "/",
        sep = "/"
      )
      # content -- tricks in followPath: same path, different tree
      .content <- followPath(eml.list, .path)
      
      return(.content)
    })
    
    # * output form ----
    torender.ui <- reactive({
      names(content())[sapply(content(), depth) == 0]
    })
    torender.links <- reactive({
      names(content())[sapply(content(), depth) > 0]
    })
    
    # ** insert forms ----
    inserted.uis <- c()
    observeEvent(torender.ui(), {
      message("triggered")
      # Remove previous UIs
      if(isContentTruthy(inserted.uis)){
        browser()
        sapply(inserted.uis, removeUI, immediate = TRUE)
      }
      
      # Insert UI
      lapply(
        torender.ui(), 
        function(content_name) {
          subcontent <- content()[[content_name]]
          local.rv$count <<- local.rv$count+1
          .path <- attr(get_selected(input$tree)[[1]], "ancestry")
          contentID <- paste(content_name, local.rv$count, collapse = "_")
          
          # Save inserted UI's ID
          inserted.uis <<- c(inserted.uis, contentID)
          # Set tag
          new.ui <- if(is.numeric(subcontent))
            numericInput(
              session$ns(contentID), content_name, value = subcontent
            ) else
              textInput(
                session$ns(contentID), content_name, value = subcontent
              )
          # insert tag
          insertUI(
            selector = "#inserthere_mf_form",
            ui = new.ui,
            immediate = TRUE
          )
          # add observer for eml item
          observeEvent(input[[contentID]], {
            req(isTruthy(input[[contentID]]))
            
            .path <- c(
              attr(get_selected(input$tree)[[1]], "ancestry"),
              get_selected(input$tree)[[1]][1],
              content_name
            )
            .value <- input[[contentID]]
            eml.list <- assignPath(eml.list, .path, .value)
          })
          
        }
      )
    })
    
    # ** browse buttons ----
    output$links <- renderUI({
      if(length(torender.links()) > 0) {
        shinyWidgets::checkboxGroupButtons(
          session$ns("links"),
          "Other items",
          torender.links,
          individual = TRUE
        )
      } else 
        NULL
    })
    
    observeEvent(input$links, {
      req(isTruthy(input$links))
      
      shinyWidgets::updateCheckboxGroupInput(
        session,
        "links",
        selected = NULL
      )
      browser()
      # shinyTree::updateTree(
      #   session,
      #   "tree",
      #   
      # )
    })
    
    # * xml attributes  ----
    collapsible("attributes")
    
    # Edit
    # * Select a node
    # * Update UI
    # ** edit content
    # ** edit attributes
    # ** remove children
    # ** add children
    # Save / quit
    # * turns back into XML
    
  })
}