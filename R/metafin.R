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
      actionButton(ns("dev"), "DEV")
    ),
    column(
      8,
      uiOutput(ns("form"))
    )
  )
}

MetaFIN <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    
    # File input ====
    
    path <- reactive({
      req(input$file)
      input$file$datapath
    })
    
    # Resources ====
    
    # * read eml file ----
    EML <- reactive({
      req(path())
      
      return(XML::xmlParse(path()))
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
    
    # * make eml as list ----
    eml <- reactive({
      req(eml.root())
      
      .out <- XML::xmlToList(eml.root())
      .out <- renameList(.out)
      
      return(.out)
    })
    
    # * isolate root node ----
    # used to rewrite EML on save
    eml.back <- reactive({
      req(eml.root())
      
      return(XML::xmlClone(eml.root()) %>% XML::removeChildren("dataset"))
    })
    
    # * make eml as shiny tree ----
    eml.shinyTree <- reactive({
      validate(
        need(eml(), "no eml() provided")
      )
      
      return(listToStructure(eml()))
    })
    
    # * render tree ----
    output$tree <- renderTree(eml.shinyTree())
    
    
    # * dev ----
    observeEvent(input$dev, {
      browser()
    })
    
    # Use tree ====
    content <- eventReactive(input$tree, {
      req(isContentTruthy(get_selected(input$tree)))
      
      .path <- paste(
        attr(get_selected(input$tree)[[1]], "ancestry"),
        # get_selected(input$tree)[[1]][1],
        collapse = "/"
      )
      # content -- tricks in followPath: same path, different tree
      .content <- followPath(eml(), .path)[[get_selected(input$tree)[[1]][1]]]
      
      return(.content)
    })
    
    output$form <- renderUI({
      validate(
        need(isTruthy(content()), "Please select a node"),
        need(depth(content(), mode = "min") <= 1, "Please select a sub node")
      )
      
      if(depth(content()) == 0){
        content_name <- get_selected(input$tree)[[1]][1]
        
        if(is.numeric(content()))
          numericInput(content_name, content_name, value = content())
        else
          textInput(content_name, content_name, value = content())
      }
      else if(depth(content()) > 0) {
        tagList(
          lapply(names(content()), function(content_name) {
            subcontent <- content()[[content_name]]
            
            if(depth(subcontent) > 0)
              actionLink(content_name, content_name)
            else {
              if(is.numeric(subcontent))
                numericInput(content_name, content_name, value = subcontent)
              else
                textInput(content_name, content_name, value = subcontent)
            }
          })
        )
      }
    })
    
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