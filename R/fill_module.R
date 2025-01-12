#' @import shiny
#'
#' @noRd
fillUI <- function(id) {
  tabsetPanel(
    id = NS(id, "tabs"),
    tabPanel(
      tags$h4("EML Assembly Line"),
      fluidPage(
        style = "padding-top:2.5%;",
        # * Top row ----
        tags$span(
          fluidRow(
            div(
              HTML(
                '<svg style="height: 50px;width: 100%;/*! bottom: -35px; */position: absolute;">
                  <line y1="0" y2="0" style="stroke:rgb(149, 149, 149);stroke-width:3" x2="400" x1="-1000"></line>
                  <line y1="0" y2="50" style="stroke:rgb(149, 149, 149);stroke-width:3" x2="450" x1="400"></line>
                  <line y1="50" x2="2000" y2="50" style="stroke:rgb(149, 149, 149);stroke-width:3" x1="450"></line>
                </svg>'
              ),
              h3(
                textOutput(NS(id, "current_step"))
              ),
              style = "float: left"
            ),
            div(
              shinyWidgets::actionBttn(
                NS(id, "help"), 
                "Help",
                icon("question-circle"),
                style = "simple",
                color = "primary"
              ),
              # actionButton(
              #   NS(id, "help"), 
              #   "Help", 
              #   icon("question-circle"),
              #   style = "background-color: #6cb5e1; color: #fff"
              # ),
              shinyWidgets::actionBttn(
                NS(id, "save"),
                "Save",
                icon("save"),
                style = "simple",
                color = "success"
              ),
              # actionButton(
              #   NS(id, "save"),
              #   "Save",
              #   icon("save"),
              #   style = "background-color: #a2d98e;"
              # ), # fill-wizard-save
              shinyWidgets::actionBttn(
                NS(id, "quit"), 
                "Quit",
                icon("times-circle"),
                style = "simple",
                color = "danger"
              ),
              # actionButton(
              #   NS(id, "quit"), 
              #   "Quit",
              #   icon("times-circle"),
              #   class = "danger"
              # ), # fill-wizard-quit
              style = "float: right;"
            ), # fill-wizard-help
            style = "width: 100%"
          ),
          # uiOutput(NS(id, "chain")),
          style = "display: inline-flex; width: 100%"
        ),
        hr(),
        # * Pages ----
        pagesUI(
          NS(id, "wizard"),
          parent.id = id
        )
      ) # end fluidPage
    ),
    tabPanel(
      tags$h4("MetaFIN"),
      MetaFINUI(
        NS(id, "metafin"),
        wip = base::get("metashark.args", envir = .GlobalEnv)$wip
      )
    )
  )
}

#' @import shiny
#' @importFrom shinyjs onclick show hide 
#'
#' @noRd
fill <- function(id, main.env) {
  moduleServer(id, function(input, output, session) {
    if (main.env$dev) {
      shinyjs::onclick(
        "dev",
        {
          if (main.env$current.tab() == "fill"
              && main.env$EAL$page != 3) {
            browser()
          }
        },
        asis = TRUE
      )
    }
    
    # Variable initialization ====
    steps <- isolate(main.env$VALUES$steps)
    
    # Wizard  ====
    # pages change
    pagesServer("wizard", main.env)
    
    # modules content
    sapply(seq_along(isolate(main.env$VALUES$steps)), function(i){
      .id <- isolate(main.env$VALUES$steps)[i]
      .to.call <- switch(i, "SelectDP", "DataFiles", "Attributes",
                         "CatVars", "GeoCov", "TaxCov", "Personnel", "Misc", "MakeEML")
      do.call(
        what = .to.call,
        args = list(
          id =  .id,
          main.env = main.env
        )
      )
    })
    
    # * Quit ----
    {
      # show modal.state 'quit' button clicked
      observeEvent(input$quit, {
        req(input$quit)
        showModal(
          modalDialog(
            title = "You are leaving data description.",
            "Are you sure to leave? Some of your metadata have maybe not been saved.",
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                NS(id, "save_quit"),
                "Save & Quit"
              ),
              actionButton(
                NS(id, "simple_quit"),
                "Quit",
                icon("times-circle"),
                class = "redButton"
              )
            )
          ) # end of modalDialog
        )
      },
      label = "EAL quit?"
      )
      
      # calls save method and quits
      observeEvent(input$save_quit, {
        req(input$save_quit)
        
        # Save work at this state
        saveReactive(main.env, main.env$EAL$page, do.template = FALSE)
        
        # Clean & reset variables
        removeModal()
        main.env <- cleanModules(main.env)
      },
      label = "EAL save+quit",
      ignoreInit = TRUE
      )
      
      # quits simply
      observeEvent(input$simple_quit, {
        req(input$quit)
        
        # Clean & reset variables
        removeModal()
        main.env <- cleanModules(main.env)
      },
      label = "EAL quit",
      ignoreInit = TRUE
      )
    } # quit management
    
    # * Save ----
    observeEvent(input$save, {
      if(main.env$dev) devmsg("saved: %s", main.env$EAL$page)
      saveReactive(main.env, main.env$EAL$page, do.template = FALSE)
    })
    
    output$current_step <- renderText({
      req(main.env$EAL$current)
      
      gsub(
        "_",
        " ",
        switch(
          main.env$EAL$current,
          SelectDP = "Select Data Package",
          main.env$EAL$current
        )
      )
    })
    
    # Navigation ====
    observeEvent(main.env$EAL$page, {
      req(main.env$EAL$page != main.env$EAL$old.page)
      
      withProgress(
        {
          # * Save  & Template ----
          devmsg(tag="fill_module.R", "save & template\r")
          if(main.env$EAL$old.page > 1)
            saveReactive(
              main.env, 
              main.env$EAL$old.page, 
              # do not template on 'previous'
              do.template = main.env$EAL$old.page < main.env$EAL$page
            )
          incProgress(1/7)
          
          # * set EAL variables ----
          devmsg(tag="fill_module.R", "set EAL variables\r")
          # left Data Files
          if (main.env$EAL$old.page == 2) 
            unlink(main.env$PATHS$eal.tmp)
          # reached Data Files
          if (main.env$EAL$page == 2)
            main.env$PATHS$eal.tmp <- tempdir()
          
          # Properly change page
          main.env$EAL$current <- main.env$VALUES$steps[main.env$EAL$page]
          main.env$EAL$tag.list <- tagList()
          incProgress(1/7)
          
          # * Reset local.rv ----
          devmsg(tag="fill_module.R", "set local rv\r")
          # browser()
          main.env <- setLocalRV(main.env)
          incProgress(1/7)
          
          # * Change page ----
          devmsg(tag="fill_module.R", "change pane\r")
          updateTabsetPanel(session, "wizard-wizard", selected = steps[main.env$EAL$page])
          incProgress(1/7)
          
          # * Update history ----
          if (!main.env$EAL$current %in% main.env$EAL$history) {
            main.env$EAL$history <- c(main.env$EAL$history, main.env$EAL$current)
          }
          devmsg(tag="fill_module.R", "update history\r")
          incProgress(1/7)
          
          # * Savevar changes ----
          devmsg(tag="fill_module.R", "save variables change\r")
          main.env$save.variable$step <- main.env$EAL$page # save current location
          main.env$save.variable$history <- main.env$EAL$history # erase old save
          
          # * Accessory UI elements ----
          devmsg(tag="fill_module.R", "display UI\r")
          if(main.env$EAL$page > 1) {
            shinyjs::show("help")
            shinyjs::show("save")
            shinyjs::show("quit")
          } else {
            shinyjs::hide("help")
            shinyjs::hide("save")
            shinyjs::hide("quit")
          }
          incProgress(1/7)
          
          devmsg(tag="fill_module.R", "ended\r")
          
          # * Helps ====
          {
            main.env$EAL$help <- modalDialog(
              title = paste0(main.env$EAL$current, " - Help"),
              switch(
                main.env$EAL$page,
                # ** SelectDP ====
                tagList(
                  tags$p("This module allows you to manage your", tags$strong("data packages"), ".
                    A data package (aka DP) is a collection of a dataset and its associated metadata
                    and resources (scripts, literature, ...). You can:"),
                  tags$ul(
                    tags$li("Load an existing data package among local ones (left). You
                      will resume its edition at the last saved encountered step."),
                    tags$li(
                      "Create a new data package. You must fill in", tags$strong("three fields"),
                      tags$i("Data package name"), "will be used for identifying the DP,",
                      tags$i("Data package title"), "will be displayed when consulting the DP
                        page once formatted (explained in last step),",
                      tags$i("Data package license"), "is the license assigned to this production for
                        intellectual rights properties"
                    )
                  ),
                  tags$p("Also, notice the", tags$strong("quick"), "check box above DP name.
                    Checking this box enables \"quick mode\" which will pre-fill most of the 
                    fields in further steps. You still will be able to edit them at your 
                    convenience.")
                ),
                # ** Data Files ====
                tagList(
                  tags$p("This module allows you to load data files from the dataset you want to
            describe. Once uploaded, you can set:"),
                  tags$ul(
                    tags$li(tags$i("Content name:"), "A name for the data contained in the file (e.g. table name)."),
                    tags$li(tags$i("URL:"), "If the file is accessible remotely, here is a way to reference it."),
                    tags$li(tags$i("Description:"), "A short text provided to describe the file, its content,
              relevant information about the data in the entity.")
                  ),
                  tags$p("To edit your selection, select files among the list with the check boxes on
            their left, then click the \"Remove\" button."),
                  tags$p("Recommended size per file is around 1 Gb. Such files and heavier ones might slow down the
            app.")
                ),
                # ** Attributes  ====
                tagList(
                  tags$p("This module allows you to describe precisely each attribute of each file. Some of these metadata
            are guessed from the data files. Such fields are annoted with a star (*).
            For each attribute, you can set:"),
                  tags$ul(
                    tags$li(tags$i("Attribute Name*:"), "the name of the attribute."),
                    tags$li(tags$i("Attribute Description:"), "a "),
                    tags$li(tags$i("Attribute Class*:"), "the type of content in the attributes among
              \"numeric\", \"character\", \"categorical\" and \"Date\". Categorical means a 
              character string with encoded values (e.g. Male/Female)."),
                    tags$li(tags$i("Date format (only for Date class): how the dates of the attributes are
              written (e.g. DD-MM-YYYY).")),
                    tags$li(tags$i("Unit (only for Numeric class):"), "which is the unit used for the 
              numeric data of the attributes. The list is huge and refers to", tags$a("STMML Scientific
              units", href = "http://www.ch.ic.ac.uk/rzepa/codata2/"), ". You can also define you own unit
              (see Custom Units thereafter)."),
                    tags$li(tags$i("Missing Value Code:"), "a one-word code used instead of a missing value among 
              the data of the attribute currently described."),
                    tags$li(tags$i("Missing Value Code Explanation:"), "a short definition of the meaning(s) given
              for the missing value code.")
                  ),
                  tags$h3("Custom units creation"),
                  tags$p("EML allows the user to define its own units, but this require to fulfill some more
            fields. However, the custom units you will have defined will be saved into the Custom Units
            table at the bottom of this page. You will find the written custom units in the units selection
            once they are written. To define a custom unit, chose the unit to be \"custom\". A custom 
            unit is defined with:"),
                  tags$ul(
                    tags$li(tags$i("Unit id:"), "the id of the unit (e.g. gramsPerOneThirdMeter). The unit id 
              must validate the STMML schema."),
                    tags$li(tags$i("Unit type:"), "the physical property measured by the custom unit (e.g. mass)."),
                    tags$li(tags$i("Parent unit in SI:"), "from which unit among the most common one is the custom 
              unit derived (e.g. gram)."),
                    tags$li(tags$i("Multiplier to SI:"), "by how many has the custom unit to be multiplied to be 
              equal to its parent unit."),
                    tags$li(tags$i("Unit description:"), "some additional notes about the unit, how to compute it.")
                  )
                ),
                # ** Catvars ====
                tagList(
                  tags$p("This module allows you to detail the categorical variables (class \"categorical\" in Attributes).
            For each variable, you will be able to detail each of its value by a short description.")
                ),
                # ** Geocov ====
                tagList(
                  tags$p("This module allows you to define the geographic area in which the data have been produced. 
            You have the choice between two methods to define geographic coverage:"),
                  tags$ul(
                    tags$li(
                      tags$h4("Variable selection (recommended)"),
                      tags$p("This method allows you to fetch interesting attributes for
                        geographic coverage. Prefer storing all of your locations into a 
                        single table or equally dimensioned tables, under 3 to 5 columns:
                        one for the site description and one or two others for latitude 
                        and longitude. For latitude and longitude, chosing a single 
                        column will be interpreted as points, while chosing two will
                        result in an area. Southern latitude and western longitude
                        shall be noted with negative values.")
                    ),
                    tags$li(
                      tags$h4("Manual geographic coverage"),
                      tags$p("Chose this method if your dataset does not include data sites
                        description and/or coordinates. You will be able to provide as many 
                        sites as you wish, as single points or rectangle areas.You can detail 
                        a more precise number by using the left/right (or down/up) arrows of
                        your keyboard. Precision can be given at 0.01", HTML("&#176"), ".")
                    )
                  )
                ),
                # ** Taxcov ====
                tagList(
                  tags$p("This module allows you to define the taxonomical coverage of the study. You will be asked to 
            select columns among your files containing the species name. Also, let the app know if the taxonomic
            coverage shall be written with scientific, common or both names. At last, select at least one taxonomic
            authority among the ones suggested."),
                ),
                # ** Personnel ====
                tagList(
                  tags$p("This module allows you to get a full list of the people who contributed to the creation of 
            this dataset. The recommended best practice is to", tags$b("use the ORCID"), "of a person. With 
            this and the help of {rorcid}, the app will be able to fetch all available and interesting data. 
            You will be able to correct this afterwards. Also, note that you must fill in the fields
            for two roles: creator and contact."),
                  tags$p("Suggested roles are the following:"),
                  tags$ul(
                    tags$li("Creator: person who contributed to produce the data."),
                    tags$li("Contact: persone to contact for any question about the data."),
                    tags$li("Principal investigator: person who led the creation of the dataset. Selecting this will allow
                          you to fill in additional information about the project and its funding."),
                    tags$li("Custom: as the list of roles is not exhaustive, feel free to add any role you consider important.")
                  )
                ),
                # ** Misc ====
                tagList(
                  tags$p("This module allows you to define the last details of your data package. Note that you can write
            some of these metadata using the markdown syntax. Here are brief descriptions of the fields:"),
                  tags$ul(
                    tags$li("Abstract: the abstract of the publication linked to those data."),
                    tags$li("Methods: the methods used in the production of this dataset."),
                    tags$li(
                      "Keywords: you can type a list of keywords for your dataset. For each keyword, you can add a
              keyword thesaurus, like", tags$a("the LTER controlled vocabulary ", href = "http://vocab.lternet.edu/vocab/vocab/index.php"),
                      ", which are controlled vocabulary your exact keyword originates from. Keywords thesaurus are not
              required."
                    ),
                    tags$li("Temporal coverage: this lets you define the duration of the study during which the data have been produced."),
                    tags$li("Additional information: if any information has been omitted, you can provide it here (e.g. collection metadata
              from GBIF-EML).")
                  )
                ),
                # ** Make EML ====
                tagList(
                  tags$p("Here we are (well done) ! This is the final step to write EML. Just click the button and let the magic happen. If an 
            error occurs, this will be displayed to the screen. In this case, do not hesitate to get in touch with the dev team."),
                  tags$p("You can also use the {emldown} package to get a human-readable version of the generated EML. The button below it will
            let you download the result of this step.")
                )
              ),
              footer = modalButton("Close"),
              easyClose = TRUE, fade = TRUE
            )
          }
          incProgress(1/7)
        },
        message = sprintf("loading %s", main.env$VALUES$steps[main.env$EAL$page])
      )
    },
    label = "EAL0: change page"
    )
    
    observeEvent(input$help, {
      showModal(main.env$EAL$help)
    })
    # (End) ====
  })
}
