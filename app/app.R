library(shiny)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(googlesheets4)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:0px;margin-right:auto;"
  )
}

## header  ----------------------------------------------------------------------------

header <- dashboardHeader(disable = TRUE)

## sidebar  ----------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  # Input: Select file format ----
  selectInput("fileformat", "When data extraction/analysis will be finished, we will implement a filtering system here so that users can filter cohorts based on many caracteristics (N, diagnosis tool, follow up length, etc...)",
              choices = c("criteria1" = "criteria1",
                          "criteria2" = "criteria2"),
              selected = NA)
  # downloadButton("dwnld", "Download data", icon = icon("fas fa-download"), class = "button-test")
)

## Main panel  ----------------------------------------------------------------------------
body <- dashboardBody(
  # changing some elements of style -----------
  tags$head(tags$style(HTML('
    .content-wrapper, .right-side {
       background-color: #fff;
    }
    .skin-blue .main-sidebar {
      background-color: #52667F;
    }
        .button-test{
    margin-top: 2rem !important;
    font-size: 1.3rem;
    
    background-color: #fff;
    display: inline-block;
    text-decoration: none;
    color: #000 !important;
    border: double 4px #2F496C;
    border-radius: 5px;
    transition: .2s;
}

    .button-test:hover{
    border: double 4px #2F496C;
    opacity: 0.6

    }
    .button-test:active {
    background-color: #fff !important;
}
    .button-test:focus {
    background-color: #fff;
    display: inline-block;
    padding: 0.5em 1em;
    text-decoration: none;
    color: #000 !important;
    border: double 4px #2F496C;
    border-radius: 5px;
    transition: .4s;
    }

    '))),
  # main output panel ------------
  tabsetPanel(id = "results",
              type = "tabs",
              tabPanel("Summary", 
                       br(),
                       fluidRow(column(12, infoBoxOutput("includedBox"))), 
                       fluidRow(column(12, infoBoxOutput("discussionBox"))),
                       fluidRow(column(12, infoBoxOutput("difficultBox"))),
                       fluidRow(column(12, infoBoxOutput("excludedBox")))
              ),
              tabPanel("Included cohorts", 
                       downloadButton("dwnldINC", "Download data", icon = icon("fas fa-download"), class = "button-test", width = 2),
                       dataTableOutput("included") %>% withSpinner(color = "#2f496c", type = 8)),
              tabPanel("Cohorts in discussion", 
                       downloadButton("dwnldDISC", "Download data", icon = icon("fas fa-download"), class = "button-test"),
                       dataTableOutput("discussion") %>% withSpinner(color = "#2f496c", type = 8)),
              tabPanel("Cohorts difficult to reach", 
                       downloadButton("dwnldDIFF", "Download data", icon = icon("fas fa-download"), class = "button-test"),
                       dataTableOutput("difficult") %>% withSpinner(color = "#2f496c", type = 8)),
              tabPanel("Excluded cohorts", 
                       downloadButton("dwnldEXCL", "Download data", icon = icon("fas fa-download"), class = "button-test"),
                       dataTableOutput("excluded") %>% withSpinner(color = "#2f496c", type = 8))
  ))



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
#############################Loading files #############################
  # style = "max-height: 50vh; overflow-y: auto;"
  gs4_deauth()
  dataINC <- read_sheet("https://docs.google.com/spreadsheets/d/1LqOLWh63a66NMTVqzRoollmPLZQdSbRs_-JfQOFBV38/edit#gid=0")
  dataEXCL <- read_sheet("https://docs.google.com/spreadsheets/d/1tR6_5i2oMmazmh78euXZ16P9YBXIcN1rLoCcDtWieiM/edit#gid=0")
  dataDISC <- read_sheet("https://docs.google.com/spreadsheets/d/1RkbXqIFUmk9hzWt279XNqveiK8Sn2wLmOadJI9canR0/edit#gid=0") 
  dataDIFF <- read_sheet("https://docs.google.com/spreadsheets/d/1EsSshK5iuICVFG-dIT0KHdgXTkZBBAiC8z6iq2x0AIc/edit#gid=0") 
  
  output$includedBox <- renderInfoBox({
    infoBox("Included", paste0(nrow(dataINC)), icon = icon("thumbs-up", lib = "font-awesome"), color = "olive", fill = TRUE)
  })
  output$discussionBox <- renderInfoBox({
    infoBox("In discussion", paste0(nrow(dataDISC)), icon = icon("envelope-open-text", lib = "font-awesome"), color = "light-blue", fill = TRUE)
  })
  output$difficultBox <- renderInfoBox({
    infoBox("Difficult to reach", paste0(nrow(dataDIFF)), icon = icon("exclamation-triangle", lib = "font-awesome"), color = "orange", fill = TRUE)
  })
  output$excludedBox <- renderInfoBox({
    infoBox("Decline", paste0(nrow(dataEXCL)), icon = icon("thumbs-down", lib = "font-awesome"), color = "red", fill = TRUE)
  })
  output$dwnldINC <- downloadHandler(
    filename = function() { paste('SIMBA-INC-cohorts-', Sys.Date(), '.csv', sep='')  },
    content = function(con) { write.csv(dataINC, con) })
  
  output$dwnldDISC <- downloadHandler(
    filename = function() { paste('SIMBA-DISC-cohorts-', Sys.Date(), '.csv', sep='') },
    content = function(con) { write.csv(dataDISC, con) })
  
  output$dwnldDIFF <- downloadHandler(
    filename = function() { paste('SIMBA-DIFF-cohorts-', Sys.Date(), '.csv', sep='') },
    content = function(con) { write.csv(dataDIFF, con) })
  
  output$dwnldEXCL <- downloadHandler(
    filename = function() { paste('SIMBA-EXC-cohorts-', Sys.Date(), '.csv', sep='') },
    content = function(con) { write.csv(dataEXCL, con) } )
  
  ## INCLUDED ---------------------------------------------------------------------------------------------------------------------------------------------------
  output$included <- renderDT({
    if (is.null(dataINC)) {
      df_null = data.frame(matrix(ncol = 1, nrow = 0))
      return(datatable(df_null, colnames = "",
                       options = list(
                         dom = 't')))
    } else {
      headerCallback <- c("function(thead, data, start, end, display){", "  $('th', thead).css('border-top', '1px solid black');", "}")
      DT::datatable(dataINC,
        caption = htmltools::tags$caption(
        'If you are listed in this database, you can modify the content of this table here: https://docs.google.com/spreadsheets/d/1LqOLWh63a66NMTVqzRoollmPLZQdSbRs_-JfQOFBV38/edit#gid=0.'),
        class = 'cell-border stripe',
        rownames = FALSE,
        extensions = 'Buttons',
        fillContainer = TRUE,
        options = list(
          headerCallback = JS(headerCallback),
          scrollX = TRUE,
          scrollY = TRUE,
          dom = c('ft'),
          order = list(1, 'asc'),
          autoWidth = TRUE,
          pageLength = 100,
          columnDefs = list(
            list(width = '150px',
                 targets = "_all"),
            list(className = 'dt-center',
                 targets = "_all"))
          )
        )
    }
  })
  ## IN DISCUSSION ---------------------------------------------------------------------------------------------------------------------------------------------------
  output$discussion <- renderDT({
    if (is.null(dataDISC)) {
      df_null = data.frame(matrix(ncol = 1, nrow = 0))
      return(datatable(df_null, colnames = "",
                       options = list(
                         dom = 't')))
    } else {
      headerCallback <- c("function(thead, data, start, end, display){", "  $('th', thead).css('border-top', '1px solid black');", "}")
      DT::datatable(dataDISC,
                    # caption = htmltools::tags$caption(
                    #   ''),
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    extensions = 'Buttons',
                    fillContainer = TRUE,
                    options = list(
                      headerCallback = JS(headerCallback),
                      scrollX = TRUE,
                      scrollY = TRUE,
                      dom = c('ft'),
                      order = list(1, 'asc'),
                      autoWidth = TRUE,
                      pageLength = 100,
                      columnDefs = list(
                        list(className = 'dt-center',
                             targets = "_all"))
                    )
      )
    }
  })
  ## DIFFICULT ---------------------------------------------------------------------------------------------------------------------------------------------------
  output$difficult <- renderDT({
    if (is.null(dataDIFF)) {
      df_null = data.frame(matrix(ncol = 1, nrow = 0))
      return(datatable(df_null, colnames = "",
                       options = list(
                         dom = 't')))
    } else {
      headerCallback <- c("function(thead, data, start, end, display){", "  $('th', thead).css('border-top', '1px solid black');", "}")
      DT::datatable(dataDIFF,
                    caption = htmltools::tags$caption(
                      'If you have access to the data of any of these cohorts and you would like to participate in this project, please contact us at "corentin.gosling@parisnanterre.fr"'),
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    extensions = 'Buttons',
                    fillContainer = TRUE,
                    options = list(
                      headerCallback = JS(headerCallback),
                      scrollX = TRUE,
                      scrollY = TRUE,
                      dom = c('ft'),
                      order = list(1, 'asc'),
                      autoWidth = TRUE,
                      pageLength = 100,
                      columnDefs = list(
                        list(className = 'dt-center',
                             targets = "_all"))
                    )
      )
    }
  })
  ## EXCLUDED ---------------------------------------------------------------------------------------------------------------------------------------------------
  output$excluded <- renderDT({
    if (is.null(dataEXCL)) {
      df_null = data.frame(matrix(ncol = 1, nrow = 0))
      return(datatable(df_null, colnames = "",
                       options = list(
                         dom = 't')))
    } else {
      headerCallback <- c("function(thead, data, start, end, display){", "  $('th', thead).css('border-top', '1px solid black');", "}")
      DT::datatable(dataEXCL,
                    caption = htmltools::tags$caption(
                      'If you have access to the data of any of these cohorts and you would like to participate in this project, please contact us at "corentin.gosling@parisnanterre.fr"'),
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    extensions = 'Buttons',
                    fillContainer = TRUE,
                    options = list(
                      headerCallback = JS(headerCallback),
                      scrollX = TRUE,
                      scrollY = TRUE,
                      dom = c('ft'),
                      order = list(1, 'asc'),
                      autoWidth = TRUE,
                      pageLength = 100,
                      columnDefs = list(
                        list(className = 'dt-center',
                             targets = "_all"))
                    )
      )
    }
  })
  
}

shinyApp(ui = ui, server = server)
