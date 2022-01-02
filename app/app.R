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
              choices = c(".xlsx" = "criteria1",
                          ".csv" = "criteria2",
                          ".txt" = "criteria3"),
              selected = "xlsx"),
  downloadButton("dwnld", "Download data", icon = icon("fas fa-download"), class = "button-test")
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
    margin-left: 1.5rem !important;
    margin-top: 3rem !important;
    font-size: 1.3rem;
    
    background-color: #fff;
    display: inline-block;
    padding: 0.5em 1em;
    text-decoration: none;
    color: #000 !important;
    border: double 4px #2F496C;
    border-radius: 5px;
    transition: .4s;
}

    .button-test:hover{
    border: double 4px #2F496C;

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
              tabPanel("Included cohorts", dataTableOutput("summary") %>% withSpinner(color = "#2f496c", type = 8)),
              tabPanel("Excluded cohorts", verbatimTextOutput("txt"))
  ))



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
#############################Loading files #############################
  # style = "max-height: 50vh; overflow-y: auto;"
  output$txt <- renderText({paste0("This list will be uploaded when data collection will be finished.")})

  gs4_deauth()
  dataLoad <- reactive({ read_sheet("https://docs.google.com/spreadsheets/d/1LqOLWh63a66NMTVqzRoollmPLZQdSbRs_-JfQOFBV38/edit#gid=0") })

  output$dwnld <- downloadHandler(
    filename = function() {
      paste('SIMBA-cohorts-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dataLoad(), con)
    }
  )

  # 1. UMBRELLA + ADD.EVIDENCE ------------------------------------------------
  output$summary <- renderDT({
    if (is.null(dataLoad())) {
      df_null = data.frame(matrix(ncol = 1, nrow = 0))
      return(datatable(df_null, colnames = "",
                       options = list(
                         dom = 't')))
    } else {

      headerCallback <- c(
        "function(thead, data, start, end, display){",
        "  $('th', thead).css('border-top', '1px solid black');",
        "}"
      )

      DT::datatable(dataLoad(),
        caption = htmltools::tags$caption(
        'You can modify the content of this table here: https://docs.google.com/spreadsheets/d/1LqOLWh63a66NMTVqzRoollmPLZQdSbRs_-JfQOFBV38/edit#gid=0.'),
        class = 'cell-border stripe',
        # style = "bootstrap",
        rownames = FALSE,
        extensions = 'Buttons',
        # class = "display",
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

}

shinyApp(ui = ui, server = server)
