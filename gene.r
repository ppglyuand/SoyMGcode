library(shiny)
library(DT)
library(biomaRt)

geneSearchUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Genes Search"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("speciesDatabase"), "Select Species Database:",
                    choices = c("Soybean (Williams 82)" = "gmax_eg_gene", 
                                "Glycine soja (Wild soybean)" = "gsoja_eg_gene"),
                    selected = "gsoja_eg_gene"),
        textAreaInput(ns("geneList"), "Enter Gene IDs (one per line):", rows = 10, value = ""),
        actionButton(ns("runSearch"), "Run Search"),
        actionButton(ns("loadExample"), "Load Example"),
        downloadButton(ns("downloadTable"), "Download Table"),
        
      ),
      mainPanel(
        DTOutput(ns("resultsTable"))
      )
    )
  )
}


geneSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$loadExample, {
      exampleGeneList <- switch(input$speciesDatabase,
                                "gmax_eg_gene" = "GLYMA_01G094200\nGLYMA_02G073100",
                                "gsoja_eg_gene" = "LOC114406335")
      updateTextAreaInput(session, "geneList", value = exampleGeneList)
    })
    
    genesInfo <- eventReactive(input$runSearch, {
      showModal(modalDialog(
        title = "Please wait",
        "Loading, please wait...",
        easyClose = FALSE,
        footer = NULL
      ))
      
      on.exit(removeModal(), add = TRUE)
      
      req(input$runSearch)
      geneList <- unlist(strsplit(input$geneList, "\n"))
      geneList <- geneList[geneList != ""]
      
      mart <- useMart(biomart = "plants_mart", dataset = input$speciesDatabase, host = "https://plants.ensembl.org")
      
      getBM(attributes = c('ensembl_gene_id', 'go_id', 'external_gene_name', 'gene_biotype', 'description', 
                            'chromosome_name', 'start_position', 'end_position', 
                            'strand'),
            filters = 'ensembl_gene_id',
            values = geneList,
            mart = mart)
    })
    
    output$resultsTable <- renderDT({
      req(genesInfo())
      datatable(genesInfo(), options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
    })
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("genes-info-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(genesInfo(), file, row.names = FALSE)
      }
    )
  })
}

ui <- geneSearchUI("genesearch")

server <- function(input, output, session) {
  geneSearchServer("genesearch")
}

shinyApp(ui, server)

