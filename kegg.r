library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(clusterProfiler)

# KEGG Enrichment UI部分
keggUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("KEGG Enrichment Analysis"),
    sidebarLayout(
      sidebarPanel(
        textAreaInput(ns("geneList"), "Enter Gene IDs (one per line):"),
        fileInput(ns("fileUpload"), "Or upload file with Gene IDs:", accept = ".txt"),
        actionButton(ns("runAnalysis"), "Run Analysis", style = "color: white; background-color: #007BFF;"),
        hr(),
        actionButton(ns("exampleBtn"), "Load Example", style = "color: white; background-color: #007BFF;"),
        selectInput(ns("organismSelect"), "Select Organism:",
                    choices = c("Soybean (gmx)", "Arabidopsis thaliana (ath)", "General (ko)")),
        hr(),
        downloadButton(ns("downloadResults"), "Download Results")
      ),
      mainPanel(
        DTOutput(ns("resultsTable")),
        plotlyOutput(ns("barPlot")),
        plotlyOutput(ns("bubblePlot"))
      )
    )
  )
}


# KEGG Enrichment Server部分
# KEGG Enrichment Server部分
keggServer <- function(id) {
  moduleServer(id, function(input, output, session) {    
    observeEvent(input$exampleBtn, {
      updateTextAreaInput(session, "geneList", value = "100037449\n548099\n100305785\n100527186")
    })

    enrichedResults <- eventReactive(input$runAnalysis, {
      geneList <- if (!is.null(input$fileUpload)) {
        readLines(input$fileUpload$datapath)
      } else {
        unlist(strsplit(input$geneList, "\n"))
      }

      organismCode <- switch(input$organismSelect,
                             "Soybean (gmx)" = "gmx",
                             "Arabidopsis thaliana (ath)" = "ath",
                             "General (ko)" = "ko")

      kk <- enrichKEGG(gene = geneList, organism = organismCode, keyType = "kegg",
                       pAdjustMethod = "BH", qvalueCutoff = 0.05)

      if (!is.null(kk) && "Description" %in% colnames(kk@result)) {
        return(kk@result)
      } else {
        showNotification("No enrichment results found.", type = "warning")
        return(NULL)
      }
    })

    output$resultsTable <- renderDT({
      df <- enrichedResults()
      if (is.null(df)) return(data.frame())
      df$Entry <- sapply(df$ID, function(id) sprintf('<a href="https://www.genome.jp/entry/%s">%s</a>', id, id))
      datatable(df, escape = FALSE, options = list(autoWidth = TRUE, pageLength = 10, scrollX = TRUE))
    })

    output$downloadResults <- downloadHandler(
      filename = function() {
        paste("enrichment-results-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(enrichedResults(), file, row.names = FALSE)
      }
    )

    output$barPlot <- renderPlotly({
      df <- enrichedResults()
      if (is.null(df)) return()
      customText <- paste("Description:", df$Description, "<br>Count:", df$Count, "<br>GeneRatio:", df$GeneRatio, "<br>pvalue:", df$pvalue, "<br>p.adjust:", df$p.adjust, "<br>qvalue:", df$qvalue)
      p <- ggplot(df, aes(x = reorder(Description, -qvalue), y = Count, fill = Description, text = customText)) +
        geom_bar(stat = "identity") + coord_flip() +
        theme_minimal() + labs(x = "", y = "Count")
      ggplotly(p, tooltip = "text")
    })

    output$bubblePlot <- renderPlotly({
      df <- enrichedResults()
      if (is.null(df)) return()
      customText <- paste("Description:", df$Description, "<br>GeneRatio:", df$GeneRatio, "<br>pvalue:", df$pvalue, "<br>p.adjust:", df$p.adjust, "<br>qvalue:", df$qvalue)
      p <- ggplot(df, aes(x = GeneRatio, y = Description, size = pvalue, color = -log10(p.adjust), text = customText)) +
        geom_point() + scale_size() + scale_color_gradient(low = "blue", high = "red") +
        theme_minimal() + labs(x = "Gene Ratio", y = "")
      ggplotly(p, tooltip = "text")
    })
  })
}
