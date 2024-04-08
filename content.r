library(shinydashboard)
library(shiny)
library(readxl)
library(DT)
library(plotly)
library(ggplot2)
library(qqman)
library(LDheatmap)
library(genetics)
library(gprofiler2)

# 定义保存文件的目录，确保这个目录下已经有了一些示例的Excel文件
saveDir <- "www/excel_uploads"

# 确保保存文件的目录存在
if (!dir.exists(saveDir)) {
  dir.create(saveDir, recursive = TRUE)
}
source("E:/R/gene.r", local = FALSE)
source("E:/R/kegg.r", local = FALSE)
ui <- dashboardPage(
  dashboardHeader(title = "SoyMG-MFP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Management", tabName = "data_management"),
      menuItem("Pie Chart", tabName = "pie_chart"),
      menuItem("Bar Chart", tabName = "bar_chart"),
      menuItem("Gene Search", tabName = "gene_search"), # 添加 Gene Search 菜单项
      menuItem("GWAS Plot", tabName = "gwas_plot"),
      menuItem("Allele Boxplot", tabName = "boxplot"),
      menuItem("LD Plot", tabName = "ld_plot"),
      menuItem("KEGG Enrichment Analysis", tabName = "kegg_enrichment"), 
      menuItem("GO Relationship Query", tabName = "go_relationship"),
      menuItem("eFP-demo", tabName = "efp_demo"),
      menuItem("PE", tabName = "pe"),
       menuItem("Download", tabName = "download")
    )
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName = "gene_search",
              geneSearchUI("geneSearchModule") # 使用geneSearch模块的UI
      ),
    tabItem(tabName = "efp_demo",
              titlePanel("eFP-demo"),
              HTML('<iframe src="https://filed77e2f5b4a33.v4.h5sys.cn/play/Go3tlOYb" style="border: none; width:100%; height:600px"></iframe>')
      ),
      tabItem(tabName = "pe",
  titlePanel("PE"),
  HTML('<a href="https://filed77e2f5b4a33.v4.h5sys.cn/play/Yku5WiCV" target="_blank">PE Link here:version-2024.4.7</a>
       <p>In the download menu, open-source code is provided that includes drawing eFP and PE visualizations\nmaking it convenient for users to draw the desired visualizations themselves. Of course, you can also submit a link like the one above, and I will add it for you.</p>')
),

      tabItem(tabName = "kegg_enrichment",
              keggUI("keggModule") # 在这里调用 keggUI
      ),
      tabItem(tabName = "data_management",
              h2("Data Management"),
              tabsetPanel(
                tabPanel("Create Folder",
                         h2("Create New Folder"),
                         textInput("new_folder_name", "New Folder Name"),
                         actionButton("create_folder_btn", "Create Folder")
                ),
                tabPanel("Upload Excel file",
                         h2("Upload Excel File"),
                         selectInput("folder_select", "Select Folder", choices = list.dirs(saveDir, full.names = FALSE, recursive = FALSE)),
                         fileInput('file', 'Select Excel or CSV file', accept = c(".xlsx", ".csv")),
                         actionButton("upload", "Upload")
                ),
                tabPanel("Manage Table",
                         h2("Manage Table"),
                         fluidRow(
                           column(
                             4, 
                             selectInput("select_folder", "Select Folder", choices = list.dirs(saveDir, full.names = FALSE, recursive = FALSE)),
                             actionButton("delete_folder", "Delete Folder", class = "btn-danger")
                           ),
                           column(
                             4,
                             selectInput("select_file", "Select File", choices = NULL),
                             actionButton("delete_file", "Delete File", class = "btn-default")
                           ),
                           column(
                             4,
                             downloadButton("download_data", "Download Data")  # 添加下载按钮
                           )
                         ),
                         DTOutput("excel_table")
                )
              )
      ),
      tabItem(tabName = "gwas_plot",
              titlePanel("GWAS Data Plot"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput("select_folder_gwas"), # 动态生成的选择文件夹UI
                  uiOutput("select_existing_table_gwas"), # 动态生成的选择表格UI
                  fileInput("data_file", "Upload data file"),
                  actionButton("load_button", "Load data"),
                  checkboxInput("header_checkbox", "Header", value = TRUE),
                  # GWAS Plot: 使用文本输入框让用户输入红线位置
                  textInput("redline_input", "Threshold Line Position", value = "1")

                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Manhattan Plot", plotlyOutput("manhattan_plot")),
                    tabPanel("QQ Plot", plotOutput("qq_plot"))
                  )
                )
              )
      ),
      tabItem(tabName = "boxplot",
              titlePanel("Allele Boxplot"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput("select_existing_table_boxplot"),  # Dynamic UI for selecting existing tables
                  fileInput("data_file_boxplot", "Upload data file"),
                  actionButton("load_button_boxplot", "Load data"),
                  checkboxInput("header_checkbox_boxplot", "Header", value = TRUE),
                  textInput("boxplot_title", "Boxplot Title", placeholder = "Enter Boxplot Title")
                ),
                mainPanel(
                  plotlyOutput("boxplot")
                )
              )
      ),
     tabItem(tabName = "download",
      titlePanel("Download Resources"),
      fluidRow(
        column(width = 12,
          tags$h3("Note:"),
          tags$p("Here are some materials to assist in using this tool. Click the link to start downloading. More resources can be found in the Github link on the Help page\nIn addition, the tool can also load some genomes of model plants such as Arabidopsis, but unfortunately, due to space limitations, it cannot be downloaded here."),
          tags$ul(
            tags$li(tags$a(href="https://soymgdata-1253971857.cos.ap-nanjing.myqcloud.com/%E5%AF%8C%E9%9B%86%E5%88%86%E6%9E%90%E5%A4%A7%E8%B1%86%E7%9A%84%E5%9F%BA%E5%9B%A0.txt", "Integration of genes that can be inputted into the KEGG module", target = "_blank")),
            tags$li(tags$a(href="https://soymgdata-1253971857.cos.ap-nanjing.myqcloud.com/KEGG%E5%A4%A7%E8%B1%86%E7%9B%B8%E5%85%B3%E7%9A%84%E6%95%B0%E6%8D%AE%E6%94%B6%E9%9B%86%EF%BC%88%E5%8C%85%E6%8B%AC%E6%97%A0%E9%80%9A%E8%B7%AF%E7%9A%84EF%BC%89.txt", "Information related to KEGG soybeans collected", target = "_blank")),
            tags$li(tags$a(href="https://soymgdata-1253971857.cos.ap-nanjing.myqcloud.com/ncbi_dataset.tsv", "Glycine soja gene（from NCBI）", target = "_blank")),
            tags$li(tags$a(href="https://soymgdata-1253971857.cos.ap-nanjing.myqcloud.com/eFP%E4%BB%A3%E7%A0%81.txt", "eFP code", target = "_blank")),
            tags$li(tags$a(href="https://soymgdata-1253971857.cos.ap-nanjing.myqcloud.com/PE%E4%BB%A3%E7%A0%81.txt", "PE code", target = "_blank"))
          )
        )
      )
    ),
     

    tabItem(tabName = "go_relationship", # 添加 GO Relationship Query 页面
           div(id = "loading_message", style = "display: none;",
          h2("Loading, please wait..."),
          tags$i(class = "fa fa-spinner fa-spin fa-3x")
        ),
              titlePanel("GO Relationship Query"),
              sidebarLayout(
                sidebarPanel(
                  fileInput("fileInput", "Upload GO List File:", 
                            accept = c(
                              "text/csv", 
                              "text/comma-separated-values", 
                              "text/plain", 
                              ".csv", 
                              ".txt"
                            )),
                  textAreaInput("geneList", "Or Enter GO IDs (one per line):", rows = 5),
                  actionButton("runAnalysis", "Run Analysis"),
                  actionButton("example", "Example") # 添加示例按钮
                ),
                mainPanel(
                  div(style = "overflow-x: auto;", DTOutput("resultsTable")), # 添加水平滚动条
           
                )
              )
      ),

      tabItem(tabName = "pie_chart",
              titlePanel("Pie Chart"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput("select_column_pie_chart"),
                  uiOutput("select_file_pie_chart"),
                  actionButton("update_pie_chart", "Update Pie Chart"),
                  helpText("Data comes from the 'Manage Table' selected table. If you need to switch data, please select a different table.")
                ),
                mainPanel(
                  plotlyOutput("pie_chart_output")
                )
              )
      ),
      tabItem(tabName = "bar_chart",
              titlePanel("Bar Chart"),
              sidebarLayout(
                sidebarPanel(
                  uiOutput("select_column_bar_chart"),
                  uiOutput("select_file_bar_chart"),
                  actionButton("update_bar_chart", "Update Bar Chart"),
                  helpText("Data comes from the 'Manage Table' selected table. If you need to switch data, please select a different table.")
                ),
                mainPanel(
                  plotlyOutput("bar_chart_output")
                )
              )
      ),
 
      tabItem(tabName = "ld_plot",
              titlePanel("Linkage Disequilibrium Block Plot"),
              sidebarLayout(
                sidebarPanel(
                  fileInput("genotypeFile", "GenotypeFile(上传SNP标记基因型信息文件)"),
                  fileInput("positionFile", "PositionFile(上传SNP标记位置信息文件)"),
                  textAreaInput("genotypeText", "GenotypeText(粘贴SNP标记基因型信息)", rows = 10),
                  textAreaInput("positionText", "PositionText(粘贴SNP标记位置信息)", rows = 10),
                  actionButton("generateExampleGenotype", "Generate Example Genotype"), # 生成示例基因型信息按钮
                  br(), # 添加一个换行
                  br(), # 添加另一个换行
                  actionButton("generateExamplePosition", "Generate Example Position") # 生成示例位置信息按钮
                ),
                mainPanel(
                  plotOutput("ldPlot", height = "500px")
                )
              )
      )
    )
  )
)


# 服务器逻辑
server <- function(input, output, session) {
    # 确保gene.r中的代码可以访问server环境中的变量
    #source("E:/R/gene.r", local = TRUE)
  # 处理示例按钮点击事件
  geneSearchServer("geneSearchModule")
 keggServer("keggModule")
  observeEvent(input$example, {
    # 将文本输入框的值设置为"GO:0005253"
    updateTextAreaInput(session, "geneList", value = "GO:0005253")
  })
  
observeEvent(input$runAnalysis, {
  # 显示加载提示
  shinyjs::show("loading_message")
  
  # 初始化基因列表
  geneList <- character(0)
  
  # 如果文件被上传
  if (!is.null(input$fileInput)) {
    geneList <- read.csv(input$fileInput$datapath, header = FALSE, stringsAsFactors = FALSE)[,1]
  }
  
  # 如果文本输入不为空
  if (input$geneList != "") {
    geneList <- c(geneList, unlist(strsplit(input$geneList, "\n", TRUE)))
  }
  
  geneList <- geneList[geneList != ""] # 移除空行
  
  # 执行GO富集分析, 针对大豆
  result <- gost(query = geneList, organism = "gmax", sources = c("GO"), significant = FALSE)
  
  # 处理和展示结果
  if (is.data.frame(result$result)) {
    # 提取并计算所需的列
    resultData <- result$result
    resultData$p.adjust <- p.adjust(resultData$p_value, method = "BH")
    
    # 显示结果表格
    output$resultsTable <- renderDT({
      datatable(resultData, options = list(pageLength = 10, autoWidth = TRUE))
    }, server = FALSE) # 使用server = FALSE以处理大型数据集
  } else {
    # 如果没有结果，显示空的数据表
    output$resultsTable <- renderDT({
      datatable(data.frame(), options = list(pageLength = 10, autoWidth = TRUE))
    }, server = FALSE)
  }
  
  # 隐藏加载提示
  shinyjs::hide("loading_message")
})
  


  # Data Management: Upload file logic
  observeEvent(input$upload, {
    req(input$file)
    target_folder <- input$folder_select
    file_path <- file.path(saveDir, target_folder, input$file$name)
    file.rename(input$file$datapath, file_path)
    updateSelectInput(session, "select_file", choices = list.files(file.path(saveDir, target_folder)))
  })
  
  # Data Management: Update file choices when a folder is selected
  observe({
    req(input$select_folder)
    updateSelectInput(session, "select_file", choices = list.files(file.path(saveDir, input$select_folder)))
  })
  
 # 修改Data Management的表格渲染逻辑来同时支持xlsx和csv文件
output$excel_table <- renderDT({
  req(input$select_folder, input$select_file)
  file_path <- file.path(saveDir, input$select_folder, input$select_file)
  if (grepl("\\.csv$", file_path)) {
    excel_data <- read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    excel_data <- read_excel(file_path, sheet = 1)
  }
  datatable(excel_data, filter = 'top', options = list(autoWidth = TRUE, scrollX = TRUE))
})

  # Data Management: 删除文件逻辑
  observeEvent(input$delete_file, {
    req(input$select_folder, input$select_file)
    file_path <- file.path(saveDir, input$select_folder, input$select_file)
    if(file.exists(file_path)) {
      showModal(modalDialog(
        title = "Confirm Deletion",
        "Are you sure you want to delete this file?",
        modalButton("Cancel"),
        actionButton("confirm_delete_file", "Delete", class = "btn-danger")
      ))
    }
  })

  # Data Management: 确认删除文件
  observeEvent(input$confirm_delete_file, {
    removeModal()
    file_path <- file.path(saveDir, input$select_folder, input$select_file)
    if(file.exists(file_path)) {
      file.remove(file_path)
      updateSelectInput(session, "select_file", choices = list.files(file.path(saveDir, input$select_folder)))
    }
  })

  # GWAS Plot: Dynamic UI for selecting existing tables
  output$select_existing_table <- renderUI({
    req(list.dirs(saveDir, full.names = FALSE, recursive = FALSE))
    selectInput("existing_table", "Select Existing Table", choices = c("Upload New File", list.files(saveDir, full.names = FALSE, recursive = TRUE)))
  })

  # Boxplot: Dynamic UI for selecting existing tables for Boxplot
  output$select_existing_table_boxplot <- renderUI({
    req(list.dirs(saveDir, full.names = FALSE, recursive = FALSE))
    selectInput("existing_table_boxplot", "Select Existing Table for Boxplot", choices = c("Upload New File", list.files(saveDir, full.names = FALSE, recursive = TRUE)))
  })

  # GWAS Plot: Load data from selected existing table or uploaded file
  data <- reactive({
    req(input$existing_table)
    if (input$existing_table == "Upload New File") {
      if (!is.null(input$paste_data) && input$paste_data != "") {
        df <- read.table(text = input$paste_data, header = input$header_checkbox, sep = "")
      } else {
        req(input$data_file)
        df <- read.table(input$data_file$datapath, header = input$header_checkbox, sep = "\t")
      }
    } else {
      file_path <- file.path(saveDir, input$existing_table)
      df <- read_excel(file_path, sheet = 1)
    }
    df
  })

  # Boxplot: Load data from selected existing table or uploaded file for Boxplot
  data_boxplot <- reactive({
    req(input$existing_table_boxplot)
    if (input$existing_table_boxplot == "Upload New File") {
      if (!is.null(input$paste_data_boxplot) && input$paste_data_boxplot != "") {
        df <- read.table(text = input$paste_data_boxplot, header = input$header_checkbox_boxplot, sep = "")
      } else {
        req(input$data_file_boxplot)
        df <- read.table(input$data_file_boxplot$datapath, header = input$header_checkbox_boxplot, sep = "\t")
      }
    } else {
      file_path <- file.path(saveDir, input$existing_table_boxplot)
      df <- read_excel(file_path, sheet = 1)
    }
    df
  })

# GWAS Plot: Render Manhattan plot
output$manhattan_plot <- renderPlotly({
  req(data())
  # 假设 data() 数据框中有 CHROM, POS, P 三列
  p <- ggplot(data(), aes(x = as.factor(CHROM), y = -log10(P), color = as.factor(CHROM), text = paste("CHROM:", CHROM, "<br>POS:", POS, "<br>P-value:", P))) +
    geom_point(position = position_jitter(width = 0.2)) +
    scale_color_manual(values = rainbow(length(unique(data()$CHROM)))) +
    labs(x = "Chromosome", y = "-log10(P-value)", color = "Chromosome") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = as.numeric(input$redline_input), color = "red")  # 使用输入的值作为红线位置
  ggplotly(p, tooltip = "text")  # 使用 tooltip 参数指定显示 text 列的信息
})



  # GWAS Plot: Render QQ plot
  output$qq_plot <- renderPlot({
    req(data())
    qq(data()$P, main = "QQ Plot", xlim = c(0, 5))
    abline(a = 0, b = 1, col = "red")
  })

  # Boxplot: Render Boxplot
  output$boxplot <- renderPlotly({
    req(data_boxplot())
    p <- ggplot(data_boxplot(), aes_string(x = "Allele", y = colnames(data_boxplot())[2], fill = "Allele")) +
      geom_boxplot() +
      geom_jitter(position = position_jitter(width = 0.1)) +
      theme_minimal() +
      labs(x = "Allele", y = colnames(data_boxplot())[2], title = input$boxplot_title) +
      geom_text(data = head(data_boxplot(), 1), aes(x = as.numeric(factor(Allele)), y = max(data_boxplot()[[2]]) + 0.1 * (max(data_boxplot()[[2]]) - min(data_boxplot()[[2]])), 
                                                   label = paste("P-value:", round(t.test(data_boxplot()[[2]] ~ data_boxplot()$Allele)$p.value, 2), 
                                                                 "Statistic:", round(t.test(data_boxplot()[[2]] ~ data_boxplot()$Allele)$statistic, 2),
                                                                 "Sample Size:", length(data_boxplot()[[2]]))),
                vjust = -1, hjust = 0.5, size = 3, inherit.aes = FALSE, check_overlap = TRUE)
    ggplotly(p)
  })
  
  # Pie Chart: Dynamic UI for selecting column and file
  output$select_column_pie_chart <- renderUI({
    req(input$select_folder, input$select_file)
    file_path <- file.path(saveDir, input$select_folder, input$select_file)
    excel_data <- read_excel(file_path, sheet = 1)
    selectInput("column_pie_chart", "Select Column", choices = names(excel_data))
  })
  
  
  
  # Pie Chart: Render Pie Chart
  output$pie_chart_output <- renderPlotly({
    req(input$select_folder, input$select_file, input$column_pie_chart)
    file_path <- file.path(saveDir, input$select_folder, input$select_file)
    excel_data <- read_excel(file_path, sheet = 1)
    
    column_data <- table(excel_data[[input$column_pie_chart]])
    plot_ly(labels = names(column_data), values = as.numeric(column_data), type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial')
  })
  
  # Bar Chart: Dynamic UI for selecting column and file
  output$select_column_bar_chart <- renderUI({
    req(input$select_folder, input$select_file)
    file_path <- file.path(saveDir, input$select_folder, input$select_file)
    excel_data <- read_excel(file_path, sheet = 1)
    selectInput("column_bar_chart", "Select Column", choices = names(excel_data))
  })
  

  # Bar Chart: Render Bar Chart
  output$bar_chart_output <- renderPlotly({
    req(input$select_folder, input$select_file, input$column_bar_chart)
    file_path <- file.path(saveDir, input$select_folder, input$select_file)
    excel_data <- read_excel(file_path, sheet = 1)
    
    count_data <- as.data.frame(table(excel_data[[input$column_bar_chart]]))
    names(count_data) <- c('Category', 'Count')

    plot_ly(data = count_data, x = ~Category, y = ~Count, type = 'bar', marker = list(color = 'lightskyblue')) %>%
      layout(title = 'Bar Chart', xaxis = list(title = input$column_bar_chart), yaxis = list(title = 'Count'))
  })

  # 响应生成示例基因型信息按钮的点击事件
  observeEvent(input$generateExampleGenotype, {
    example_genotype <- "SNP1\tSNP2\tSNP3\tSNP4\tSNP5\n
    T/C\tG/G\tC/C\tG/G\tG/G\n
    T/T\tG/C\tC/C\tG/G\tG/G\n
    C/C\tC/C\tT/T\tC/C\tA/A\n
    T/T\tG/G\tC/C\tG/G\tG/G\n
    T/C\tC/C\tT/T\tC/C\tG/G"
    updateTextAreaInput(session, "genotypeText", value = example_genotype)
  })

  # 响应生成示例位置信息按钮的点击事件
  observeEvent(input$generateExamplePosition, {
    example_position <- "26276207\n26276350\n26276420\n26276650\n26276720"
    updateTextAreaInput(session, "positionText", value = example_position)
  })

  # LD Plot: 处理上传文件或文本输入的基因型信息
  SNPdata <- reactive({
    if(!is.null(input$genotypeFile)) {
      read.table(input$genotypeFile$datapath, header = TRUE, sep = "\t")
    } else if(nchar(input$genotypeText) > 0) { # 确保文本框有内容
      read.table(text = input$genotypeText, header = TRUE, sep = "\t")
    }
  })

  # LD Plot: 处理上传文件或文本输入的位置信息
  SNPpos <- reactive({
    if(!is.null(input$positionFile)) {
      read.table(input$positionFile$datapath, header = FALSE, sep = "\t")
    } else if(nchar(input$positionText) > 0) { # 确保文本框有内容
      read.table(text = input$positionText, header = FALSE, sep = "\t")
    }
  })

  # LD Plot: 绘制连锁不平衡图
  output$ldPlot <- renderPlot({
    req(SNPdata())
    req(SNPpos())
    SNPdata_df <- SNPdata()
    SNPpos_df <- SNPpos()
    
    num <- ncol(SNPdata_df)
    for(i in 1:num) {
      SNPdata_df[, i] <- as.genotype(SNPdata_df[, i])
    }
    
    pos <- as.vector(unlist(SNPpos_df))
    
    color.rgb <- colorRampPalette(rev(c("white", "red")), space = "rgb")
    LDheatmap(SNPdata_df, pos, color = color.rgb(20), flip = TRUE)
  })
}

# 运行应用程序
shinyApp(ui, server)  

