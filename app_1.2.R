# SPDX-License-Identifier: GPL-2.0-or-later

# Load required libraries
library(shiny)
library(readxl)
library(dplyr)
library(tibble)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(ggbiplot)
library(viridis)
library(shinythemes)
library(ggrepel)

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Flexible HCPC & PCA Biplot Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Input"),
      fileInput("datafile", "Upload Excel File", accept = c(".xlsx", ".xls")),
      
      uiOutput("sheetSelector"),
      uiOutput("groupColumns"),
      uiOutput("analysisColumns"),
      
      tags$hr(),
      h4("Analysis Parameters"),
      numericInput("nCluster", "Number of Clusters", value = 3, min = 2),
      numericInput("pcX", "PC for X axis", value = 1, min = 1),
      numericInput("pcY", "PC for Y axis", value = 2, min = 1),
      actionButton("run", "Run Analysis"),
      
      tags$hr(),
      checkboxInput("showDownloads", "Show download options", value = FALSE),
      uiOutput("downloadControls")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Plots",
                 fluidRow(
                   column(6, plotOutput("eigPlot")),
                   column(6, plotOutput("dendroPlot"))
                 ),
                 fluidRow(
                   column(6, plotOutput("clusterPlot"))
                 )
        ),
        tabPanel("Biplot",
                 fluidRow(
                   column(4,
                          checkboxInput("filterLoadings", "Filter important loadings", value = FALSE),
                          conditionalPanel(
                            condition = "input.filterLoadings == true",
                            numericInput("nLoadings", "Number of loadings to show", value = 5, min = 1)
                          )
                   )
                 ),
                 fluidRow(
                   column(12, plotOutput("biplot"))
                 )
        ),
        tabPanel("Variable Extraction",
                 h4("Quantitative Description per Cluster (raw, unscaled values)"),
                 tabsetPanel(
                   tabPanel("Cluster 1",
                            tableOutput("vars1"),
                            downloadButton("downloadVars1", "Download Cluster 1 Variables")
                   ),
                   tabPanel("Cluster 2",
                            tableOutput("vars2"),
                            downloadButton("downloadVars2", "Download Cluster 2 Variables")
                   ),
                   tabPanel("Cluster 3",
                            tableOutput("vars3"),
                            downloadButton("downloadVars3", "Download Cluster 3 Variables")
                   )
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamic UI for selecting sheet, group and analysis columns
  output$sheetSelector <- renderUI({
    req(input$datafile)
    sheets <- excel_sheets(input$datafile$datapath)
    selectInput("sheet", "Select Sheet", choices = sheets)
  })
  
  output$groupColumns <- renderUI({
    req(input$datafile, input$sheet)
    df <- read_excel(input$datafile$datapath, sheet = input$sheet)
    selectInput("groupCols", "Select Grouping Columns",
                choices = names(df), multiple = TRUE,
                selected = head(names(df), 2))
  })
  
  output$analysisColumns <- renderUI({
    req(input$datafile, input$sheet)
    df <- read_excel(input$datafile$datapath, sheet = input$sheet)
    numericChoices <- names(df)[sapply(df, is.numeric)]
    if(length(numericChoices) == 0) numericChoices <- names(df)
    selectInput("analysisCols", "Select Analysis Columns (Numeric)",
                choices = numericChoices, multiple = TRUE,
                selected = numericChoices)
  })
  
  # Reactive: grouped raw data
  rawGroupedData <- reactive({
    req(input$datafile, input$sheet, input$groupCols, input$analysisCols)
    df <- read_excel(input$datafile$datapath, sheet = input$sheet)
    df %>%
      group_by(across(all_of(input$groupCols))) %>%
      summarise(across(all_of(input$analysisCols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      mutate(Sample = do.call(paste0, .[input$groupCols])) %>%
      column_to_rownames("Sample")
  })
  
  # Reactive: processed (scaled) data for PCA/HCPC
  processedData <- eventReactive(input$run, {
    dfg <- rawGroupedData()
    mat <- dfg[, input$analysisCols, drop = FALSE]
    list(
      mat_scaled = scale(mat),
      sampleNames = rownames(dfg)
    )
  })
  
  # Reactive: PCA + HCPC results
  analysisResults <- eventReactive(input$run, {
    dl <- processedData()
    ncp <- max(input$pcX, input$pcY)
    pca <- PCA(dl$mat_scaled, scale.unit = TRUE, ncp = ncp, graph = FALSE)
    hcpc <- HCPC(pca, nb.clust = input$nCluster, graph = FALSE)
    list(pca = pca, hcpc = hcpc)
  })
  
  # Scree plot
  output$eigPlot <- renderPlot({
    req(analysisResults())
    fviz_eig(analysisResults()$pca, addlabels = TRUE,
             barfill = "steelblue", barcolor = "steelblue") +
      ggtitle("Scree Plot")
  })
  
  # Dendrogram
  output$dendroPlot <- renderPlot({
    req(analysisResults())
    fviz_dend(analysisResults()$hcpc,
              cex = 0.7, palette = "jco", rect = TRUE,
              rect_fill = TRUE, rect_border = "jco",
              labels_track_height = 0.8)
  })
  
  # Cluster plot
  output$clusterPlot <- renderPlot({
    req(analysisResults())
    fviz_cluster(analysisResults()$hcpc, main = "Cluster Plot", repel = TRUE,
                 axes = c(input$pcX, input$pcY)) +
      scale_fill_manual(name = "Cluster",
                        values = c("#6699CC", "#CC6677", "#999933", "#44AA99")[1:input$nCluster]) +
      scale_color_manual(name = "Cluster",
                         values = c("#6699CC", "#CC6677", "#999933", "#44AA99")[1:input$nCluster])
  })
  
  # Biplot
  output$biplot <- renderPlot({
    req(analysisResults())
    pcaObj <- analysisResults()$pca
    hcpcObj <- analysisResults()$hcpc
    groups <- as.factor(hcpcObj$data.clust$clust)
    labs <- rownames(hcpcObj$data.clust)
    
    # Extract percent variance explained
    var_exp <- pcaObj$eig[, 2]
    pct_x <- round(var_exp[input$pcX], 1)
    pct_y <- round(var_exp[input$pcY], 1)
    
    p <- ggbiplot(
      pcaObj,
      choices = c(input$pcX, input$pcY),
      ellipse = TRUE,
      groups = groups,
      labels = labs,
      var.scale = 1,
      alpha = 0,
      varname.size = 0,
      var.axes = FALSE
    ) +
      scale_color_manual(name = "Cluster",
                         values = c("#6699CC", "#CC6677", "#999933")[1:input$nCluster]) +
      scale_fill_manual(name = "Cluster",
                        values = alpha(c("#6699CC", "#CC6677", "#999933")[1:input$nCluster], 0.1)) +
      theme_minimal() +
      ggtitle("PCA Biplot") +
      xlab(paste0("PC", input$pcX, " (", pct_x, "% )")) +
      ylab(paste0("PC", input$pcY, " (", pct_y, "% )"))
    
    loadings <- as.data.frame(
      pcaObj$var$coord[, c(input$pcX, input$pcY)]
    )
    colnames(loadings) <- c("DimX", "DimY")
    loadings$var <- rownames(loadings)
    loadings$importance <- sqrt(loadings$DimX^2 + loadings$DimY^2)
    if (input$filterLoadings) {
      loadings <- head(
        loadings[order(-loadings$importance), ],
        input$nLoadings
      )
    }
    
    p +
      geom_segment(data = loadings,
                   aes(x = 0, y = 0, xend = DimX, yend = DimY),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "gray40") +
      geom_text_repel(data = loadings,
                      aes(x = DimX, y = DimY, label = var),
                      color = "gray40", size = 4)
  })
  
  # Variable extraction (raw values)
  varsCluster <- reactive({
    req(input$analysisCols, input$nCluster)
    dfg <- rawGroupedData()
    mat <- dfg[, input$analysisCols, drop = FALSE]
    pcaRaw <- PCA(mat, scale.unit = FALSE, graph = FALSE)
    hcpcRaw <- HCPC(pcaRaw, nb.clust = input$nCluster, graph = FALSE)
    descVar <- hcpcRaw$desc.var$quanti
    lapply(c("1", "2", "3"), function(cl) {
      if (cl %in% names(descVar)) as.data.frame(unlist(descVar[[cl]]))
      else data.frame()
    }) -> lst
    names(lst) <- paste0("cluster", 1:3)
    lst
  })
  
  output$vars1 <- renderTable({ varsCluster()$cluster1 }, rownames = TRUE)
  output$vars2 <- renderTable({ varsCluster()$cluster2 }, rownames = TRUE)
  output$vars3 <- renderTable({ varsCluster()$cluster3 }, rownames = TRUE)
  
  # Download handlers for variable extraction
  output$downloadVars1 <- downloadHandler(
    filename = function() "vars_cluster1.txt",
    content = function(file) {
      write.table(varsCluster()$cluster1, file, sep = "\t", row.names = TRUE)
    }
  )
  output$downloadVars2 <- downloadHandler(
    filename = function() "vars_cluster2.txt",
    content = function(file) {
      write.table(varsCluster()$cluster2, file, sep = "\t", row.names = TRUE)  
    }
  )
  output$downloadVars3 <- downloadHandler(
    filename = function() "vars_cluster3.txt",
    content = function(file) {
      write.table(varsCluster()$cluster3, file, sep = "\t", row.names = TRUE)
    }
  )
  
  # Download handlers for plots
  output$downloadEig <- downloadHandler(
    filename = function() "scree_plot.svg",
    content = function(file) {
      req(analysisResults())
      svg(file, width = 10, height = 8)
      print(
        fviz_eig(analysisResults()$pca, addlabels = TRUE,
                 barfill = "steelblue", barcolor = "steelblue") +
          ggtitle("Scree Plot")
      )
      dev.off()
    }
  )
  
  output$downloadDendro <- downloadHandler(
    filename = function() "dendrogram.svg",
    content = function(file) {
      req(analysisResults())
      svg(file, width = 10, height = 8)
      print(
        fviz_dend(analysisResults()$hcpc,
                  cex = 0.7, palette = "jco", rect = TRUE,
                  rect_fill = TRUE, rect_border = "jco",
                  labels_track_height = 0.8)
      )
      dev.off()
    }
  )
  
  output$downloadCluster <- downloadHandler(
    filename = function() "cluster_plot.svg",
    content = function(file) {
      req(analysisResults())
      svg(file, width = 10, height = 8)
      print(
        fviz_cluster(analysisResults()$hcpc,
                     main = "Cluster Plot", repel = TRUE,
                     axes = c(input$pcX, input$pcY)) +
          scale_fill_manual(name = "Cluster",
                            values = c("#6699CC", "#CC6677", "#999933", "#44AA99")[1:input$nCluster]) +
          scale_color_manual(name = "Cluster",
                             values = c("#6699CC", "#CC6677", "#999933", "#44AA99")[1:input$nCluster])
      )
      dev.off()
    }
  )
  
  output$downloadBiplot <- downloadHandler(
    filename = function() "biplot.svg",
    content = function(file) {
      req(analysisResults())
      svg(file, width = 10, height = 8)
      # Rebuild and print biplot
      pcaObj <- analysisResults()$pca
      hcpcObj <- analysisResults()$hcpc
      groups <- as.factor(hcpcObj$data.clust$clust)
      labs <- rownames(hcpcObj$data.clust)
      
      var_exp <- pcaObj$eig[, 2]
      pct_x <- round(var_exp[input$pcX], 1)
      pct_y <- round(var_exp[input$pcY], 1)
      
      p <- ggbiplot(
        pcaObj,
        choices = c(input$pcX, input$pcY),
        ellipse = TRUE,
        groups = groups,
        labels = labs,
        var.scale = 1,
        alpha = 0,
        varname.size = 0,
        var.axes = FALSE
      ) +
        scale_color_manual(name = "Cluster",
                           values = c("#6699CC", "#CC6677", "#999933")[1:input$nCluster]) +
        scale_fill_manual(name = "Cluster",
                          values = alpha(c("#6699CC", "#CC6677", "#999933")[1:input$nCluster], 0.1)) +
        theme_minimal() +
        ggtitle("PCA Biplot") +
        xlab(paste0("PC", input$pcX, " (", pct_x, "% )")) +
        ylab(paste0("PC", input$pcY, " (", pct_y, "% )"))
      
      loadings <- as.data.frame(
        pcaObj$var$coord[, c(input$pcX, input$pcY)]
      )
      colnames(loadings) <- c("DimX", "DimY")
      loadings$var <- rownames(loadings)
      loadings$importance <- sqrt(loadings$DimX^2 + loadings$DimY^2)
      if (input$filterLoadings) {
        loadings <- head(
          loadings[order(-loadings$importance), ],
          input$nLoadings
        )
      }
      
      p <- p +
        geom_segment(data = loadings,
                     aes(x = 0, y = 0, xend = DimX, yend = DimY),
                     arrow = arrow(length = unit(0.2, "cm")),
                     color = "gray40") +
        geom_text_repel(data = loadings,
                        aes(x = DimX, y = DimY, label = var),
                        color = "gray40", size = 4)
      
      print(p)
      dev.off()
    }
  )
  
  # Render download controls once analysis is done
  output$downloadControls <- renderUI({
    req(input$showDownloads, analysisResults())
    tagList(
      h4("Download Plots (SVG)"),
      downloadButton("downloadEig", "Download Scree Plot"),
      downloadButton("downloadDendro", "Download Dendrogram"),
      downloadButton("downloadCluster", "Download Cluster Plot"),
      downloadButton("downloadBiplot", "Download Biplot")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
