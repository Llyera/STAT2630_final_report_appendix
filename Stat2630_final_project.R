install.packages(c("shiny", "ggplot2", "plotly", "wordcloud2", "dplyr", "tidyr", "bslib", "fmsb", "scales", "stringr"), dependencies = TRUE)

library(shiny)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(dplyr)
library(tidyr)
library(bslib)
library(fmsb)
library(scales)
library(stringr)

ui <- fluidPage(
  titlePanel("Procrastination Research Articles Analysis Dashboard"),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File (procrastination_master_analysis.csv)", accept = ".csv"),
      checkboxInput("header", "Has Header Row", TRUE),
      
      hr(),
      radioButtons(
        "viz_type",
        "Choose Visualization",
        choices = c(
          "Word Cloud (Titles + Content)" = "wordcloud",
          "Sentiment Distribution (Pie)" = "sentiment",
          "Positive vs Negative Count (Scatter)" = "scatter",
          "Procrastination Dimensions Radar" = "radar",   # ← 修改為呢個
          "Data Preview & Summary" = "preview"
        ),
        selected = "radar"
      ),
      
      conditionalPanel(
        condition = "input.viz_type == 'wordcloud'",
        sliderInput("num_words", "Number of Words in Word Cloud", min = 5, max = 60, value = 30, step = 1)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Main Visualization",
                 conditionalPanel(condition = "input.viz_type == 'wordcloud'",
                                  wordcloud2Output("wordcloud", height = "680px")),
                 
                 conditionalPanel(condition = "input.viz_type == 'sentiment'",
                                  plotlyOutput("sentiment_pie", height = "650px")),
                 
                 conditionalPanel(condition = "input.viz_type == 'scatter'",
                                  plotOutput("scatter_plot", height = "650px")),
                 
                 conditionalPanel(condition = "input.viz_type == 'radar'",
                                  plotOutput("radar_plot", height = "680px")),
                 
                 conditionalPanel(condition = "input.viz_type == 'preview'",
                                  tableOutput("preview"),
                                  verbatimTextOutput("summary"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data_reactive <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, stringsAsFactors = FALSE, check.names = FALSE)

    if (!"prediction" %in% names(df)) df$prediction <- 0
    if (!"pos_count" %in% names(df)) df$pos_count <- 0
    if (!"neg_count" %in% names(df)) df$neg_count <- 0
    if (!"label" %in% names(df)) df$label <- 0
    
    df
  })
  
  #Word Cloud
  output$wordcloud <- renderWordcloud2({
    req(data_reactive())
    df <- data_reactive()
    
    text_all <- paste(df$content_clean, df$title, collapse = " ")
    text_all <- tolower(text_all)
    text_all <- gsub("[^a-zA-Z\\s]", " ", text_all)
    words <- unlist(strsplit(text_all, "\\s+"))
    words <- words[nchar(words) > 3]
    
    stopwords <- c("the","and","of","to","in","a","is","that","it","for","on","with","as","was","were","are","be","this","by","at","from","or","an","not","have","has","had")
    words <- words[!words %in% stopwords]
    
    word_freq <- sort(table(words), decreasing = TRUE)
    word_df <- data.frame(word = names(word_freq), freq = as.numeric(word_freq))
    
    word_df <- head(word_df, input$num_words)
    word_df$freq <- log1p(word_df$freq) ^ 1.6
    
    wordcloud2(word_df, size = 0.9, color = "random-dark",
               backgroundColor = "#f8f9fa", shape = "circle", rotateRatio = 0)
  })
  
  #Sentiment Pie Chart
  output$sentiment_pie <- renderPlotly({
    req(data_reactive())
    df <- data_reactive()
    
    total_pos <- sum(df$pos_count, na.rm = TRUE)
    total_neg <- sum(df$neg_count, na.rm = TRUE)
    
    sentiment_data <- data.frame(
      Sentiment = c("Positive", "Negative"),
      Count = c(total_pos, total_neg)
    )
    
    plot_ly(sentiment_data, labels = ~Sentiment, values = ~Count, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c("#2e8b57", "#cd5c5c"))) %>%
      layout(title = "Overall Sentiment Distribution in Articles")
  })
  
  #Positive vs Negative Scatter
  output$scatter_plot <- renderPlot({
    req(data_reactive())
    df <- data_reactive()
    
    df$label_name <- ifelse(df$label == 1, "Negative View", "Positive View")
    
    ggplot(df, aes(x = pos_count, y = neg_count, color = label_name, size = pos_count + neg_count)) +
      geom_point(alpha = 0.75) +
      scale_color_manual(values = c("Positive View" = "#2e8b57", "Negative View" = "#cd5c5c")) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
      labs(title = "Positive vs Negative Word Count per Article",
           x = "Positive Word Count", y = "Negative Word Count") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "bottom")
  })

  output$radar_plot <- renderPlot({
    req(data_reactive())
    df <- data_reactive()
    
    dim_count <- table(factor(df$prediction, levels = 0:3))
    dim_prop <- as.numeric(dim_count) / sum(dim_count)   # 轉為比例
    
    #Radar Chart
    radar_data <- data.frame(
      Psychological = dim_prop[1],
      Environmental = dim_prop[2],
      Task_based    = dim_prop[3],
      Motivational  = dim_prop[4]
    )
    
    radar_final <- rbind(
      rep(1, 4), 
      rep(0, 4), 
      radar_data
    )
    
    par(mar = c(5, 5, 5, 5))
    radarchart(radar_final,
               axistype = 1,
               vlabels = c("Psychological", "Environmental", "Task-based", "Motivational"),
               vlcex = 1.25,
               axislabcol = "grey25",
               calcex = 1.0,
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               pcol = "#4a86e8",
               pfcol = scales::alpha("#4a86e8", 0.35),
               plwd = 4.5,
               cglcol = "grey75",
               cglty = 1,
               title = "Procrastination Dimensions Distribution")
    
    legend("topright", legend = "Proportion", col = "#4a86e8", lwd = 4, bty = "n")
  })
  
  # Data Preview
  output$preview <- renderTable({
    req(data_reactive())
    head(data_reactive(), 10)
  })
  
  output$summary <- renderPrint({
    req(data_reactive())
    cat("=== Dataset Summary ===\n")
    summary(data_reactive())
    cat("\n\nPrediction Distribution:\n")
    print(table(data_reactive()$prediction))
  })
}

shinyApp(ui = ui, server = server)

