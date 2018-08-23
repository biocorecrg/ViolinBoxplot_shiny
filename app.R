### Shiny app for building Boxplot / Violin plots
# Author: Sarah Bonnin - sarah.bonnin@crg.eu

library(shiny)
library(ggplot2)
library(colourpicker)
library(shinyjs)
library(shinyWidgets) # for downloadBttn
library(DT)
library(rhandsontable) # for interactive table
library(reshape2)

# Define UI for application that draws a Boxplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Box / violin plots"),
  
  # Sidebar with options
  sidebarLayout(
    sidebarPanel(
      # input file: accepts text and csv files
      fileInput("file_boxplot", "Upload text file", accept = c("text/txt", "text/csv")),
      # Choose whether to plot a box or a violon plot (default is a box plot)
      radioButtons("boxORviolin", label = h4("Type of plot:"),
                   choices = list("Box plot" = 1, "Violin plot" = 2), 
                   selected = 1, inline = TRUE),
      # Grouping column
      uiOutput("boxplot_char"),
      # Grouping action button
      uiOutput("boxplot_action_split"),
      # Plot title
      textInput("boxplot_title", label = h4("Plot title"), value = "Boxplot"),
      # Sliders for title, x-axis and y-axis labels sizes.
      sliderInput("boxplot_size_title",
                  "Title font size:",
                  min = 12, max = 60,
                  value = 12, step=2),
      sliderInput("boxplot_size_xlab",
                  "x-axis font size:",
                  min = 10, max = 40,
                  value = 10, step=2),
      sliderInput("boxplot_size_ylab",
                  "y-axis font size:",
                  min = 10, max = 40,
                  value = 10, step=2),
      sliderInput("boxplot_size_textx",
                  "x-axis labels font size:",
                  min = 10, max = 40,
                  value = 10, step=2)
      # Slider for x-axis limits: pending
    )
    ,
    # Show a plot of the generated distribution
    mainPanel(
      # Action button that will trigger the plot to be drawn
      actionBttn("doBoxplot", "Display Box/violin plot", icon("bar-chart-o"), style = "jelly", size = "sm"),
      # Plot Boxplot
      plotOutput("boxplotPlot"),
      # Conditional panel: if the user clicked on "display Boxplot plot", the download button appears
      # ! Working properly only on Google Chrome!!
      conditionalPanel(
        condition = "input.doBoxplot == true",
        downloadBttn("downloadBoxplot", "Save Boxplot plot", size = "sm", style = "jelly"),
        # Table with samples to plot appears: the user can modify the samples labels and groups, and unclick samples
        rHandsontableOutput("hot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  input_boxplot <- reactive({
    inFile <- input$file_boxplot
    if (is.null(inFile))
      return(NULL)
    boxplot1 <- read.table(inFile$datapath, sep="\t", header=T, as.is = T)
    # select numeric columns only
    dplyr::select_if(boxplot1, is.numeric)
  })
  # Retrieve non-numeric columns to put as options for the grouping
  observe({
     if(!is.null(input$file_boxplot)){
       inFile <- input$file_boxplot
    #  if (is.null(inFile))
    #     return(NULL)
      boxplot1 <- read.table(inFile$datapath, sep="\t", header=T, as.is = T)

      output$boxplot_char <- renderUI({
        columns_boxplot_char <- colnames(dplyr::select_if(boxplot1, is.character))
        selectInput(inputId="boxplot_group_col", "Split boxplot by:", 
                    choices=columns_boxplot_char,
                    selected=input$boxplot_group_col)
      })
      output$boxplot_action_split <- renderUI({
        # actionButton("boxplot_split_groups", "Split!")
        actionBttn("boxplot_split_groups", "Split!")
        
      })
      
     }
  })

  
  shinyjs::enable("doBoxplot")
  shinyjs::enable("boxplot_split_groups")
  
  
  #  values <- reactiveValues()
  
  observeEvent(input$doBoxplot, {
    values = reactiveValues()
    
    data <- reactive({
      if (!is.null(input$hot)) {
        DF <- hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]])){
          #DF = select_dat()
          DF1 <- colnames(input_boxplot())
          DF <- data.frame(Select=TRUE, Samples=DF1, Groups=DF1, stringsAsFactors = FALSE)
          
        }else{
          DF <- values[["DF"]]
        }
      }
      values[["DF"]] <- DF
      DF
    })
    
    output$hot <- renderRHandsontable({
      DF <- data()
      #      DF1 <- colnames(input_boxplot())
      #      DF <- data.frame(Select=TRUE, Samples=DF1, Groups=DF1, stringsAsFactors = FALSE)
      
      if (!is.null(DF))
        rhandsontable(DF, useTypes = TRUE, stretchH = "all")
    })
    
  })
  
  
  #output$boxplotPlot <- renderPlot({
  drawBoxplot <- reactive({
    # if no input file, do not output anything
    if (is.null(input$file_boxplot))
      return(NULL)
    # retrieve data frame from input file
    boxplot2 <- input_boxplot()
    
    # Samples selected by the user form the rhandsontable object
    boxplot_samples_select <- as.data.frame(hot_to_r(input$hot))[,1]
    # Names of samples that can be modified by the user
    boxplot_sample_names <- as.data.frame(hot_to_r(input$hot))[,2]
    # The user can make changes of grouping of samples in the rhansontable object
    boxplot_grouping <- as.data.frame(hot_to_r(input$hot))[,3]
    
    # Select samples, groupings, and changed sample names
    boxplot3 <- boxplot2[,boxplot_samples_select]
    boxplot_grouping1 <- boxplot_grouping[boxplot_samples_select]
    boxplot_sample_names1 <- boxplot_sample_names[boxplot_samples_select]
    colnames(boxplot3) <- boxplot_sample_names1
    
    # Convert to long format to be able to plot all samples independently      
    boxplot_long <- melt(boxplot3)
    colnames(boxplot_long) <- c("sample", "intensity")
    
    # Observe event not working!!! PENDING FIXING
    observeEvent(input$boxplot_action_split, {
      boxplot1b <- boxplot1[,input$boxplot_group_col]
      boxplot_long <- melt(data.frame(boxplot3, boxplot1b))
      # Change column names to more comprehensive ones
      colnames(boxplot_long) <- c("sample", "intensity", "groups")
    })
    
    # Merge group column: PENDING!
    #boxplot_final <- merge(boxplot_long, data.frame(sample=boxplot_sample_names1, groups=boxplot_grouping1), by="sample", all=F)
    boxplot_final <- boxplot_long
    
    ## Box or violin plot? Split by group or not? ##
    
    p_box <- ggplot(boxplot_final, aes(x=sample, y=intensity))
    
    observeEvent(input$boxplot_action_split, {
      p_box <- ggplot(boxplot_final, aes(x=sample, y=intensity, fill=groups))
    })
    if(input$boxORviolin == 1){
      p_box <- p_box + geom_boxplot()
    }else{
      p_box <- p_box + geom_violin()
    }
    
    # if(input$boxORviolin == 1){
    #   if(input$boxplot_action_split){
    #     p_box <- ggplot(boxplot_final, aes(x=sample, y=intensity, fill=groups)) + geom_boxplot()
    #   }else{
    #     p_box <- ggplot(boxplot_final, aes(x=sample, y=intensity)) + geom_boxplot()
    #   }
    # }else if(input$boxORviolin == 2){
    #   if(input$boxplot_action_split){
    #     p_box <- ggplot(boxplot_final, aes(x=sample, y=intensity, fill=groups)) + geom_violin()
    #   }else{
    #     p_box <- ggplot(boxplot_final, aes(x=sample, y=intensity)) + geom_violin()
    #   }
    # }
    # 
    
    # Plot
    p_box +
      theme_bw() +
      ggtitle(input$boxplot_title) +
      xlab(label = "Samples") +
      theme(plot.title = element_text(size = input$boxplot_size_title, face = "bold", hjust=0.5), # size of Boxplot plot title
            axis.title.x = element_text(size=input$boxplot_size_xlab), # size of x-axis label
            axis.title.y = element_text(size=input$boxplot_size_ylab),
            axis.text.x = element_text(angle = 90, hjust = 1, size=input$boxplot_size_textx))
    
    
    # set x limits to allow space for the labels (pending letting users modify it)
    # xlimits <- c(min(df_boxplot$PC1)+0.2*min(df_boxplot$PC1), max(df_boxplot$PC1)+0.4*max(df_boxplot$PC1))
    # 
    # # plot
    # p_boxplot + geom_point(size=3) +
    #   theme_bw() +
    #   xlab(paste0("PC:",input$boxplot_col1," ", round(percentVar[input$boxplot_col1] * 100),"% variance")) +
    #   ylab(paste0("PC:",input$boxplot_col2," ", round(percentVar[input$boxplot_col2] * 100),"% variance")) +
    #   ggtitle(input$boxplot_title) +
    #   geom_text(size=5, hjust=-0.2) +
    #   xlim(xlimits) +
    #   theme(plot.title = element_text(size = input$boxplot_size_title, face = "bold", hjust=0.5), # size of Boxplot plot title
    #         axis.title.x=element_text(size=input$boxplot_size_xlab), # size of x-axis label
    #         axis.title.y=element_text(size=input$boxplot_size_ylab))
    
  })
  
  observeEvent(input$doBoxplot, {
    # If the user clicks on the "Display Vocano" button, draw boxplot plot
    output$boxplotPlot <- renderPlot({
      drawBoxplot()
    })
    # The option to export Boxplot plot appears at the same time as the plot itself
    output$downloadBoxplot <- downloadHandler(
      filename = function(){paste(Sys.Date(), "_Boxplot.png", sep ="")},
      content = function(file){
        png(file) # picking up correct file name in Chrome only...
        p <- drawBoxplot()
        print(p)
        dev.off()
        #ggsave(filename=file, plot=p)
      }
    )
    
    ## Interactive table with rhandsontable ##
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
# runApp(launch.browser=TRUE)

