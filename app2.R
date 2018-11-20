### Shiny app for building Boxplot plots
# Author: Sarah Bonnin - sarah.bonnin@crg.eu

library(shiny)
library(ggplot2)
library(colourpicker)
library(shinyjs)
library(shinyWidgets) # for downloadBttn
library(DT)
library(rhandsontable) # for interactive table
library(data.table)
library(shinyBS)

options(shiny.maxRequestSize=100*1024^2) # sets the max file to load to 100Mb

# Define UI for application that draws a Boxplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Box and Violing plots"),
  
  # Sidebar with options
  sidebarLayout(
    sidebarPanel(
      # input file: accepts tab-separated text files
      fileInput("file_boxplot", "Upload expression matrix", accept = c("text/txt", "text/csv", "text/tab")),
      # Choose whether to plot a box or a violon plot (default is a box plot)
      radioButtons("boxORviolin", label = h4("Type of plot:"),
                   choices = list("Box plot" = 1, "Violin plot" = 2), 
                   selected = 1, inline = TRUE),

      
      # also the potential grouping and naming columns from the sample table appear
      uiOutput("sample_grouping_factor"),
      uiOutput("sample_point_labels"),
      uiOutput("sample_point_shapes"),
      #      uiOutput("boxplot_samples_check"),
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
      )
    ,
    # Panel for the Boxplot plot itself + an additional tab to select the samples
    mainPanel(
      uiOutput("allsamples")
    )
  )
)



# Define server logic required to draw a Boxplot plot
server <- function(input, output, session) {
  
  input_boxplot <- reactive({
    # Reactive function that detects and reads in the input file
    inFile <- input$file_boxplot
    if (is.null(inFile))
      return(NULL)
    #boxplot1 <- read.table(inFile$datapath, sep="\t", header=T, as.is = T)
    boxplot1 <- fread(inFile$datapath, sep="\t", header=T)
    # select numeric columns only (others are supposed to be annotation etc.)
    dplyr::select_if(boxplot1, is.numeric)
  })
  # enable button that will draw the Boxplot 
  shinyjs::enable("doBoxplot")
  
  
  observe({
    if(!is.null(input$file_boxplot)){
      
      output$boxplot_columns1 <- renderUI({
        numericInput("boxplot_col1", 
                     label = h6("Principal component on x-axis:"), 
                     value = 1,
                     min=1, 
                     max=10, 
                     step=1)
      })
      
      output$boxplot_columns2 <- renderUI({
        numericInput("boxplot_col2", 
                     label = h6("Principal component on y-axis:"), 
                     value = 2,
                     min=1,
                     max=10,
                     step=1)
      })
      
      output$allsamples <- renderUI({
        tabsetPanel(
          tabPanel("Plot",
                   # Action button that will trigger the plot to be drawn after the parameters are set
                   actionBttn("doBoxplot", "Display Boxplot plot", icon("bar-chart-o"), style = "jelly", size = "sm"),
                   # Plot Boxplot
                   plotOutput("boxplotPlot"),
                   # Conditional panel: if the user clicked on "display Boxplot plot", the download button appears
                   # ! Working properly only on Google Chrome!!
                   conditionalPanel(
                     condition = "input.doBoxplot > 0",
                     fluidRow(column(3,
                                     textInput("name_png", 
                                               label = h6("Enter file name:"), 
                                               value = "")),
                              column(3, numericInput("width_png", 
                                                     label = h6("PNG file width:"), 
                                                     value = 700, 
                                                     min=480, 
                                                     max=4000, 
                                                     step=20)),
                              column(3, numericInput("height_png", 
                                                     label = h6("PNG file height:"), 
                                                     value = 500, 
                                                     min=480, 
                                                     max=4000, 
                                                     step=20)),
                              column(3, downloadBttn("downloadBoxplot", 
                                                     "Save Boxplot plot", 
                                                     size = "sm", 
                                                     style = "jelly"))
                     )
                   )
          ),
          tabPanel("Selected samples", 
                   checkboxGroupInput("sampleschecks", 
                                      label=h6("Samples selected: unselect and refresh to update the plot"), 
                                      choices=colnames(input_boxplot()), 
                                      selected=colnames(input_boxplot()))
          )
        )
      })
      
    }
  }) # end of observe
  
  
  #output$boxplotPlot <- renderPlot({
  drawBoxplot <- reactive({
    # if no input file, do not output anything
    if (is.null(input$file_boxplot))
      return(NULL)
    #input$doBoxplot
    # retrieve data frame from input file
    boxplot2 <- input_boxplot()
    # Select samples, groupings, and changed sample names
    boxplot_samples_select <- input$sampleschecks
    boxplot_grouping <- input$sampleschecks
    boxplot_sample_names <- input$sampleschecks
    boxplot3 <- boxplot2[,..boxplot_samples_select]
    
    boxplot_sample_names1 <- boxplot_sample_names[boxplot_samples_select]
    colnames(boxplot3) <- boxplot_sample_names1
    
    # Performs principal component analysis on data
    boxplot <- prcomp(t(boxplot3))
    # Retrieve the percentages of variation for each component
    percentVar <- boxplot$sdev^2 / sum( boxplot$sdev^2 )
    
    # Create data frame with principal components (elected or default) and default sample names.
    df_boxplot0 <- data.frame(PC1=boxplot$x[,input$boxplot_col1], 
                          PC2=boxplot$x[,input$boxplot_col2], 
                          sample=colnames(boxplot3))
    
    # if there is a sample table file, join with expression file and then choose:
    # 1. grouping by color 
    # 2. labels 
    # 3. point shape  
    if(!is.null(input$sample_table)){
      # extract selected samples from sample table
      boxplot_grouping_colors <- input_sampletable()[input_sampletable()[,1] %in% input$sampleschecks, ]
      # merge data frame with sample table
      df_boxplot <- merge(df_boxplot0, boxplot_grouping_colors, by.x="sample", by.y=1, all=T)
      
      # if no point label is specified, take sample names are labels
      if(input$point_labels != "no_label_by"){
        # remove "names" column to replace it by user chosen column for labels
        df_boxplot <- df_boxplot[,-grep("^sample$", colnames(df_boxplot))]
        colnames(df_boxplot)[grep(paste0("^",input$point_labels,"$"), colnames(df_boxplot))] <- "name_points"
      }else{
        colnames(df_boxplot)[grep("^sample$", colnames(df_boxplot))] <- "name_points"
      }
      # depending on what parameters where set, build ggplot base layer
      if(input$point_shapes == "no_shape_by" & input$group_color == "no_colour_by"){
        p_boxplot <- ggplot(data=df_boxplot, aes_string(x="PC1", y="PC2", label="name_points"))
      }else if(input$point_shapes != "no_shape_by" & input$group_color == "no_colour_by"){
        p_boxplot <- ggplot(data=df_boxplot, aes_string(x="PC1", y="PC2", label="name_points", shape=input$point_shapes))
      }else if(input$point_shapes == "no_shape_by" & input$group_color != "no_colour_by"){
        p_boxplot <- ggplot(data=df_boxplot, aes_string(x="PC1", y="PC2", label="name_points", color=input$group_color))
      }else if(input$point_shapes != "no_shape_by" & input$group_color != "no_colour_by"){
        p_boxplot <- ggplot(data=df_boxplot, aes_string(x="PC1", y="PC2", label="name_points", color=input$group_color, shape=input$point_shapes))
      }
    }else{ # if there is no sample table at all
      df_boxplot <- df_boxplot0
      p_boxplot <- ggplot(data=df_boxplot, aes(x=PC1, y=PC2, label=sample))
    }
    # set x limits to allow space for the labels (pending letting users modify it)
    xlimits <- c(min(df_boxplot$PC1)*1.2, max(df_boxplot$PC1)*2.5)
    
    # plot Boxplot
    p_boxplot1 <- p_boxplot + geom_point(size=3) +
      theme_bw() +
      xlab(paste0("PC:",input$boxplot_col1," ", round(percentVar[input$boxplot_col1] * 100),"% variance")) +
      ylab(paste0("PC:",input$boxplot_col2," ", round(percentVar[input$boxplot_col2] * 100),"% variance")) +
      ggtitle(input$boxplot_title) +
      geom_text(size=input$label_size, hjust=-0.1) +
      xlim(xlimits) +
      theme(plot.title = element_text(size = input$boxplot_size_title, face = "bold", hjust=0.5), # size of Boxplot plot title
            axis.title.x=element_text(size=input$boxplot_size_xlab), # size of x-axis label
            axis.title.y=element_text(size=input$boxplot_size_ylab)
            #            legend.position="none"
      )
    # if point shapes are assigned, add more point shape options
    if(!is.null(input$sample_table)){
      if(input$point_shapes != "no_shape_by"){
        p_boxplot1 + scale_shape_manual(values=1:length(unique(df_boxplot[,input$point_shapes])))
      }else{
        p_boxplot1
      }
    }else{
      p_boxplot1
    }
    
  })
  
  
  observeEvent(input$doBoxplot, {
    
    # If the user clicks on the "Display Vocano" button, draw boxplot plot
    output$boxplotPlot <- renderPlot({
      # isolate so that the plot is not updated until the "Refresh plot" button is pressed again...
      isolate({drawBoxplot()})
    })
    
    # The option to export Boxplot plot appears at the same time as the plot itself
    output$downloadBoxplot <- downloadHandler(
      filename = function(){
        paste0(Sys.Date(), "_", gsub(" ", "_", input$name_png), "_Boxplot.png")
      },
      content = function(file){
        png(file, width=input$width_png, height=input$height_png) # picking up correct file name in Chrome only...
        p <- drawBoxplot()
        print(p)
        dev.off()
      }
    )
    # Update "doBoxplot" button label
    updateActionButton(session, "doBoxplot",
                       label = "Refresh plot", 
                       icon("bar-chart-o"))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
# runApp(launch.browser=TRUE)

