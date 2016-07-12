library(shiny)
#library(DT)
library(googlesheets)



saveData <- function(BU,data) {

    gs_new(BU, input = data, trim = TRUE)
    
}
#outputDir <- "C:/Users/lshu0/Documents/daily work/7-5-2016 Google Cloud/UI allin1"
#saveData <- function(data) {
  # Create a unique file name
 # fileName <- sprintf("%s.csv", as.integer(Sys.time()))
  # Write the file to the local system
#  write.csv(
 #   x = data,
  #  file = file.path(outputDir, fileName), 
   # row.names = FALSE, quote = TRUE
  #)
#}

options(shiny.maxRequestSize = 20*1024^2)

shinyApp(
  ui= fluidPage(
    titlePanel("Uploading Hierarchy Table"),
    sidebarLayout(
      sidebarPanel(
        textInput('LDAP', label = h5("Please enter your LDAP")),
        selectInput("BU", label = h5("Please Select BU"), 
                    choices = list("Auto" = 'AutoItem', "Home Appliance" = 'HAItem', "Sporting Goods & Toys" = 'SGTItem', 
                                   "Home" = 'HomeItem', "Tools" = 'ToolsItem', "Lawn and Garden" = 'LGItem', "Outdoor Living" = 'ODLItem' ), 
                    selected = 'AutoItem'),
        
        fileInput('file1', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  )
        ),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr(),
        actionButton("submit", "Submit")
      ),
      mainPanel(
        dataTableOutput('contents')
      )
    )
  ), 
  server = function(input, output, session) {
    observeEvent(input$submit,
                 {
                   inFile <- input$file1
                   
                   if (is.null(inFile))
                     return(NULL)
                   
#                   gap <- gs_title(input$BU)
#                   original_data <- gap %>% gs_read( ws = 'Sheet1')
                   
                   new_data = read.csv(inFile$datapath, header = input$header,
                            sep = input$sep, quote = input$quote)
                   new_data$creater <- rep(input$LDAP,nrow(new_data))
                   new_data$creat_ts <- rep(Sys.time(),nrow(new_data))
                   
#                   stack_data <- rbind(original_data, new_data)
#                   duplicates <- stack_data[duplicated(stack_data[c('Div','Item')], incomparables = FALSE),]
                   
 #                  validate(
  #                   need(dim(duplicates)[1]==0, "Please select a data set")
   #                )

                   saveData( input$BU,new_data)
                   
                 })
    
    output$contents <- renderDataTable({
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      data <- read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
      data$creater <- rep(input$LDAP,nrow(data))
      data$creat_ts <- rep(Sys.time(),nrow(data))
      data
    })
  }
  
)