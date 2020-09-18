library(shiny)

source('global.R')

# server   
server <- function(input, output, session) {
  
  # global variable, what type of plot interaction
  interaction_type <- "click"
  
  # renderUI  
  output$Field <- renderUI({
    if (input$radio != "evolution") {
      selectInput('field', 
                  label = "Select ESI Field",
                  choices = fieldnames,
                  selected = "Agricultural Sciences")}
  })
  
  output$Issue <- renderUI({
    if (input$radio == "compare") {
      selectInput('issue', 
                  label = "Select Comparison Issue",
                  choices = issuelist,
                  selected = "202007")}
  })
  
  output$Topic <- renderUI({
    if (input$radio == "evolution") {
      textInput('topic',
                label = "Input Topic",
                value = "perovskite solar cells")}
  })
  
  #output$Range1 <- renderUI({
  #  if (input$radio == "cloud") {
  #    sliderInput("freq",
  #                "Minimum Frequency:",
  #                min = 1,  max = 100, value = 10)
  #  }
  #}) 
  
  #output$Range2 <- renderUI({
  #  if (input$radio == "cloud") {
  #    sliderInput("max",
  #                "Maximum Number of Words:",
  #                min = 1,  max = 100,  value = 50)
  #  }
  #})
  
  output$Range3 <- renderUI({
    actionButton("update", "Change")
  })                
  
  # observe for user interaction and change the global interaction_type variable
  observeEvent(input$user_click, interaction_type <<- "click")
  observeEvent(input$user_brush, interaction_type <<- "brush")
  
  # generate dataset
  dataset <- reactive({
    input$update  
    isolate({
      withProgress({setProgress(message = "Processing ...")
        dset <- switch(input$radio,
                       statistics = Data_statistics,
                       similarity = simiIndex,
                       simi_grade = simiGrade, 
                       compare    = compare_fronts(field = input$field, issue = input$issue),
                       evolution  = evolute_fronts(topic = input$topic),
                       cloud      = wordcloud_fronts(field = input$field)
        )      	
      })
    })      
    return(dset)
  })
  
  # generate the subdata to put in the table
  dat <- reactive({
    user_brush <- input$user_brush
    user_click <- input$user_click
    
    if(interaction_type == "brush") 
      res <- brushedPoints(dataset(), user_brush)
    if(interaction_type == "click") 
      res <- nearPoints(dataset(), user_click, threshold = 10, maxpoints = 1)
    
    return(res)
  })
  
  # Output the plot
  output$plot <- renderPlot({
    switch(input$radio,
           statistics = statisticsPlot(), 
           similarity = similarityPlot(field = input$field), 
           simi_grade = simigradePlot(field = input$field), 
           compare    = comparePlot(dataset()),
           evolution  = evolutionPlot(dataset()),
           cloud      = cloudPlot(dataset(), repeatable(wordcloud), input$freq, input$max)           
    )
  })
  
  # Output table
  output$tab <- DT::renderDataTable(DT::datatable(dataset()))
  output$table <- DT::renderDataTable(DT::datatable(dat()))
  
  # Output text                                                           
  output$selected_radio <- renderText({ 
    paste("You have selected field is : ", input$radio)
  })
  
  output$selected_field <- renderText({ 
    paste("You have selected field is : ", input$field)
  })
  
  output$selected_issue <- renderText({ 
    paste("You have selected issue is: ", input$issue)
  })
  
  output$selected_topic <- renderText({ 
    paste("You have selected topic is: ", input$topic)
  })
  
  #output$selected_freq <- renderText({ 
  #  paste("The Top Number is: ", input$freq)
  #})    
  
  #output$selected_max <- renderText({ 
  #  paste("The Maximum Number of Words is: ", input$max)
  #})          
}

# ui
ui <- fluidPage(
  
  # title  
  # titlePanel("Data Analysis and Visualization for ESI Research Fronts"),  
  tags$title("ESI Research Fronts"),
  div(tags$header(
    p("Data Analysis and Visualization for ESI Research Fronts", style = "font-size:32px"),
    p("author : Ruienzi", style = "font-size:20px")),
    align = "center", 
    style = "color:#ffffff; background-color: #4d728d") ,
  
  sidebarLayout(
    position = c("right"), 
    fluid = TRUE, 
    sidebarPanel(width = 4, 
                 radioButtons("radio", 
                              label = "Function",
                              choices = list(
                                "statistics"       = "statistics",
                                "Jac similarity"   = "similarity",
                                "similarity grade" = "simi_grade",
                                "compare"          = "compare",
                                "theme evolution"  = "evolution"),
                              # "words cloud"      = "cloud"), 
                              selected = "statistics"),
                 
                 uiOutput('Field'),
                 uiOutput('Issue'),
                 uiOutput('Topic'),
                 #uiOutput('Range1'),
                 #uiOutput('Range2'),
                 uiOutput('Range3')
                 # submitButton("Submit")           
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Note",
                 textOutput("selected_radio")),
        tabPanel("Table",
                 DT::dataTableOutput("tab")),
        tabPanel("Plot",
                 plotOutput("plot", click = "user_click", brush = "user_brush"))
      )
    )      
  ),
  
  # textOutput("selected_field"),
  # textOutput("selected_issue"),
  # textOutput("selected_topic"),
  # textOutput("selected_freq"),
  # textOutput("selected_max"),    
  
  DT::dataTableOutput("table"),
  
  tags$footer(p("contact : ruienzi@qq.com"), align = "left"),      
)

shinyApp(ui = ui, server = server)