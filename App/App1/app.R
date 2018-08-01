pkg_gui = c("shiny","shinydashboard")
#install.packages(pkg_gui)
#lapply(pkg_gui,library, character.only = TRUE)
library(shiny)
library(shinydashboard)
library(rsconnect)

## start with the same template
## add elements as arguement to fluidPage()
## Create reactive inputs with an *Input() function
## Create reactive results with an *Output() function


# sets up UI object that is the user interface 
ui = fluidPage(
  # *Input functions to get input from the user there are a lot in the library
  # *Output functions to get output to the user like plots
  sliderInput(inputId = "num",label = "Choose a number",
              value = 25, min = 0, max = 100),
  actionButton(inputId = "normal", label = "Normal"),
  actionButton(inputId = "uniform", label = "Uniform"),
  
  plotOutput(outputId = "hist"), # only creates the space to put in the ouput; how to build that is defined in the server function 
  verbatimTextOutput("stats")
)

# sets up sever object i.e. to assemble inputs into outputs

## Rules for server function
## 1. Save the output object to output$
## 2. Build objects to display with render*()
## 3. Use of reactive objects with input values


server = function(input,output){  # input output arguments are like list
  # output$hist = #code   # save to hist element of output list # this gets placed in the plotOutput
  
  #data = reactive({rnorm(input$num)})
  
  rv = reactiveValues(data = rnorm(100))
  
  observeEvent(input$normal,{rv$data = rnorm(100)})
  observeEvent(input$uniform,{rv$data = runif(100)})
  
  
  # data = eventReactive(input$normal, {
  #   rnorm(input$num)
  # })
  # 
  # data = eventReactive(input$uniform, {
  #   runif(input$num)
  # })

  output$hist = renderPlot({hist(rv$data)}) # code block ; using the input values
  output$stats = renderPrint({summary(rv$data)})
}

# knit the user interface and server object
shinyApp(ui = ui, server = server)





