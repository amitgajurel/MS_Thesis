pkg_gui = c("shiny","shinydashboard")
#install.packages(pkg_gui)
lapply(pkg_gui,library, character.only = TRUE)


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
  
  plotOutput(outputId = "hist") # only creates the space to put in the ouput; how to build that is defined in the server function 
)

# sets up sever object i.e. to assemble inputs into outputs

## Rules for server function
## 1. Save the output object to output$
## 2. Build objects to display with render*()
## 3. Use of reactive objects with input values



server = function(input,output){  # input output arguments are like list
  # output$hist = #code   # save to hist element of output list # this gets placed in the plotOutput
  
    output$hist = renderPlot({hist(rnorm(input$num))}) # code block ; using the input values
}

# knit the user interface and server object
shinyApp(ui = ui, server = server)
